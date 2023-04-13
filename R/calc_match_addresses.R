#' Match two sets of addresses
#'
#' Match a distinct dataframe of primary addresses to a distinct dataframe of
#' lookup addresses.
#'
#' Returns a dataframe of exact matches (postcode and single line address) and
#' non exact matches (postcode and fuzzy single line address). For non exact
#' matches it will retain draws.
#'
#' @param primary_df Dataframe of distinct primary addresses
#' @param primary_postcode_col Column containing the primary postcode
#' @param primary_address_col Column containing the primary single line address
#' @param lookup_df Dataframe of distinct lookup addresses
#' @param lookup_postcode_col Column containing the lookup postcode
#' @param lookup_address_col Column containing the lookup single line address
#'
#' @importFrom rlang ':='
#'
#' @examples
#' @export
calc_match_addresses <- function(
  primary_df,
  primary_postcode_col,
  primary_address_col,
  lookup_df,
  lookup_postcode_col,
  lookup_address_col
) {

  # Rename the lookup postcode column name to be the same as the primary
  # postcode column name
  lookup_df <- lookup_df %>%
    dplyr::rename("{primary_postcode_col}" := .data[[lookup_postcode_col]])

  # First step is to do the exact matches. We mock join address column here so
  # that we can do the join easily.
  exact_match_df <-
    dplyr::inner_join(
      x = primary_df %>%
        # Mock a join address column on the primary dataframe
        dplyr::mutate(JOIN_ADDRESS = .data[[primary_address_col]]),
      y = lookup_df %>%
        # Mock a join address column on the lookup dataframe
        dplyr::mutate(JOIN_ADDRESS = .data[[lookup_address_col]]),
      by = c(primary_postcode_col, "JOIN_ADDRESS"),
      suffix = c("", "_LOOKUP"),
      copy = TRUE
    ) %>%
    dplyr::select(-.data$JOIN_ADDRESS)

  # Now get the rows that haven't already been matched
  non_exact_match_df <- primary_df %>%
    dplyr::anti_join(
      y = exact_match_df,
      na_matches = "na",
      copy = TRUE
    )

  # Filter non exact matches to postcodes in the lookup
  non_exact_match_df <- non_exact_match_df %>%
    dplyr::semi_join(
      y = lookup_df %>%
        dplyr::select(.data[[primary_postcode_col]]),
      copy = TRUE
    )

  # Tokenise non exact match addresses
  non_exact_match_df <- non_exact_match_df %>%
    nhsbsaR::oracle_unnest_tokens(col = primary_address_col, drop = FALSE) %>%
    dplyr::mutate(TOKEN_WEIGHT = ifelse(REGEXP_LIKE(.data$TOKEN, "[0-9]"), 4, 1))

  # Add the theoretical max score for each non exact match address
  non_exact_match_df <- non_exact_match_df %>%
    dplyr::group_by(dplyr::across(-c(.data$TOKEN_NUMBER, .data$TOKEN, .data$TOKEN_WEIGHT))) %>%
    dplyr::mutate(MAX_SCORE = sum(.data$TOKEN_WEIGHT, na.rm = TRUE)) %>%
    dplyr::ungroup()

  # Tokenise lookup addresses
  lookup_df <- lookup_df %>%
    nhsbsaR::oracle_unnest_tokens(col = lookup_address_col, drop = FALSE) %>%
    dplyr::select(-.data$TOKEN_NUMBER) %>%
    dplyr::distinct() %>%
    dplyr::mutate(TOKEN_WEIGHT = ifelse(REGEXP_LIKE(.data$TOKEN, "[0-9]"), 4, 1))

  # We want to minimise the amount of Jaro–Winkler calculations we do. So first
  # do the exact token level matches
  non_exact_match_exact_match_df <- non_exact_match_df %>%
    dplyr::inner_join(
      y = lookup_df,
      by = c(primary_postcode_col, "TOKEN_WEIGHT", "TOKEN"),
      suffix = c("", "_LOOKUP"),
      copy = TRUE
    ) %>%
    dplyr::mutate(TOKEN_LOOKUP = .data$TOKEN)

  # Now get the remaining candidates to consider for Jaro–Winkler matching
  # (character token types that aren't an exact match)
  non_exact_match_jw_match_df <- non_exact_match_df %>%
    dplyr::inner_join(
      y = lookup_df,
      by = c(primary_postcode_col, "TOKEN_WEIGHT"),
      suffix = c("", "_LOOKUP"),
      copy = TRUE
    ) %>%
    dplyr::filter(
      .data$TOKEN_WEIGHT == 1,
      .data$TOKEN != .data$TOKEN_LOOKUP
    )

  # We can also apply some other filters
  non_exact_match_jw_match_df <- non_exact_match_jw_match_df %>%
    dplyr::filter(
      # Tokens share the same first letter
      SUBSTR(.data$TOKEN_LOOKUP, 1, 1) == SUBSTR(.data$TOKEN, 1, 1) |
        # Tokens share same second letter
        SUBSTR(.data$TOKEN_LOOKUP, 2, 1) == SUBSTR(.data$TOKEN, 2, 1) |
        # Tokens share same last letter
        SUBSTR(.data$TOKEN_LOOKUP, LENGTH(.data$TOKEN_LOOKUP), 1) %in% c(
          SUBSTR(.data$TOKEN, LENGTH(.data$TOKEN), 1),
          # One token is a substring of the other
          INSTR(.data$TOKEN_LOOKUP, .data$TOKEN) > 1,
          INSTR(.data$TOKEN, .data$TOKEN_LOOKUP) > 1
        )
    )

  # Now calculate the jarrow winkler scores
  non_exact_match_jw_match_df <- non_exact_match_jw_match_df %>%
    dplyr::mutate(SCORE = UTL_MATCH.JARO_WINKLER(.data$TOKEN, .data$TOKEN_LOOKUP))

  # And filter to scores above 0.8
  non_exact_match_jw_match_df <- non_exact_match_jw_match_df %>%
    dplyr::filter(.data$SCORE > 0.8)

  # Now stack the non exact exact and Jaro–Winkler matches back together
  non_exact_match_df <- dplyr::union_all(
    x = non_exact_match_exact_match_df %>%
      dplyr::mutate(SCORE = 1),
    y = non_exact_match_jw_match_df
  )

  # Multiply the score by the token weight
  non_exact_match_df <- non_exact_match_df %>%
    dplyr::mutate(SCORE = .data$SCORE * .data$TOKEN_WEIGHT)

  # Get the max score for each primary token in the primary address from each
  # lookup address
  non_exact_match_df <- non_exact_match_df %>%
    dplyr::group_by(dplyr::across(-c(.data$TOKEN_LOOKUP, .data$SCORE))) %>%
    dplyr::summarise(SCORE = max(.data$SCORE, na.rm = TRUE)) %>%
    dplyr::ungroup()

  # Sum the score for each single line address combination
  non_exact_match_df <- non_exact_match_df %>%
    dplyr::group_by(
      dplyr::across(-c(.data$TOKEN_NUMBER, .data$TOKEN, .data$TOKEN_WEIGHT, .data$SCORE))
    ) %>%
    dplyr::summarise(SCORE = sum(.data$SCORE, na.rm = TRUE)) %>%
    dplyr::ungroup()

  # Normalise the score
  non_exact_match_df <- non_exact_match_df %>%
    dplyr::mutate(SCORE = .data$SCORE / .data$MAX_SCORE) %>%
    dplyr::select(-.data$MAX_SCORE)

  # Take the top scoring lookup address for each primary address (if there are
  # draws then keep all of them)
  non_exact_match_df <- non_exact_match_df %>%
    dplyr::group_by(
      .data[[primary_postcode_col]], .data[[primary_address_col]]
    ) %>%
    dplyr::slice_max(order_by = .data$SCORE) %>%
    dplyr::ungroup()

  # Stack the exact and non exact matches together and output
  dplyr::union_all(
    x = exact_match_df %>%
      dplyr::mutate(SCORE = 1, MATCH_TYPE = "EXACT"),
    y = non_exact_match_df %>%
      dplyr::mutate(MATCH_TYPE = "NON-EXACT")
  )

}
