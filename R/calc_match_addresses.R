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
    dplyr::rename_with(
      .fn = ~ primary_postcode_col,
      .cols = !!dplyr::sym(lookup_postcode_col)
    )

  # First step is to do the exact matches. We mock join address column here so
  # that we can do the join easily.
  exact_match_df <-
    dplyr::inner_join(
      x = primary_df %>%
        # Mock a join address column on the primary dataframe
        dplyr::mutate(JOIN_ADDRESS = !!dplyr::sym(primary_address_col)),
      y = lookup_df %>%
        # Mock a join address column on the lookup dataframe
        dplyr::mutate(JOIN_ADDRESS = !!dplyr::sym(lookup_address_col)),
      by = c(primary_postcode_col, "JOIN_ADDRESS"),
      suffix = c("_PRIMARY", "_LOOKUP"),
      copy = TRUE
    ) %>%
    dplyr::select(-JOIN_ADDRESS)

  # Now get the rows that haven't already been matched
  non_exact_match_df <- primary_df %>%
    dplyr::anti_join(exact_match_df)

  # Get the intersection of postcodes in both datasets
  postcodes_in_both_df <-
    dplyr::inner_join(
      x = non_exact_match_df %>%
        dplyr::distinct(!!dplyr::sym(primary_postcode_col)),
      y = lookup_df %>%
        dplyr::distinct(!!dplyr::sym(primary_postcode_col)),
      copy = TRUE
    )

  # Filter lookup dataset to postcodes in both datasets
  lookup_df <- lookup_df %>%
    dplyr::inner_join(
      y = postcodes_in_both_df,
      copy = TRUE
    )

  # Filter non exact matches to postcodes in both datasets
  non_exact_match_df <- non_exact_match_df %>%
    dplyr::inner_join(
      y = postcodes_in_both_df,
      copy = TRUE
    )

  # Filter lookup dataset to postcodes in non exact matches
  lookup_df <- lookup_df %>%
    dplyr::inner_join(
      y = non_exact_match_df %>%
        dplyr::distinct(!!dplyr::sym(primary_postcode_col)),
      copy = TRUE
    )

  # Tokenise non exact match addresses
  non_exact_match_df <- non_exact_match_df %>%
    nhsbsaR::oracle_unnest_tokens(col = primary_address_col, drop = FALSE) %>%
    dplyr::select(-TOKEN_NUMBER) %>%
    dplyr::distinct() %>%
    dplyr::mutate(TOKEN_TYPE = ifelse(REGEXP_LIKE(TOKEN, "[0-9]"), "D", "C"))

  # Add the theoretical max score for each non exact match address
  non_exact_match_df <- non_exact_match_df %>%
    dplyr::group_by(-c(TOKEN, TOKEN_TYPE)) %>%
    dplyr::mutate(MAX_SCORE = sum(ifelse(TOKEN_TYPE == "D", 4, 1))) %>%
    dplyr::ungroup()

  # Tokenise lookup addresses
  lookup_df <- lookup_df %>%
    nhsbsaR::oracle_unnest_tokens(col = lookup_address_col, drop = FALSE) %>%
    dplyr::select(-TOKEN_NUMBER) %>%
    dplyr::distinct() %>%
    dplyr::mutate(TOKEN_TYPE = ifelse(REGEXP_LIKE(TOKEN, "[0-9]"), "D", "C"))

  # We want to minimise the amount of jarrow winkler calculations we do. So first
  # do the exact token level matches
  non_exact_match_exact_match_df <- non_exact_match_df %>%
    dplyr::inner_join(
      y = lookup_df,
      by = c(primary_postcode_col, "TOKEN_TYPE", "TOKEN"),
      suffix = c("_PRIMARY", "_LOOKUP"),
      copy = TRUE
    ) %>%
    dplyr::rename(TOKEN_PRIMARY = TOKEN) %>%
    dplyr::mutate(TOKEN_LOOKUP = TOKEN_PRIMARY)

  # Now get the remaining candidates to consider for jarrow winkler matching
  # (character token types that aren't an exact match)
  non_exact_match_jw_match_df <- non_exact_match_df %>%
    dplyr::inner_join(
      y = lookup_df,
      by = c(primary_postcode_col, "TOKEN_TYPE"),
      suffix = c("_PRIMARY", "_LOOKUP"),
      copy = TRUE
    ) %>%
    dplyr::filter(
      TOKEN_TYPE == "C",
      TOKEN_PRIMARY != TOKEN_LOOKUP
    )

  # We can also apply some other filters
  non_exact_match_jw_match_df <- non_exact_match_jw_match_df %>%
    dplyr::filter(
      # Tokens share the same first letter
      substr(TOKEN_LOOKUP, 1, 1) == substr(TOKEN_PRIMARY, 1, 1) |
        # Tokens share same second letter
        substr(TOKEN_LOOKUP, 2, 1) == substr(TOKEN_PRIMARY, 2, 1) |
        # Tokens share same last letter
        substr(TOKEN_LOOKUP, nchar(LOOKUP_ADDRESS), 1) == substr(TOKEN_PRIMARY, nchar(TOKEN_PRIMARY), 1) |
        # One token is a substring of the other
        INSTR(TOKEN_LOOKUP, TOKEN_PRIMARY) > 1 |
        INSTR(TOKEN_PRIMARY, TOKEN_LOOKUP) > 1
    )

  # Now calculate the jarrow winkler scores
  non_exact_match_jw_match_df <- non_exact_match_jw_match_df %>%
    dplyr::mutate(SCORE = UTL_MATCH.JARO_WINKLER(TOKEN_PRIMARY, TOKEN_LOOKUP))

  # And filter to scores above 0.8
  non_exact_match_jw_match_df <- non_exact_match_jw_match_df %>%
    dplyr::filter(SCORE > 0.8)

  # Now stack the non exact exact and jarrow winkler matches back together
  non_exact_match_df <- dplyr::union_all(
    x = non_exact_match_exact_match_df %>%
      dplyr::mutate(SCORE = 1),
    y = non_exact_match_jw_match_df
  )

  # If the token is a digit then multiply the score by 4
  non_exact_match_df <- non_exact_match_df %>%
    dplyr::mutate(SCORE = ifelse(TOKEN_TYPE == "D", SCORE * 4, SCORE))

  # Get the max score for each primary token in the primary address from each
  # lookup address
  non_exact_match_df <- non_exact_match_df %>%
    dplyr::group_by(dplyr::across(-c(TOKEN_LOOKUP, SCORE))) %>%
    dplyr::summarise(SCORE = max(SCORE)) %>%
    dplyr::ungroup()

  # Sum the score for each single line address combination
  non_exact_match_df <- non_exact_match_df %>%
    dplyr::group_by(dplyr::across(-c(TOKEN_PRIMARY, TOKEN_TYPE, SCORE))) %>%
    dplyr::summarise(SCORE = sum(SCORE)) %>%
    dplyr::ungroup()

  # Normalise the score
  non_exact_match_df <- non_exact_match_df %>%
    dplyr::mutate(SCORE = SCORE / MAX_SCORE) %>%
    dplyr::select(-MAX_SCORE)

  # Take the top scoring lookup address for each primary address (if there are
  # draws then keep all of them)
  non_exact_match_df <- non_exact_match_df %>%
    dplyr::group_by(
      dplyr::across(
        dplyr::any_of(
          c(
            "POSTCODE",
            primary_address_col,
            paste0(primary_address_col, "_PRIMARY")
          )
        )
      )
    ) %>%
    dplyr::slice_max(order_by = SCORE) %>%
    dplyr::ungroup()

  # Stack the exact and non exact matches together and output
  dplyr::union_all(
    x = exact_match_df %>%
      dplyr::mutate(SCORE = 1, MATCH_TYPE = "EXACT"),
    y = non_exact_match_df %>%
      dplyr::mutate(MATCH_TYPE = "NON-EXACT")
  )

}
