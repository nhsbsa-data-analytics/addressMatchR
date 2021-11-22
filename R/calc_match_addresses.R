#' Calculate best match addresses
#'
#' Match one set of addresses to another.
#'
#' @param main_df Main set of addresses
#' @param main_postcode_col
#' @param main_address_col
#' @param secondary_df Secondary set of addresses
#' @param secondary_postcode_col
#' @param secondary_address_col
#'
#' @examples
#' @export
calc_match_addresses <- function(
 main_df,
 main_postcode_col,
 main_address_col,
 secondary_df,
 secondary_postcode_col,
 secondary_address_col
) {

  # First step is to do the exact matches
  df <- main_df %>%
    dplyr::left_join(
      y = secondary_df %>%
        dplyr::mutate(
          SCORE = 1,
          MATCH_TYPE = "EXACT"
        ),
      by = c(
        main_postcode_col = secondary_postcode_col,
        main_address_col = secondary_address_col
        ),
      copy = TRUE
    )

  # Now get the rows that haven't already been matched
  non_exact_match_df <- df %>%
    dplyr::filter(is.na(MATCH_TYPE))

  # Filter non exact matches by postcodes in the secondary dataset
  non_exact_match_df <- non_exact_match_df %>%
    dplyr::inner_join(
      y = secondary_df %>%
        dplyr::distinct(!!sym(secondary_postcode_col)),
      by = c(main_postcode_col = secondary_postcode_col)
    )

  # Filter secondary dataset to postcodes in non exact matches
  secondary_df <- secondary_df %>%
    dplyr::inner_join(
      y = non_exact_match_df %>%
        dplyr::distinct(!!sym(main_postcode_col)),
      by = c(secondary_postcode_col = main_postcode_col)
    )

  # Tokenise non exact match addresses
  non_exact_match_df <- non_exact_match_df
  nhsbsaR::oracle_unnest_tokens(col = main_address_col, drop = FALSE) %>%
    dplyr::mutate(
      TOKEN_TYPE = ifelse(REGEXP_LIKE(TOKEN, "[0-9]"), "D", "C")
    ) %>%
    dplyr::rename(
      MAIN_ADDRESS = !!sym(main_address_col),
      MAIN_TOKEN = TOKEN
    )

  # Tokenise secondary addresses
  secondary_df <- secondary_df
  nhsbsaR::oracle_unnest_tokens(col = secondary_address_col, drop = FALSE) %>%
    dplyr::mutate(
      TOKEN_TYPE = ifelse(REGEXP_LIKE(TOKEN, "[0-9]"), "D", "C")
    ) %>%
    dplyr::rename(
      SECONDARY_ADDRESS = !!sym(secondary_address_col),
      SECONDARY_TOKEN = TOKEN
    )

  # Join the two together (keeping blank rows as NA and joining on token type
  # as we aren't comparing digits to characters)
  non_exact_match_df <- non_exact_match_df %>%
    dplyr::left_join(
      y = secondary_df,
      by = c(
        secondary_postcode_col = main_postcode_col,
        TOKEN_TYPE
      ),
      copy = TRUE
    )

  # Remove addresses that don't have at least one postcode and token type match
  # (at least one non null row)
  non_exact_match_df <- non_exact_match_df %>%
    dplyr::mutate(
      NON_NULL_ROW = ifelse(
        test = is.na(!!sym(secondary_address_col)),
        yes = 0,
        no = 1
      )
    ) %>%
    dplyr::group_by(!!sym(main_postcode_col), !!sym(main_address_col)) %>%
    dplyr::mutate(SUM_NON_NULL_ROW = sum(NON_NULL_ROW)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(SUM_NON_NULL_ROW > 0) %>%
    dplyr::select(-SUM_NON_NULL_ROW)


  # Calculate the similarity score for each token combination (where the address
  # base token is null, JW returns 0)
  non_exact_match_df <- non_exact_match_df %>%
    dplyr::mutate(
      SCORE = dplyr::case_when(
        MAIN_TOKEN == SECONDARY_TOKEN ~ 1, # exact matches
        TRUE ~ UTL_MATCH.JARO_WINKLER(MAIN_TOKEN, SECONDARY_TOKEN)
      )
    )

  # If the score is under 0.8 then set it to 0
  non_exact_match_df <- non_exact_match_df %>%
    dplyr::mutate(SCORE = ifelse(SCORE < 0.8, 0, SCORE))

  # If the token is a digit then multiply the score by 4
  non_exact_match_df <- non_exact_match_df %>%
    dplyr::mutate(SCORE = ifelse(TOKEN_TYPE == "D", SCORE * 4, SCORE))

  # Get the max score for each main token in the main address from each
  # secondary address
  non_exact_match_df <- non_exact_match_df %>%
    dplyr::group_by(dplyr::across(-c(SECONDARY_TOKEN, SCORE))) %>%
    dplyr::summarise(SCORE = max(SCORE)) %>%
    dplyr::ungroup()

  # Sum the score for each single line address combination and also include a
  # theoretical max score
  non_exact_match_df <- non_exact_match_df %>%
    dplyr::group_by(
      dplyr::across(-c(MAIN_TOKEN, TOKEN_TYPE, SCORE))
    ) %>%
    dplyr::summarise(
      SCORE = sum(SCORE),
      MAX_SCORE = sum(ifelse(TOKEN_TYPE == "D", 4, 1))
    ) %>%
    dplyr::ungroup()

  # Normalise the score
  non_exact_match_df <- non_exact_match_df %>%
    dplyr::mutate(SCORE = SCORE / MAX_SCORE) %>%
    dplyr::select(-MAX_SCORE)

  # Take the top scoring secondary address for each main address (if there are
  # draws then keep all of them)
  non_exact_match_df <- non_exact_match_df %>%
    dplyr::group_by(POSTCODE, SINGLE_LINE_ADDRESS) %>%
    dplyr::slice_max(order_by = SCORE) %>%
    dplyr::ungroup()

  # Output the exact and non exact matches together
  df %>%
    dplyr::anti_join(
      y = non_exact_match_df,
      by = c(
        main_postcode_col = secondary_postcode_col,
        main_address_col = secondary_address_col
      ),
      copy = TRUE
    ) %>%
    dplyr::union_all(
      y = non_exact_match_df %>%
        dplyr::mutate(MATCH_TYPE = "NON EXACT")
    )

}
