#' Match two sets of addresses, using dataframes rather than lazy_tables
#'
#' Match a distinct dataframe of primary addresses to a distinct dataframe of
#' lookup addresses.
#'
#' Returns a dataframe of exact matches (postcode and single line address) and
#' non exact matches (postcode and fuzzy single line address). For non exact
#' matches it will retain draws.
#'
#' Non-matching records will also be returned in the output
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
calc_match_addresses_df <- function(
    primary_df,
    primary_postcode_col,
    primary_address_col,
    lookup_df,
    lookup_postcode_col,
    lookup_address_col
) {
  # Disable group by override groups notice
  options(dplyr.summarise.inform = FALSE)

  # Check if there are zero shared postcodes
  row_count = lookup_df %>%
    dplyr::rename("{primary_postcode_col}" := .data[[lookup_postcode_col]]) %>%
    dplyr::inner_join(
      primary_df,
      by = primary_postcode_col,
      relationship = "many-to-many"
    ) %>%
    nrow(.)

  # Break if zero shared postcodes
  if(row_count == 0){
    message("There are no shared postcodes between the datasets so no matching was possible")
    return(NULL)
  }

  # Catch error when supplied data is too big
  out = tryCatch(
    {
      # Generate primary df ID
      primary_df = primary_df %>%
        dplyr::mutate(ID = dplyr::row_number())

      # Generate lookup df ID
      lookup_df = lookup_df %>%
        dplyr::mutate(ID_LOOKUP = dplyr::row_number()) %>%
        dplyr::rename("{primary_postcode_col}" := .data[[lookup_postcode_col]])

      # Find exact matches
      exact_match_df = dplyr::inner_join(
        x = primary_df %>%
          dplyr::mutate(JOIN_ADDRESS = .data[[primary_address_col]]),
        y = lookup_df %>%
          dplyr::mutate(JOIN_ADDRESS = .data[[lookup_address_col]]),
        by = c(primary_postcode_col, "JOIN_ADDRESS"),
        suffix = c("", "_LOOKUP")
        ) %>%
        dplyr::select(ID, ID_LOOKUP) %>%
        dplyr::mutate(
          SCORE = 1,
          MATCH_TYPE = "EXACT"
        )

      # Identify then tokenise non exact matches
      non_exact_match_df <- primary_df %>%
        dplyr::anti_join(
          y = exact_match_df,
          na_matches = "na",
          by = "ID"
        ) %>%
        tidytext::unnest_tokens(
          output = "TOKEN",
          input = primary_address_col,
          to_lower = FALSE,
          drop = FALSE,
          token = stringr::str_split,
          pattern = "\\s"
          ) %>%
        dplyr::group_by(ID) %>%
        dplyr::mutate(
          TOKEN_WEIGHT = dplyr::if_else(grepl("[0-9]", TOKEN) == TRUE, 4, 1),
          TOKEN_NUMBER = row_number(),
          # Add the theoretical max score for each non exact match address
          MAX_SCORE = sum(TOKEN_WEIGHT, na.rm = TRUE)
        ) %>%
        dplyr::ungroup()

      # Tokenise lookup addresses
      lookup_tokens_df <- lookup_df %>%
        tidytext::unnest_tokens(
          output = "TOKEN",
          input = lookup_address_col,
          to_lower = FALSE,
          drop = FALSE,
          token = stringr::str_split,
          pattern = "\\s"
          ) %>%
        # Only need 1 instance of token per lookup ID
        dplyr::distinct() %>%
        dplyr::mutate(
          TOKEN_WEIGHT = dplyr::if_else(grepl("[0-9]", TOKEN) == TRUE, 4, 1)
          )

      # Score remaining matches
      non_exact_match_df = dplyr::full_join(
        x = non_exact_match_df,
        y = lookup_tokens_df,
        by = c(primary_postcode_col, "TOKEN_WEIGHT"),
        suffix = c("", "_LOOKUP"),
        relationship = "many-to-many"
        ) %>%
        # Remove unwanted token pairs (with jw score < 0.8)
        dplyr::filter(
          # Remove NA tokens
          !is.na(TOKEN),
          !is.na(TOKEN_LOOKUP),
          # Tokens share same first letter
          substr(TOKEN_LOOKUP, 1, 1) == substr(TOKEN, 1, 1) |
            # Tokens share same second letter
            substr(TOKEN_LOOKUP, 2, 2) == substr(TOKEN, 2, 2) |
            # Tokens share same last letter
            substr(TOKEN_LOOKUP, nchar(TOKEN_LOOKUP), nchar(TOKEN_LOOKUP)) ==
            substr(TOKEN, nchar(TOKEN_LOOKUP), nchar(TOKEN_LOOKUP)) |
            # One token is a substring of the other
            stringr::str_detect(TOKEN, TOKEN_LOOKUP) == 1 |
            stringr::str_detect(TOKEN_LOOKUP, TOKEN) == 1
        ) %>%
        # Score remaining token pairs
        dplyr::mutate(
          SCORE = dplyr::case_when(
            # Exact matches
            TOKEN == TOKEN_LOOKUP ~ 1,
            (TOKEN != TOKEN_LOOKUP) & (TOKEN_WEIGHT == 4) ~ 0,
            TOKEN != TOKEN_LOOKUP ~ stringdist::stringsim(
              a = TOKEN,
              b = TOKEN_LOOKUP,
              method = "jw",
              p = 0.1
              )
            )
        ) %>%
        # Remove tokens with score less than 0.8 then multiple by weight
        dplyr::filter(SCORE > 0.8) %>%
        dplyr::mutate(SCORE = SCORE * TOKEN_WEIGHT) %>%
        # Max score per token
        dplyr::group_by(ID, ID_LOOKUP, MAX_SCORE, TOKEN_NUMBER) %>%
        dplyr::summarise(SCORE = max(SCORE, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        # Sum scores per ID pair & generate score out of maximum score
        dplyr::group_by(ID, ID_LOOKUP, MAX_SCORE) %>%
        dplyr::summarise(SCORE = sum(SCORE, na.rm = TRUE)) %>%
        dplyr::mutate(SCORE = SCORE / MAX_SCORE) %>%
        dplyr::ungroup() %>%
        dplyr::select(-MAX_SCORE) %>%
        # Slice top score with ties per primary df ID
        dplyr::group_by(ID) %>%
        dplyr::slice_max(order_by = SCORE, with_ties = TRUE) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(MATCH_TYPE = "NON-EXACT")

      # Identify records not exact or non-exact matched
      no_match_df = primary_df %>%
        dplyr::anti_join(
          exact_match_df %>% select(ID) %>% distinct(),
          by = "ID"
        ) %>%
        dplyr::anti_join(
          non_exact_match_df %>% select(ID) %>% distinct(),
          by = "ID"
        ) %>%
        dplyr::transmute(
          ID,
          ID_LOOKUP = NA,
          SCORE = 0,
          MATCH_TYPE = "NONE"
        )

      # Stack the exact and non exact matches together and output
      output = dplyr::bind_rows(exact_match_df, non_exact_match_df, no_match_df) %>%
        dplyr::left_join(
          primary_df,
          by = "ID"
        ) %>%
        dplyr::left_join(
          lookup_df,
          by = "ID_LOOKUP",
          suffix = c("", "_LOOKUP")
        ) %>%
        dplyr::group_by(ID) %>%
        dplyr::mutate(MATCH_COUNT = n_distinct(ID_LOOKUP, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::select(-c(ID, ID_LOOKUP)); gc()

      # Return output
      return(output)
  },
  error = function(cond){
    gc()
    items = c(
    "The size of the datasets attempting to be matched are too big and the function has failed.",
    "Try looping through groups of postcodes, such as several thousand at a time.",
    "Also ensure each matching df only contains distinct address-postcode records, to streamline the process.",
    "Although RAM dependent, the function has the ability to match up to tens of thousands of records on each side.",
    "For large-scale matching tasks, please use the database version of this function: addressMatchR::calc_match_addresses()."
    )
    message(paste(" ", items, sep = "\n"))
    return(NULL)
    }
  )
  return(out)
}
