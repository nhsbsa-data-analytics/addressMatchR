#' Merge Two Strings Together Whilst Retaining an Order of Some Kind
#'
#' This is intended to be used with differing address strings
#' The result is a 'master' address string that contains all parts of each address
#'
#' @param df local df
#' @param first_col First column
#' @param second_col Second column
#' @param merge_col Name to give merged column
#'
#' @examples
#' table_df %>%
#'   mutate(
#'     merged_string = mapply(merge_strings_df, string1, string2)
#'     )
#'
#' @returns when used with mapply, original df with additional merged column
#' @export
merge_strings_df <- function(string1, string2) {

  # Split strings into individual tokens
  tokens1 <- strsplit(string1, "\\s+")[[1]]
  tokens2 <- strsplit(string2, "\\s+")[[1]]

  # Initialize a vector to store unique tokens
  unique_tokens <- character(0)

  # Initialize merged tokens
  merged_tokens <- c()

  # Interweave unique tokens from string1 and string2
  for (i in 1:max(length(tokens1), length(tokens2))) {
    if (i <= length(tokens1) && !tokens1[i] %in% unique_tokens) {
      merged_tokens <- c(merged_tokens, tokens1[i])
      unique_tokens <- c(unique_tokens, tokens1[i])
    }
    if (i <= length(tokens2) && !tokens2[i] %in% unique_tokens) {
      merged_tokens <- c(merged_tokens, tokens2[i])
      unique_tokens <- c(unique_tokens, tokens2[i])
    }
  }

  # Concatenate tokens back into a single string
  merged_string <- paste(merged_tokens, collapse = " ")

  # Return merged string
  return(merged_string)
}
