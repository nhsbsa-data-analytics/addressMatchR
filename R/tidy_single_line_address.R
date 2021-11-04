#' Tidy a single line address
#'
#' Tidy a single line address ready for tokenising.
#'
#' @param df Database table
#' @param col Single line address column
#' @param remove_postcode If to remove the postcode. Default is TRUE.
#'
#' @examples
#' @export
tidy_single_line_address <- function(df, col, remove_postcode = TRUE) {

  # Remove postcode from single line address if neccesary
  if (remove_postcode) {
    df <- df %>%
      dplyr::mutate({{ col }} := REGEXP_REPLACE({{ col }}, "[,][^,]+$", ""))
  }

  # Prep the single line address for tokenisation
  df %>%
    dplyr::mutate(

      # Replace special characters with a single space
      {{ col }} := REGEXP_REPLACE({{ col }}, "[,.();:#'']", " "),

      # Add a space between a digit followed by a non-digit (e.g. "1A" -> "1 A")
      {{ col }} := REGEXP_REPLACE({{ col }}, "(\\d)(\\D)", "\\1 \\2"),

      # Add a space between a non-digit followed by a digit (e.g. "A1" -> "A 1")
      {{ col }} := REGEXP_REPLACE({{ col }}, "(\\D)(\\d)", "\\1 \\2"),

      # Replace ambersand character with "and"
      {{ col }} := REGEXP_REPLACE({{ col }}, "&", " AND "),

      # Replace multiple spaces with a single space
      {{ col }} := REGEXP_REPLACE({{ col }}, "( ){2,}", " "),

      # Remove any spaces around a hyphen
      {{ col }} := REGEXP_REPLACE({{ col }}, " - ", "-")
    )
}
