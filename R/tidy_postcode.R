#' Tidy a lazy_table postcode
#'
#' Tidy a postcode so it is ready for joining.
#'
#' @param df Database table
#' @param col Postcode column
#'
#' @examples
#'
#' @export
tidy_postcode <- function(df, col) {

  # Tide the postcode column
  df %>%
    dplyr::mutate(
      # Remove anything not a character or digit
      {{ col }} := REGEXP_REPLACE(toupper({{ col }}), "[^A-Z0-9]", "")
    )
}
