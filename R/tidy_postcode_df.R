#' Tidy a datarame postcode
#'
#' Tidy a postcode so it is ready for joining.
#'
#' @param df dataFrame
#' @param col Postcode column
#'
#' @examples
#'
#' @export
tidy_postcode_df <- function(df, col) {

  # Tide the postcode column
  df %>%
    dplyr::mutate(
      # Remove anything not a character or digit
      {{ col }} := gsub("[^A-Z0-9]", "", toupper({{ col }}))
    )
}
