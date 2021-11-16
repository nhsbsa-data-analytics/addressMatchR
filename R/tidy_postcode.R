#' Tidy a postcode
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

      {{ col }} := REGEXP_REPLACE(toupper({{ col }}), # Uppercase
                                  "[^A-Z0-9]", "")    # Remove anything not a character or digit

    )
}
