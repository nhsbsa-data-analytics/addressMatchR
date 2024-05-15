#' Tidy a dataframe single line address
#'
#' Tidy a single line address ready for tokenising.
#'
#' @param df Must be a dataframe (i.e. not a lazy_frame)
#' @param col Single line address column
#' @param remove_postcode If to remove the postcode. Default is FALSE
#'
#' @examples
#' @export
#'
tidy_single_line_address_df <- function(df, col, remove_postcode = FALSE) {

  # Remove postcode from single line address if necessary (e.g. after last ",")
  if (remove_postcode) {
    # Else process as df
    df <- df %>% dplyr::mutate({{ col }} := gsub("[,][^,]+$", "", {{ col }}))
    }

  # Process df
  df %>%
    dplyr::mutate(
      # Address cleaning
      {{ col }} := toupper({{ col }}),
      {{ col }} := gsub(" & ", " AND ", {{ col }}),
      {{ col }} := gsub("(\\D)(\\d)", "\\1 \\2", {{ col }}),
      {{ col }} := gsub("(\\d)(\\D)", "\\1 \\2", {{ col }}),
      {{ col }} := gsub("[,.();:#''\"]", " ", {{ col }}),
      {{ col }} := stringr::str_squish({{ col }}),
      {{ col }} := ifelse(
        grepl("[0-9] - [0-9]", {{ col }}) == TRUE,
        gsub(" - ", "-", {{ col }}),
        {{ col }}
      )
    )
}
