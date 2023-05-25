#' Tidy a single line address
#'
#' Tidy a single line address ready for tokenising.
#'
#' @param df Either a database table or a local data frame
#' @param col Single line address column
#' @param remove_postcode If to remove the postcode. Default is FALSE
#'
#' @examples
#' @export
#'
tidy_single_line_address <- function(df, col, remove_postcode = FALSE) {

  # Remove postcode from single line address if necessary (e.g. after last ",")
  if (remove_postcode) {

    # Check if lazy table
    if(inherits(df, c("tbl_dbi", "tbl_lazy"))){

      # Process as lazy table
      df <- df %>%
        dplyr::mutate({{ col }} := REGEXP_REPLACE({{ col }}, "[,][^,]+$", ""))
    }else{

      # Else process as df
      df <- df %>%
        dplyr::mutate({{ col }} := gsub("[,][^,]+$", "", {{ col }}))
    }
  }

  # Check if object is a lazy table
  if(inherits(df, c("tbl_dbi", "tbl_lazy"))){

    # Process as a lazy frame
    df %>%
      dplyr::mutate(
        {{ col }} := trimws(REPLACE(REGEXP_REPLACE(REPLACE(REGEXP_REPLACE(REGEXP_REPLACE(REGEXP_REPLACE(toupper({{ col }}),             # Uppercase
                                                                                                                "[,.();:#'']", " "),      # replace special characters with a single space
                                                                                                                "(\\d)(\\D)", "\\1 \\2"), # add a space between any digit followed by a non-digit (e.g. 1A becomes 1 A)
                                                                                                                "(\\D)(\\d)", "\\1 \\2"), # add a space between any non-digit followed by a digit (e.g. A1 becomes A 1)
                                                                                                                "&", " AND "),            # replace the ampersand character with the string "and"
                                                                                                                "( ){2,}", " "),          # replace any multiple spaces with a single space
                                                                                                                " - ", "-")               # remove any spaces around a hyphen
        ),

        # Only remove spaces around hyphen if surrounded by numbers
        {{ col }} := dplyr::case_when(
          REGEXP_INSTR({{ col }}, "[0-9] - [0-9]") > 0L ~ REPLACE(" - ", "-", {{ col }}),
          TRUE ~ {{ col }}
        )
      )
  }else{

    #Process as a data frame
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
}
