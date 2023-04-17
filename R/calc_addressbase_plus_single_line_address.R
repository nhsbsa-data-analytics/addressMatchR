#' Calculate AddressBase Plus DPA single line address
#'
#' Calculate AddressBase Plus DPA single line address.
#'
#' @param df AddressBase Plus DB table
#' @param include_postcode Whether or not to include postcode. Default is FALSE.
#'
#' @importFrom rlang .data
#'
#' @examples
#' @export
calc_addressbase_plus_dpa_single_line_address <- function(
  df,
  include_postcode = FALSE
) {

  # Create the single line address
  df <- df %>%
    unite.tbl(
      .data$DPA_SINGLE_LINE_ADDRESS,
      .data$DEPARTMENT_NAME,
      .data$RM_ORGANISATION_NAME,
      .data$SUB_BUILDING_NAME,
      .data$BUILDING_NAME,
      .data$BUILDING_NUMBER,
      .data$PO_BOX_NUMBER,
      .data$DEP_THOROUGHFARE,
      .data$THOROUGHFARE,
      .data$DOU_DEP_LOCALITY,
      .data$DEP_LOCALITY,
      .data$POST_TOWN,
      sep = " ",
      remove = FALSE,
      na.rm = TRUE
    )

  # Add the postcode if necessary
  if (include_postcode) {

    df <- df %>%
      dplyr::mutate(
        DPA_SINGLE_LINE_ADDRESS = paste0(
          .data$DPA_SINGLE_LINE_ADDRESS,
          .data$POSTCODE
        )
      )

  }

  df

}


#' Calculate AddressBase Plus GEO single line address
#'
#' Calculate AddressBase Plus GEO single line address.
#'
#' @param df AddressBase Plus DB table
#' @param include_postcode Whether or not to include postcode. Default is FALSE.
#'
#' @examples
#' @export
calc_addressbase_plus_geo_single_line_address <- function(
  df,
  include_postcode = FALSE
) {

  # Create the single line address
  df <- df %>%
    dplyr::mutate(
      GEO_SINGLE_LINE_ADDRESS = paste0(
        ifelse(
          test = !is.null(.data$LA_ORGANISATION),
          yes = paste0(.data$LA_ORGANISATION, ", "),
          no = ""
        ),
        ifelse(
          test = !is.null(.data$SAO_TEXT),
          yes = paste0(.data$SAO_TEXT, ", "),
          no = ""
        ),
        ifelse(
          test = !is.null(.data$SAO_START_NUMBER) &
            is.null(.data$SAO_START_SUFFIX) &
            is.null(.data$SAO_END_NUMBER),
          yes = paste0(.data$SAO_START_NUMBER, ", "),
          no = ifelse(
            test = is.null(.data$SAO_START_NUMBER),
            yes = "",
            no = as.character(.data$SAO_START_NUMBER)
          )
        ),
        ifelse(
          test = !is.null(.data$SAO_START_SUFFIX) & is.null(.data$SAO_END_NUMBER),
          yes = paste0(.data$SAO_START_SUFFIX, ", "),
          no = ifelse(
            test = !is.null(.data$SAO_START_SUFFIX) & !is.null(.data$SAO_END_NUMBER),
            yes = .data$SAO_START_SUFFIX,
            no = ""
          )
        ),
        ifelse(
          test = !is.null(.data$SAO_END_SUFFIX) & !is.null(.data$SAO_END_NUMBER),
          yes = "-",
          no = ifelse(
            test = !is.null(.data$SAO_START_NUMBER) & !is.null(.data$SAO_END_NUMBER),
            yes = "-",
            no = ""
          )
        ),
        ifelse(
          test = !is.null(.data$SAO_END_NUMBER) & is.null(.data$SAO_END_SUFFIX),
          yes = paste0(.data$SAO_END_NUMBER, ", "),
          no = ifelse(
            test = is.null(.data$SAO_END_NUMBER),
            yes = "",
            no = as.character(.data$SAO_END_NUMBER)
          )
        ),
        ifelse(
          test = !is.null(.data$PAO_TEXT),
          yes = paste0(.data$PAO_TEXT, ", "),
          no = ""
        ),
        ifelse(
          test = !is.null(.data$PAO_START_NUMBER) &
            is.null(.data$PAO_START_SUFFIX) &
            is.null(.data$PAO_END_NUMBER),
          yes = paste0(.data$PAO_START_NUMBER, ", "),
          no = ifelse(
            test = is.null(.data$PAO_START_NUMBER),
            yes = "",
            no = as.character(.data$PAO_START_NUMBER)
          )
        ),
        ifelse(
          test = !is.null(.data$PAO_START_SUFFIX) & is.null(.data$PAO_END_NUMBER),
          yes = paste0(.data$PAO_START_SUFFIX, ", "),
          no = ifelse(
            test = !is.null(.data$PAO_START_SUFFIX) & !is.null(.data$PAO_END_NUMBER),
            yes = .data$PAO_START_SUFFIX,
            no = ""
          )
        ),
        ifelse(
          test = !is.null(.data$PAO_END_SUFFIX) & !is.null(.data$PAO_END_NUMBER),
          yes = "-",
          no = ifelse(
            test = !is.null(.data$PAO_START_NUMBER) & !is.null(.data$PAO_END_NUMBER),
            yes = "-",
            no = ""
          )
        ),
        ifelse(
          test = !is.null(.data$PAO_END_NUMBER) & is.null(.data$PAO_END_SUFFIX),
          yes = paste0(.data$PAO_END_NUMBER, ", "),
          no = ifelse(
            test = is.null(.data$PAO_END_NUMBER),
            yes = "",
            no = as.character(.data$PAO_END_NUMBER)
          )
        ),
        ifelse(
          test = !is.null(.data$STREET_DESCRIPTION),
          yes = paste0(.data$STREET_DESCRIPTION, ", "),
          no = ""
        ),
        ifelse(
          test = !is.null(.data$LOCALITY),
          yes = paste0(.data$LOCALITY, ", "),
          no = ""
        ),
        ifelse(
          test = !is.null(.data$TOWN_NAME),
          yes = paste0(.data$TOWN_NAME, ", "),
          no = ""
        )
      )
    )

  # Add the postcode if necessary
  if (include_postcode) {

    df <- df %>%
      dplyr::mutate(
        GEO_SINGLE_LINE_ADDRESS = paste0(
          .data$GEO_SINGLE_LINE_ADDRESS,
          ifelse(
            test = !is.null(.data$POSTCODE_LOCATOR),
            yes = .data$POSTCODE_LOCATOR,
            no = ""
          )
        )
      )

  }

  df

}
