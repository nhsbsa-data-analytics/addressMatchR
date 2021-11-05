#' Calculate AddressBase Plus DPA single line address
#'
#' Calculate AddressBase Plus DPA single line address.
#'
#' @param df AddressBase Plus DB table
#'
#' @examples
#' @export
calc_addressbase_plus_dpa_single_line_address <- function(df) {
  df %>%
    dplyr::mutate(
      DPA_SINGLE_LINE_ADDRESS = paste0(
        ifelse(
          test = !is.null(DEPARTMENT_NAME),
          yes = paste0(DEPARTMENT_NAME, ", "),
          no = ""
        ),
        ifelse(
          test = !is.null(RM_ORGANISATION_NAME),
          yes = paste0(RM_ORGANISATION_NAME, ", "),
          no = ""
        ),
        ifelse(
          test = !is.null(SUB_BUILDING_NAME),
          yes = paste0(SUB_BUILDING_NAME, ", "),
          no = ""
        ),
        ifelse(
          test = !is.null(BUILDING_NAME),
          yes = paste0(BUILDING_NAME, ", "),
          no = ""
        ),
        ifelse(
          test = !is.null(BUILDING_NUMBER),
          yes = paste0(BUILDING_NUMBER, " "),
          no = ""
        ),
        ifelse(
          test = !is.null(PO_BOX_NUMBER),
          yes = paste0("PO BOX ", PO_BOX_NUMBER, ", "),
          no = ""
        ),
        ifelse(
          test = !is.null(DEP_THOROUGHFARE),
          yes = paste0(DEP_THOROUGHFARE, ", "),
          no = ""
        ),
        ifelse(
          test = !is.null(THOROUGHFARE),
          yes = paste0(THOROUGHFARE, ", "),
          no = ""
        ),
        ifelse(
          test = !is.null(DOU_DEP_LOCALITY),
          yes = paste0(DOU_DEP_LOCALITY, ", "),
          no = ""
        ),
        ifelse(
          test = !is.null(DEP_LOCALITY),
          yes = paste0(DEP_LOCALITY, ", "),
          no = ""
        ),
        ifelse(
          test = !is.null(POST_TOWN),
          yes = paste0(POST_TOWN, ", "),
          no = ""
        ),
        POSTCODE
      )
    )
}


#' Calculate AddressBase Plus GEO single line address
#'
#' Calculate AddressBase Plus GEO single line address.
#'
#' @param df AddressBase Plus DB table
#'
#' @examples
#' @export
calc_addressbase_plus_geo_single_line_address <- function(df) {
  df %>%
    dplyr::mutate(
      GEO_SINGLE_LINE_ADDRESS = paste0(
        ifelse(
          test = !is.null(LA_ORGANISATION),
          yes = paste0(LA_ORGANISATION, ", "),
          no = ""
        ),
        ifelse(
          test = !is.null(SAO_TEXT),
          yes = paste0(SAO_TEXT, ", "),
          no = ""
        ),
        ifelse(
          test = !is.null(SAO_START_NUMBER) &
            is.null(SAO_START_SUFFIX) &
            is.null(SAO_END_NUMBER),
          yes = paste0(SAO_START_NUMBER, ", "),
          no = ifelse(
            test = is.null(SAO_START_NUMBER),
            yes = "",
            no = as.character(SAO_START_NUMBER)
          )
        ),
        ifelse(
          test = !is.null(SAO_START_SUFFIX) & is.null(SAO_END_NUMBER),
          yes = paste0(SAO_START_SUFFIX, ", "),
          no = ifelse(
            test = !is.null(SAO_START_SUFFIX) & !is.null(SAO_END_NUMBER),
            yes = SAO_START_SUFFIX,
            no = ""
          )
        ),
        ifelse(
          test = !is.null(SAO_END_SUFFIX) & !is.null(SAO_END_NUMBER),
          yes = "-",
          no = ifelse(
            test = !is.null(SAO_START_NUMBER) & !is.null(SAO_END_NUMBER),
            yes = "-",
            no = ""
          )
        ),
        ifelse(
          test = !is.null(SAO_END_NUMBER) & is.null(SAO_END_SUFFIX),
          yes = paste0(SAO_END_NUMBER, ", "),
          no = ifelse(
            test = is.null(SAO_END_NUMBER),
            yes = "",
            no = as.character(SAO_END_NUMBER)
          )
        ),
        ifelse(
          test = !is.null(PAO_TEXT),
          yes = paste0(PAO_TEXT, ", "),
          no = ""
        ),
        ifelse(
          test = !is.null(PAO_START_NUMBER) &
            is.null(PAO_START_SUFFIX) &
            is.null(PAO_END_NUMBER),
          yes = paste0(PAO_START_NUMBER, ", "),
          no = ifelse(
            test = is.null(PAO_START_NUMBER),
            yes = "",
            no = as.character(PAO_START_NUMBER)
          )
        ),
        ifelse(
          test = !is.null(PAO_START_SUFFIX) & is.null(PAO_END_NUMBER),
          yes = paste0(PAO_START_SUFFIX, ", "),
          no = ifelse(
            test = !is.null(PAO_START_SUFFIX) & !is.null(PAO_END_NUMBER),
            yes = PAO_START_SUFFIX,
            no = ""
          )
        ),
        ifelse(
          test = !is.null(PAO_END_SUFFIX) & !is.null(PAO_END_NUMBER),
          yes = "-",
          no = ifelse(
            test = !is.null(PAO_START_NUMBER) & !is.null(PAO_END_NUMBER),
            yes = "-",
            no = ""
          )
        ),
        ifelse(
          test = !is.null(PAO_END_NUMBER) & is.null(PAO_END_SUFFIX),
          yes = paste0(PAO_END_NUMBER, ", "),
          no = ifelse(
            test = is.null(PAO_END_NUMBER),
            yes = "",
            no = as.character(PAO_END_NUMBER)
          )
        ),
        ifelse(
          test = !is.null(STREET_DESCRIPTION),
          yes = paste0(STREET_DESCRIPTION, ", "),
          no = ""
        ),
        ifelse(
          test = !is.null(LOCALITY),
          yes = paste0(LOCALITY, ", "),
          no = ""
        ),
        ifelse(
          test = !is.null(TOWN_NAME),
          yes = paste0(TOWN_NAME, ", "),
          no = ""
        ),
        ifelse(
          test = !is.null(POSTCODE_LOCATOR),
          yes = POSTCODE_LOCATOR,
          no = ""
        )
      )
    )
}
