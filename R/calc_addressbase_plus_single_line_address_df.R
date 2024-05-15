
#' Calculate AddressBase Plus DPA single line address.
#'
#' @param df local df with individual address base plus fields
#' @param include_postcode Whether or not to include postcode. Default is FALSE.
#'
#' @examples
#' @export
# Calculate AddressBase Plus DPA single line address
calc_addressbase_plus_dpa_single_line_address <- function(
    df,
    include_postcode = FALSE
    ) {
  df <- df %>%
    dplyr::mutate(
      DPA_SINGLE_LINE_ADDRESS = paste0(
        ifelse(
          test = !is.na(DEPARTMENT_NAME),
          yes = paste0(DEPARTMENT_NAME, ", "),
          no = ""
        ),
        ifelse(
          test = !is.na(RM_ORGANISATION_NAME),
          yes = paste0(RM_ORGANISATION_NAME, ", "),
          no = ""
        ),
        ifelse(
          test = !is.na(SUB_BUILDING_NAME),
          yes = paste0(SUB_BUILDING_NAME, ", "),
          no = ""
        ),
        ifelse(
          test = !is.na(BUILDING_NAME),
          yes = paste0(BUILDING_NAME, ", "),
          no = ""
        ),
        ifelse(
          test = !is.na(BUILDING_NUMBER),
          yes = paste0(BUILDING_NUMBER, " "),
          no = ""
        ),
        ifelse(
          test = !is.na(PO_BOX_NUMBER),
          yes = paste0("PO BOX ", PO_BOX_NUMBER, ", "),
          no = ""
        ),
        ifelse(
          test = !is.na(DEP_THOROUGHFARE),
          yes = paste0(DEP_THOROUGHFARE, ", "),
          no = ""
        ),
        ifelse(
          test = !is.na(THOROUGHFARE),
          yes = paste0(THOROUGHFARE, ", "),
          no = ""
        ),
        ifelse(
          test = !is.na(DOU_DEP_LOCALITY),
          yes = paste0(DOU_DEP_LOCALITY, ", "),
          no = ""
        ),
        ifelse(
          test = !is.na(DEP_LOCALITY),
          yes = paste0(DEP_LOCALITY, ", "),
          no = ""
        ),
        ifelse(
          test = !is.na(POST_TOWN),
          yes = paste0(POST_TOWN, ", "),
          no = ""
        )
      )
    )

  # Add the postcode if necessary
  if (include_postcode) {

    df <- df %>%
      dplyr::mutate(
        DPA_SINGLE_LINE_ADDRESS = paste0(
          DPA_SINGLE_LINE_ADDRESS,
          ifelse(
            test = !is.na(POSTCODE),
            yes = paste0(POSTCODE),
            no = ""
          )
        )
      )
  }
  df
}


#' Calculate AddressBase Plus GEO single line address.
#'
#' @param df local df with individual address base plus fields
#' @param include_postcode Whether or not to include postcode. Default is FALSE.
#'
#' @examples
#' @export
# Calculate AddressBase Plus GEO single line address
calc_addressbase_plus_geo_single_line_address <- function(
    df,
    include_postcode=FALSE
    ) {
  df <- df %>%
    dplyr::mutate(
      GEO_SINGLE_LINE_ADDRESS = paste0(
        ifelse(
          test = !is.na(LA_ORGANISATION),
          yes = paste0(LA_ORGANISATION, ", "),
          no = ""
        ),
        ifelse(
          test = !is.na(SAO_TEXT),
          yes = paste0(SAO_TEXT, ", "),
          no = ""
        ),
        ifelse(
          test = !is.na(SAO_START_NUMBER) &
            is.na(SAO_START_SUFFIX) &
            is.na(SAO_END_NUMBER),
          yes = paste0(SAO_START_NUMBER, ", "),
          no = ifelse(
            test = is.na(SAO_START_NUMBER),
            yes = "",
            no = as.character(SAO_START_NUMBER)
          )
        ),
        ifelse(
          test = !is.na(SAO_START_SUFFIX) & is.na(SAO_END_NUMBER),
          yes = paste0(SAO_START_SUFFIX, ", "),
          no = ifelse(
            test = !is.na(SAO_START_SUFFIX) & !is.na(SAO_END_NUMBER),
            yes = SAO_START_SUFFIX,
            no = ""
          )
        ),
        ifelse(
          test = !is.na(SAO_END_SUFFIX) & !is.na(SAO_END_NUMBER),
          yes = "-",
          no = ifelse(
            test = !is.na(SAO_START_NUMBER) & !is.na(SAO_END_NUMBER),
            yes = "-",
            no = ""
          )
        ),
        ifelse(
          test = !is.na(SAO_END_NUMBER) & is.na(SAO_END_SUFFIX),
          yes = paste0(SAO_END_NUMBER, ", "),
          no = ifelse(
            test = is.na(SAO_END_NUMBER),
            yes = "",
            no = as.character(SAO_END_NUMBER)
          )
        ),
        ifelse(
          test = !is.na(PAO_TEXT),
          yes = paste0(PAO_TEXT, ", "),
          no = ""
        ),
        ifelse(
          test = !is.na(PAO_START_NUMBER) &
            is.na(PAO_START_SUFFIX) &
            is.na(PAO_END_NUMBER),
          yes = paste0(PAO_START_NUMBER, ", "),
          no = ifelse(
            test = is.na(PAO_START_NUMBER),
            yes = "",
            no = as.character(PAO_START_NUMBER)
          )
        ),
        ifelse(
          test = !is.na(PAO_START_SUFFIX) & is.na(PAO_END_NUMBER),
          yes = paste0(PAO_START_SUFFIX, ", "),
          no = ifelse(
            test = !is.na(PAO_START_SUFFIX) & !is.na(PAO_END_NUMBER),
            yes = PAO_START_SUFFIX,
            no = ""
          )
        ),
        ifelse(
          test = !is.na(PAO_END_SUFFIX) & !is.na(PAO_END_NUMBER),
          yes = "-",
          no = ifelse(
            test = !is.na(PAO_START_NUMBER) & !is.na(PAO_END_NUMBER),
            yes = "-",
            no = ""
          )
        ),
        ifelse(
          test = !is.na(PAO_END_NUMBER) & is.na(PAO_END_SUFFIX),
          yes = paste0(PAO_END_NUMBER, ", "),
          no = ifelse(
            test = is.na(PAO_END_NUMBER),
            yes = "",
            no = as.character(PAO_END_NUMBER)
          )
        ),
        ifelse(
          test = !is.na(STREET_DESCRIPTION),
          yes = paste0(STREET_DESCRIPTION, ", "),
          no = ""
        ),
        ifelse(
          test = !is.na(LOCALITY),
          yes = paste0(LOCALITY, ", "),
          no = ""
        ),
        ifelse(
          test = !is.na(TOWN_NAME),
          yes = paste0(TOWN_NAME, ", "),
          no = ""
        )
      )
    )

  # Add the postcode if necessary
  if (include_postcode) {

    df <- df %>%
      dplyr::mutate(
        GEO_SINGLE_LINE_ADDRESS = paste0(
          GEO_SINGLE_LINE_ADDRESS,
          ifelse(
            test = !is.na(POSTCODE_LOCATOR),
            yes = paste0(POSTCODE_LOCATOR),
            no = ""
          )
        )
      )
  }
  df
}
