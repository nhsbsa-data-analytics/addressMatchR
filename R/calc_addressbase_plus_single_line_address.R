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
      DPA_SINGLE_LINE_ADDRESS,
      DEPARTMENT_NAME,
      RM_ORGANISATION_NAME,
      SUB_BUILDING_NAME,
      BUILDING_NAME,
      BUILDING_NUMBER,
      PO_BOX_NUMBER,
      DEP_THOROUGHFARE,
      THOROUGHFARE,
      DOU_DEP_LOCALITY,
      DEP_LOCALITY,
      POST_TOWN,
      sep = ", ",
      remove = FALSE,
      na.rm = TRUE
    )

  # Add the postcode if necessary
  if (include_postcode) {

    df <- df %>%
      unite.tbl(
        DPA_SINGLE_LINE_ADDRESS,
        DPA_SINGLE_LINE_ADDRESS,
        POSTCODE
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
      SAO_DASH = ifelse(
        (!is.null(SAO_START_NUMBER) | !is.null(SAO_START_SUFFIX)) &
        (!is.null(SAO_END_NUMBER) | !is.null(SAO_END_SUFFIX)),
        "-",
        ""
      ),
      PAO_DASH = ifelse(
        (!is.null(PAO_START_NUMBER) | !is.null(PAO_START_SUFFIX)) &
          (!is.null(PAO_END_NUMBER) | !is.null(PAO_END_SUFFIX)),
        "-",
        ""
      )
    ) %>%
    unite.tbl(
      GEO_SINGLE_LINE_ADDRESS,
      LA_ORGANISATION,
      SAO_TEXT,
      SAO_START_NUMBER,
      SAO_START_SUFFIX,
      SAO_DASH,
      SAO_END_NUMBER,
      SAO_END_SUFFIX,
      PAO_TEXT,
      PAO_START_NUMBER,
      PAO_START_SUFFIX,
      PAO_DASH,
      PAO_END_NUMBER,
      PAO_END_SUFFIX,
      STREET_DESCRIPTION,
      LOCALITY,
      TOWN_NAME,
      sep = ", ",
      remove = FALSE,
      na.rm = TRUE
    )

  # Add the postcode if necessary
  if (include_postcode) {

    df <- df %>%
      unite.tbl(
        GEO_SINGLE_LINE_ADDRESS,
        GEO_SINGLE_LINE_ADDRESS,
        POSTCODE_LOCATOR
      )

  }

  df

}
