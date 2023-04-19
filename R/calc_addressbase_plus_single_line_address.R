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
      unite.tbl(
        .data$DPA_SINGLE_LINE_ADDRESS,
        .data$DPA_SINGLE_LINE_ADDRESS,
        .data$POSTCODE
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
        (!is.null(.data$SAO_START_NUMBER) | !is.null(.data$SAO_START_SUFFIX)) &
        (!is.null(.data$SAO_END_NUMBER) | !is.null(.data$SAO_END_SUFFIX))
      ),
      PAO_DASH = ifelse(
        (!is.null(.data$PAO_START_NUMBER) | !is.null(.data$PAO_START_SUFFIX)) &
          (!is.null(.data$PAO_END_NUMBER) | !is.null(.data$PAO_END_SUFFIX))
      )
    ) %>%
    unite.tbl(
      .data$GEO_SINGLE_LINE_ADDRESS,
      .data$LA_ORGANISATION,
      .data$SAO_TEXT,
      .data$SAO_START_NUMBER,
      .data$SAO_START_SUFFIX,
      .data$SAO_DASH,
      .data$SAO_END_NUMBER,
      .data$SAO_END_SUFFIX,
      .data$PAO_TEXT,
      .data$PAO_START_NUMBER,
      .data$PAO_START_SUFFIX,
      .data$PAO_DASH,
      .data$PAO_END_NUMBER,
      .data$PAO_END_SUFFIX,
      .data$STREET_DESCRIPTION,
      .data$LOCALITY,
      .data$TOWN_NAME,
      sep = " ",
      remove = FALSE,
      na.rm = TRUE
    )

  # Add the postcode if necessary
  if (include_postcode) {

    df <- df %>%
      unite.tbl(
        .data$GEO_SINGLE_LINE_ADDRESS,
        .data$GEO_SINGLE_LINE_ADDRESS,
        .data$POSTCODE_LOCATOR
      )

  }

  df

}
