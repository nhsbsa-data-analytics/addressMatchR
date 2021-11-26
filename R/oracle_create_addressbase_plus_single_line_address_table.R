#' Create a version of AddressBase Plus with the single line addresses
#'
#' Create a processed version of AddressBase Plus that contains the main columns
#' needed along with up to 3 versions of a single line address per UPRN.
#'
#' @param df Database table ("ADDRESSBASE_PLUS")
#' @param table_name Table name. Default to
#'   "ADDRESS_BASE_PLUS_SINGLE_LINE_ADDRESS"
#'
#' @examples
#' @export
oracle_create_addressbase_single_line_address_table <- function(
  df,
  table_name="ADDRESS_BASE_PLUS_SINGLE_LINE_ADDRESS"
) {

  # Filter AddressBase Plus to English properties
  df <- df %>%
    dplyr::filter(
      COUNTRY == "E",
      substr(CLASS, 1, 1) != "L", # Land
      substr(CLASS, 1, 1) != "O", # Other (Ordnance Survey only)
      substr(CLASS, 1, 2) != "PS", # Street Record
      substr(CLASS, 1, 2) != "RC", # Car Park Space
      substr(CLASS, 1, 2) != "RG", # Lock-Up / Garage / Garage Court
      substr(CLASS, 1, 1) != "Z" # Object of interest
    )

  # Add the DPA and GEO single line addresses and tidy the cols
  df <- df %>%
    addressMatchR::calc_addressbase_plus_dpa_single_line_address() %>%
    addressMatchR::calc_addressbase_plus_geo_single_line_address() %>%
    dplyr::select(
      RELEASE_DATE,
      UPRN,
      CLASS,
      POSTCODE = POSTCODE_LOCATOR,
      DPA_SINGLE_LINE_ADDRESS,
      GEO_SINGLE_LINE_ADDRESS
    ) %>%
    addressMatchR::tidy_postcode(col = POSTCODE) %>%
    addressMatchR::tidy_single_line_address(col = DPA_SINGLE_LINE_ADDRESS) %>%
    addressMatchR::tidy_single_line_address(col = GEO_SINGLE_LINE_ADDRESS)

  # For rows where DPA single line address is NA we have GEO only
  df <- df %>%
    dplyr::filter(is.na(DPA_SINGLE_LINE_ADDRESS)) %>%
    dplyr::filter(DPA_SINGLE_LINE_ADDRESS == GEO_SINGLE_LINE_ADDRESS) %>%
    dplyr::mutate(ADDRESS_TYPE = "GEO ONLY") %>%
    dplyr::select(
      RELEASE_DATE,
      UPRN,
      CLASS,
      ADDRESS_TYPE,
      POSTCODE,
      SINGLE_LINE_ADDRESS = DPA_SINGLE_LINE_ADDRESS
    )

  # For rows that have DPA == GEO single line address we group these as BOTH
  addressbase_plus_both_address_type_db <- addressbase_plus_db %>%
    dplyr::filter(DPA_SINGLE_LINE_ADDRESS == GEO_SINGLE_LINE_ADDRESS) %>%
    dplyr::mutate(ADDRESS_TYPE = "BOTH") %>%
    dplyr::select(
      RELEASE_DATE,
      UPRN,
      CLASS,
      ADDRESS_TYPE,
      POSTCODE,
      SINGLE_LINE_ADDRESS = DPA_SINGLE_LINE_ADDRESS
    )

  # Calculate a CMD (combined) single line address
  # and make the table long
  addressbase_plus_diff_address_type_db <- addressbase_plus_db %>%
    filter(DPA_SINGLE_LINE_ADDRESS != GEO_SINGLE_LINE_ADDRESS) %>%
    nhsbsaR::oracle_merge_strings(
      first_col = "DPA_SINGLE_LINE_ADDRESS",
      second_col = "GEO_SINGLE_LINE_ADDRESS",
      merge_col = "COMBINED_SINGLE_LINE_ADDRESS"
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::ends_with("_SINGLE_LINE_ADDRESS"),
      names_to = "ADDRESS_TYPE",
      names_sep = "_",
      values_to = "SINGLE_LINE_ADDRESS"
    )

  # Stack the three tables together again
  addressbase_plus_db <-
    addressbase_plus_geo_only_address_type_db %>%
    union(y = addressbase_plus_both_address_type_db) %>%
    union(y = addressbase_plus_diff_address_type_db)

}
