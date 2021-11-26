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
  only_geo_single_line_address_df <- df %>%
    dplyr::filter(is.na(DPA_SINGLE_LINE_ADDRESS)) %>%
    dplyr::mutate(ADDRESS_TYPE = "GEO ONLY") %>%
    dplyr::select(
      RELEASE_DATE,
      UPRN,
      CLASS,
      ADDRESS_TYPE,
      POSTCODE,
      SINGLE_LINE_ADDRESS = GEO_SINGLE_LINE_ADDRESS
    )

  # For rows that have DPA == GEO single line address we group these as BOTH
  equal_single_line_address_df <- df %>%
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

  # Calculate a combined single line address and make the table long
  different_single_line_address_df <- df %>%
    dplyr::filter(DPA_SINGLE_LINE_ADDRESS != GEO_SINGLE_LINE_ADDRESS) %>%
    nhsbsaR::oracle_merge_strings(
      first_col = "DPA_SINGLE_LINE_ADDRESS",
      second_col = "GEO_SINGLE_LINE_ADDRESS",
      merge_col = "COMBINED_SINGLE_LINE_ADDRESS"
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::ends_with("_SINGLE_LINE_ADDRESS"),
      names_to = "ADDRESS_TYPE",
      values_to = "SINGLE_LINE_ADDRESS"
    )

  # Stack the three tables together again
  df <-
    only_geo_single_line_address_df %>%
    dplyr::union_all(y = equal_single_line_address_df) %>%
    dplyr::union_all(y = different_single_line_address_df)

  # Write the table back to the DB
  df %>%
    nhsbsaR::oracle_create_table(table_name = table_name)


}
