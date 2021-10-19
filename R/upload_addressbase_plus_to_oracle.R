#' Upload AddressBase Plus to Oracle
#'
#' Upload AddressBase Plus to an Oracle DB. First download the data package for
#' AddressBase Plus using `addressMatchR::download_os_data_package()` and then
#' load it into the DB using this function.
#'
#' Using guidance from
#' https://www.ordnancesurvey.co.uk/documents/addressbase-products-getting-started-guide2.pdf
#'
#' @param con
#' @param data_package_location
#' @param append_existing
#'
#' @examples
#'
#' @export
upload_addressbase_plus_to_oracle <- function(
  con,
  data_package_location,
  append_existing = TRUE
) {

  # Get a list of the data files
  data_files <- list.files(
    path = data_package_location,
    pattern = "data/*.csv$",
    recursive = TRUE,
    full.names = TRUE
  )

  # What date are we loading
  date <- paste0(
    substr(data_files[1], 60, 61),
    "/",
    substr(data_files[1], 57, 58),
    "/",
    substr(data_files[1], 52, 55)
  )

  # Define the fields and types cribbed from
  # https://github.com/OrdnanceSurvey/AddressBase/blob/master/Loading_Scripts/Oracle/AddressBase_Plus_and_Islands/Oracle_AddressBase_Plus_createtable.sql
  fields = c(
    DATE = "DATE",
    UPRN = "NUMBER",
    UDPRN = "NUMBER",
    CHANGE_TYPE = "VARCHAR2(1)",
    STATE = "NUMBER",
    STATE_DATE = "DATE",
    CLASS = "VARCHAR2(6)",
    PARENT_UPRN = "NUMBER",
    X_COORDINATE = "FLOAT",
    Y_COORDINATE = "FLOAT",
    LATITUDE = "FLOAT",
    LONGITUDE = "FLOAT",
    RPC = "NUMBER",
    LOCAL_CUSTODIAN_CODE = "NUMBER",
    COUNTRY = "VARCHAR2(1)",
    LA_START_DATE = "DATE",
    LAST_UPDATE_DATE = "DATE",
    ENTRY_DATE = "DATE",
    RM_ORGANISATION_NAME = "VARCHAR2(60)",
    LA_ORGANISATION = "VARCHAR2(100)",
    DEPARTMENT_NAME = "VARCHAR2(60)",
    LEGAL_NAME = "VARCHAR2(60)",
    SUB_BUILDING_NAME = "VARCHAR2(30)",
    BUILDING_NAME = "VARCHAR2(50)",
    SAO_START_NUMBER = "NUMBER",
    SAO_START_SUFFIX = "VARCHAR2(2)",
    SAO_END_NUMBER = "NUMBER",
    SAO_END_SUFFIX = "VARCHAR2(2)",
    SAO_TEXT = "VARCHAR2(90)",
    ALT_LANGUAGE_SAO_TEXT = "VARCHAR2(90)",
    PAO_START_NUMBER = "NUMBER",
    PAO_START_SUFFIX = "VARCHAR2(2)",
    PAO_END_NUMBER = "NUMBER",
    PAO_END_SUFFIX = "VARCHAR2(2)",
    PAO_TEXT = "VARCHAR2(90)",
    ALT_LANGUAGE_PAO_TEXT = "VARCHAR2(90)",
    USRN = "NUMBER",
    USRN_MATCH_INDICATOR = "VARCHAR2(1)",
    AREA_NAME = "VARCHAR2(40)",
    LEVEL_FIELD = "VARCHAR2(30)",
    OFFICIAL_FLAG = "VARCHAR2(1)",
    OS_ADDRESS_TOID = "VARCHAR2(20)",
    OS_ADDRESS_TOID_VERSION = "NUMBER",
    OS_ROADLINK_TOID = "VARCHAR2(20)",
    OS_ROADLINK_TOID_VERSION = "NUMBER",
    OS_TOPO_TOID = "VARCHAR2(20)",
    OS_TOPO_TOID_VERSION = "NUMBER",
    VOA_CT_RECORD = "NUMBER",
    VOA_NDR_RECORD = "NUMBER",
    STREET_DESCRIPTION = "VARCHAR2(101)",
    ALT_LANGUAGE_STREET_DESCRIPTOR = "VARCHAR2(110)",
    DEP_THOROUGHFARE = "VARCHAR2(80)",
    THOROUGHFARE = "VARCHAR2(80)",
    WELSH_DEP_THOROUGHFARE = "VARCHAR2(80)",
    WELSH_THOROUGHFARE = "VARCHAR2(80)",
    DOU_DEP_LOCALITY = "VARCHAR2(35)",
    DEP_LOCALITY = "VARCHAR2(35)",
    LOCALITY = "VARCHAR2(35)",
    WELSH_DEP_LOCALITY = "VARCHAR2(35)",
    WELSH_DOU_DEP_LOCALITY = "VARCHAR2(35)",
    TOWN_NAME = "VARCHAR2(30)",
    ADMINISTRATIVE_AREA = "VARCHAR2(30)",
    POST_TOWN = "VARCHAR2(35)",
    WELSH_POST_TOWN = "VARCHAR2(30)",
    POSTCODE = "VARCHAR2(8)",
    POSTCODE_LOCATOR = "VARCHAR2(8)",
    POSTCODE_TYPE = "VARCHAR2(1)",
    DELIVERY_POINT_SUFFIX = "VARCHAR2(2)",
    ADDRESSBASE_POSTAL = "VARCHAR2(1)",
    PO_BOX_NUMBER = "VARCHAR2(6)",
    WARD_CODE = "VARCHAR2(9)",
    PARISH_CODE = "VARCHAR2(9)",
    RM_START_DATE = "DATE",
    MULTI_OCC_COUNT = "NUMBER",
    VOA_NDR_P_DESC_CODE = "VARCHAR2(5)",
    VOA_NDR_SCAT_CODE = "VARCHAR2(4)",
    ALT_LANGUAGE = "VARCHAR2(3)",
    GEOMETRY = "MDSYS.SDO_GEOMETRY"
  )

  if (!append_existing) {
    # Create the table if needed - fields and types cribbed from
    # https://github.com/OrdnanceSurvey/AddressBase/blob/master/Loading_Scripts/Oracle/AddressBase_Plus_and_Islands/Oracle_AddressBase_Plus_createtable.sql
    DBI::dbCreateTable(
      conn = con,
      name = "ADDRESSBASE_PLUS",
      fields = fields
    )

  }

  # Loop over the csv files and append them
  for (data_file in data_files) {

    DBI::dbAppendTable(
      conn = con,
      name = "ADDRESSBASE_PLUS",
      value = data_file %>%
        read.csv(col.names = fields %>% setdiff(., "DATE")) %>%
        # Add the date column so we can stack different cuts of AddressBase Plus
        # in one table
        tibble::add_column(DATE = date, .before = 1)
    )

  }

}
