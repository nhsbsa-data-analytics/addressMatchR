#' Upload AddressBase Plus to Oracle
#'
#' Upload AddressBase Plus to an Oracle DB. First download the data package for
#' AddressBase Plus using `addressMatchR::download_os_data_package()` and then
#' load it into the DB using this function.
#'
#' Using guidance from
#' https://www.ordnancesurvey.co.uk/documents/addressbase-products-getting-started-guide2.pdf
#'
#' @param con DB Connection.
#' @param path Path to .csv files
#' @param pattern Pattern of files to upload. Default is NULL (all files)
#'
#' @examples
#'
#' @export
upload_addressbase_plus_to_oracle <- function(con, path, pattern = NULL) {

  # Get a list of the data files
  data_files <- list.files(path = path, pattern = pattern, full.names = TRUE)

  # Define the fields and types cribbed from
  # https://github.com/OrdnanceSurvey/AddressBase/blob/master/Loading_Scripts/Oracle/AddressBase_Plus_and_Islands/Oracle_AddressBase_Plus_createtable.sql
  # NOTES:
  #   * Added BUILDING_NUMBER: "NUMBER"
  #   * Increased STREET_DESCRIPTION to "VARCHAR2(150)" from "VARCHAR2(101)"
  #   * Removed GEOMETRY as we aren't loading it
  sql_fields <- c(
    DATE = "DATE", # Added as we are stacking different cuts of AddressBase Plus
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
    BUILDING_NUMBER = "NUMBER", # missing
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
    STREET_DESCRIPTION = "VARCHAR2(150)", # increased from VARCHAR2(101)
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
    ALT_LANGUAGE = "VARCHAR2(3)"#,
    #GEOMETRY = "VARCHAR2(8)" # not loading this
  )

  if (!DBI::dbExistsTable(conn = con, name = "ADDRESSBASE_PLUS")) {
    # Create the table if needed
    DBI::dbCreateTable(
      conn = con,
      name = "ADDRESSBASE_PLUS",
      fields = sql_fields
    )
  }

  # Translate the field types for `{readr}` to speed up loading into R
  readr_fields <- sapply(
    X = sql_fields[2:length(sql_fields)],
    FUN = function (x) switch(x, NUMBER = "n", FLOAT = "d", DATE = "D", "c")
  )

  # Loop over the csv files and append them
  for (data_file in data_files) {

    # What date are we loading
    file_name <- tail(x = strsplit(data_file[1], "/")[[1]], n = 1)
    date <- substr(file_name, 22, 31)

    # Read the csv and append date column to front
    tmp_df <- data_file %>%
      readr::read_csv(
        col_names = names(readr_fields),
        col_types = paste(readr_fields, collapse = "")
      ) %>%
      # Add the date column so we can stack different cuts of AddressBase Plus
      # in one table
      tibble::add_column(DATE = date, .before = 1) %>%
      # Convert everything to character for loading into DB
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

    message(paste("Starting upload of", nrow(tmp_df), "rows from", file_name))

    # Upload file
    DBI::dbAppendTable(conn = con, name = "ADDRESSBASE_PLUS", value = tmp_df)

  }

}
