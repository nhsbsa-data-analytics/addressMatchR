if(getRversion() >= "2.15.1") utils::globalVariables(
  c(
    # Oracle SQL
    "INSTR",
    "LENGTH",
    "REGEXP_LIKE",
    "REGEXP_REPLACE",
    "REPLACE",
    "SUBSTR",
    "UTL_MATCH.JARO_WINKLER",

    # AddressBase table columns
    "DPA_SINGLE_LINE_ADDRESS",
    "DEPARTMENT_NAME",
    "RM_ORGANISATION_NAME",
    "SUB_BUILDING_NAME",
    "BUILDING_NAME",
    "BUILDING_NUMBER",
    "PO_BOX_NUMBER",
    "DEP_THOROUGHFARE",
    "THOROUGHFARE",
    "DOU_DEP_LOCALITY",
    "DEP_LOCALITY",
    "POST_TOWN",
    "POSTCODE",
    "GEO_SINGLE_LINE_ADDRESS",
    "LA_ORGANISATION",
    "SAO_TEXT",
    "SAO_START_NUMBER",
    "SAO_START_SUFFIX",
    "SAO_DASH",
    "SAO_END_NUMBER",
    "SAO_END_SUFFIX",
    "PAO_TEXT",
    "PAO_START_NUMBER",
    "PAO_START_SUFFIX",
    "PAO_DASH",
    "PAO_END_NUMBER",
    "PAO_END_SUFFIX",
    "STREET_DESCRIPTION",
    "LOCALITY",
    "TOWN_NAME",
    "POSTCODE_LOCATOR",

    # Created columns that do not work with .data pronoun
    "SCORE"
  )
)
