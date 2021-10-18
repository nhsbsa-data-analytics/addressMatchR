#' Download a data package from Ordinance Survey Data Hub
#'
#' Download a data package from Ordinance Survey Data Hub
#'
#' @param key OS Data Hub API key (with permissions for required data package.
#'   Default is the environment variable `Sys.getenv("OS_API_KEY")`.
#' @param data_package_id Data package ID
#' @param data_package_version Data package version. Default if `NA` is most
#'   recent.
#' @param destination Destination (folder if `unzip == TRUE` or file if
#'   `unzip == FALSE`).
#' @param unzip Whether or not to unzip the file. Default is `TRUE`.
#'
#' @examples
#'
#' @export
download_os_data_package <- function(
  key = Sys.getenv("OS_API_KEY"),
  data_package_id,
  data_package_version = NA,
  destination,
  unzip = TRUE
) {

  # Define the url for the API call
  base_endpoint <- "https://api.os.uk/downloads/v1/"
  api <- "dataPackages/"

  if (is.na(data_package_version)) {
    # If there is no package version defined then take the most recent

    # Get the response
    response <- jsonlite::fromJSON(
      paste0(
        base_endpoint,
        api,
        data_package_id,
        "?key=",
        "oxuBbtUDR3G7WBZfAGMY39UGaR4gI3n6"
      )
    )

    # Pull the versions dataframe
    versions_df <- response$versions

    # Get the most recent id as the version
    data_package_version <-
      versions_df[order(rev(as.Date(versions_df$createdOn))), "id"][1]

  }

  # Get the download link
  data_package_download_url <- jsonlite::fromJSON(
    paste0(
      base_endpoint,
      api,
      data_package_id,
      "/versions/",
      data_package_version,
      "?key=",
      "oxuBbtUDR3G7WBZfAGMY39UGaR4gI3n6"
    )
  )$downloads$url[1]

  if (unzip) {

    # If unzip then download to a temp file and then unzip into destination
    tmp <- tempfile()
    utils::download.file(
      url = data_package_download_url,
      destfile = tmp,
      mode = "wb"
    )
    utils::unzip(zipfile = tmp, exdir = destination)
    unlink(tmp)

  } else {

    # Otherwise download to the destination
    utils::download.file(
      url = data_package_download_url,
      destfile = destination
    )

  }

}

