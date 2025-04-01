#' Download data from JATOS
#'
#' This function downloads result data from a JATOS server using an API token and specified study ID and batch ID.
#' It then unzips and extracts the relevant data, including metadata, file names, and file contents.
#' The function returns a data frame with the relevant data.
#'
#' @param token An API token, you can create a new one on JATOS. Details see: https://www.jatos.org/JATOS-API.html#personal-access-tokens
#' @param url Server address. The default value is the address of our lab server.
#' @param studyId A unique code of study, you can find it on JATOS
#' @param batchId A unique code of a batch session in the study.
#' @param dataPath A path used to save data. If NUll, data will be saved in working directory.
#' @param extractInfo A list of keys to extract from the data file. Default is NULL.
#'
#' @return data.frame
#'
#' @importFrom rlang .data
#' @importFrom dplyr add_row
#' @importFrom httr GET add_headers write_disk
#' @importFrom jsonlite read_json
#' @importFrom stringr str_glue str_extract_all str_remove
#'
#' @export
#'
get_JATOS_data <- function(token,
                           url = "https://coglab.xyz/jatos/api/v1/results",
                           studyId,
                           batchId,
                           dataPath = NULL,
                           extractInfo = NULL) {
  if (is.null(dataPath))
    dataPath = "./" # Set the default data path

  # Create the authorization header with the specified token
  headers = c(`Authorization` = str_glue("Bearer {token}"))

  # Read the data from the JATOS server
  for (batch in batchId) {
    # Create the file paths for the zipped and unzipped data
    file_zip <- str_glue("{tempdir()}JATOS_DATA_{batch}.jrzip")
    file_unzip <- str_glue("{dataPath}JATOS_DATA_{batch}")

    # Send an HTTP GET request to the specified URL, passing in the UUID and batch ID
    # Also pass in the authorization header and write the response to a temporary file
    res <- GET(
      url = str_glue("{url}?studyId={studyId}&batchId={batch}"),
      add_headers(.headers = headers),
      write_disk(file_zip, overwrite = TRUE)
    )

    # Unzip the downloaded file and extract the file names into a list
    filelist = utils::unzip(file_zip, exdir = file_unzip, overwrite = TRUE)

    # Read the metadata from the last file in the list (assuming it is in JSON format)
    metaData_path <- str_glue("{file_unzip}/metadata.json")
    metaData <- read_metaData(metaData_path)

    # Read the key information from the data file
    if (!is.null(extractInfo)) {
      info_table <- metaData %>%
        dplyr::mutate(extracted = purrr::map(file, ~ read_keys_info(.x, extractInfo, warn=F))) %>%
        tidyr::unnest_wider(extracted, names_sep = "_")   # Expand list columns into separate columns

      # Remove the original extracted column
      if ("extracted_1" %in% names(info_table)) {
        info_table <- info_table %>%
          dplyr::select(-extracted_1)  # Remove the original extracted column
      }

      # rename the columns
      info_table <- info_table %>%
        rename_with(~ str_remove(.x, "^extracted_"), starts_with("extracted_"))

    } else {
      info_table <- metaData
    }
  }

  # Return the combined data frame
  return(info_table)
}
