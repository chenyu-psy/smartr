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
#' @importFrom stringr str_glue str_extract_all
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
    file_zip <- str_glue("{tempdir()}/JATOS_DATA_{batch}.jrzip")
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
    metaData <- read_json(metaData_path)$data[[1]]$studyResults

    # Extract relevant metadata from the metadata list and store in a data frame
    info_table <- data.frame(
      studyId = numeric(),
      batchId = numeric(),
      resultId = numeric(),
      componentId = numeric(),
      studyState = character(),
      startTime = POSIXct(),
      endTime = POSIXct(),
      duration = numeric(),
      file = character(),
      fileSize = numeric()
    )

    # Add new columns for extracted information
    if (!is.null(extractInfo)) {
      for (col in extractInfo) {
        info_table[[col]] <- character()
      }
    }

    for (a in 1:length(metaData)) {
      # result data
      resultData = metaData[[a]]

      for (c in 1:length(resultData$componentResults)) {
        # component data
        compData = resultData$componentResults[[c]]

        # correct the start time and end time
        if (is.null(compData$startDate)) {
          startTime = NA
        } else {
          startTime = as.POSIXct(compData$startDate / 1000, origin = "1970-01-01")
        }

        if (is.null(compData$endDate)) {
          endTime = NA
          duration = NA
        } else {
          endTime = as.POSIXct(compData$endDate / 1000, origin = "1970-01-01")
          duration = round(diff(c(startTime, endTime), units = "mins")[[1]], 2)
        }

        # check if the file is in the file list
        file_path <- str_glue(
          "{file_unzip}/study_result_{resultData$id}/comp-result_{compData$id}/data.txt"
        )
        file_path <- ifelse(file_path %in% filelist, file_path, NA)

        # check the file size and participant ID
        if (!is.na(file_path)) {
          # Extract the information from the file
          if (!is.null(extractInfo)) {
            file <- suppressWarnings(readLines(file_path))

            extractList <- list()

            for (key in extractInfo) {
              # Use sprintf to build a dynamic regex pattern
              pattern <- sprintf('(?<="%s":")\\w+', key)

              # Extract the value associated with the key
              extractList[[key]] <- unique(str_extract_all(file, pattern)[[1]])
            }
          }

          fileSize <- file.info(file_path)$size / 1024

        } else {
          extractList <- list()
          fileSize <- NA
        }

        # Add the metadata to the data frame
        info_table <- info_table %>%
          add_row(
            studyId = studyId,
            batchId = batch,
            resultId = resultData$id,
            componentId = compData$id,
            studyState = compData$componentState,
            startTime = startTime,
            endTime = endTime,
            duration = duration,
            file = file_path,
            fileSize = fileSize
          )

        # Add extracted information to the data frame
        if (!is.null(extractInfo)) {
          for (key in extractInfo) {
            info_table[[key]][nrow(info_table)] <- paste(extractList[[key]], collapse = ",")
          }
        }


      }
    }
  }

  # Return the combined data frame
  return(info_table)
}
