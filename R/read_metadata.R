#' Read the files information from the metadata file
#'
#' This function reads a metadata JSON file and extracts relevant study information
#'
#' @param path A string specifying the directory containing the metadata file.
#' @return A data frame containing study results.
#'
#' @importFrom rlang .data
#' @importFrom stringr str_glue str_detect
#' @importFrom jsonlite read_json
#' @importFrom dplyr bind_rows
#' @importFrom purrr map_dfr
#'
#' @export
#'
read_metaData <- function(path) {

  # Define the path for the metadata and the data folder
  if (stringr::str_detect(path, "metadata.json")) {
    metaData_path <- path
    data_path <- stringr::str_remove(path, "metadata.json")
  } else {
    metaData_path <- stringr::str_glue("{path}metadata.json")
    data_path <- path
  }

  # Check if metadata file exists
  if (!file.exists(metaData_path)) {
    stop("Metadata file not found: ", metaData_path)
  }

  # Read metadata file
  metaData <- tryCatch({
    jsonlite::read_json(metaData_path)
  }, error = function(e) {
    stop("Error reading metadata file: ", e$message)
  })

  # Extract study results
  studyResults <- metaData$data[[1]]$studyResults
  if (is.null(studyResults)) {
    stop("No study results found in metadata.")
  }

  # Helper function to process each study result
  process_component <- function(resultData, path) {

    if (is.null(resultData$componentResults)) return(NULL)

    purrr::map_dfr(resultData$componentResults, function(compData) {

      # Convert timestamps to POSIXct (readable date format)
      startTime <- if (!is.null(compData$startDate)) {
        as.POSIXct(compData$startDate / 1000, origin = "1970-01-01", tz = "UTC")
      } else {
        NA
      }

      endTime <- if (!is.null(compData$endDate)) {
        as.POSIXct(compData$endDate / 1000, origin = "1970-01-01", tz = "UTC")
      } else {
        NA
      }

      # Calculate duration in minutes
      duration <- if (!is.na(startTime) & !is.na(endTime)) {
        as.numeric(difftime(endTime, startTime, units = "mins"))
      } else {
        NA
      }



      # Construct file path
      file_path <- stringr::str_glue("{data_path}/study_result_{resultData$id}/comp-result_{compData$id}/data.txt")
      file_exists <- file.exists(file_path)

      # Attachment
      attach_path <- stringr::str_glue("{data_path}/study_result_{resultData$id}/comp-result_{compData$id}/files")
      attachment_exists <- dir.exists(attach_path)

      # Get file size
      fileSize <- if (file_exists) file.info(file_path)$size / 1024 else NA

      # Return as a data frame row
      data.frame(
        resultId = resultData$id,
        componentId = compData$id,
        studyState = compData$componentState,
        startTime = startTime,
        endTime = endTime,
        duration = round(duration,2),
        file = if (file_exists) file_path else NA,
        fileSize = round(fileSize,2),
        attachments = if (attachment_exists) attach_path else NA,
        stringsAsFactors = FALSE
      )
    })
  }

  # Process all study results
  info_table <- purrr::map_dfr(studyResults, process_component, path = path)

  return(info_table)
}
