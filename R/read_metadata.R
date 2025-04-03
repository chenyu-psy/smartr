#' Read the files information from the metadata file
#'
#' This function reads a metadata JSON file and extracts relevant study information
#'
#' @param metaData_path A string specifying the directory containing the metadata file.
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
read_metaData <- function(metaData_path) {

  # Correct the path if it is a directory
  if (dir.exists(metaData_path)) {
    metaData_path <- stringr::str_glue("{metaData_path}/metadata.json")
    data_path <- metaData_path
  } else {
    data_path <- str_remove(metaData_path, "metadata.json")
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
  process_component <- function(resultData, data_path) {

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

      # Path to save the data file
      file_path <- stringr::str_glue("{data_path}JATOS_DATA_{resultData$batchId}/")

      # Construct file path
      data_file <- stringr::str_glue("{file_path}study_result_{resultData$id}/comp-result_{compData$id}/data.txt")
      file_exists <- file.exists(data_file)

      # Attachment
      attach_folder <- stringr::str_glue("{file_path}study_result_{resultData$id}/comp-result_{compData$id}/files")
      attachment_exists <- dir.exists(attach_folder)

      # Return as a data frame row
      data.frame(
        batchId = resultData$batchId,
        resultId = resultData$id,
        componentId = compData$id,
        studyState = compData$componentState,
        startTime = startTime,
        endTime = endTime,
        duration = round(duration,2),
        file = if (file_exists) data_file else NA,
        fileSize = round(compData$data$size/1024, 2),
        attachments = if (attachment_exists) attach_folder else NA,
        stringsAsFactors = FALSE
      )
    })
  }

  # Process all study results
  info_table <- purrr::map_dfr(studyResults, process_component, data_path = data_path)

  return(info_table)
}
