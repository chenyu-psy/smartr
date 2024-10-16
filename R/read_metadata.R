#' Read the files information from the metadata file
#'
#' This function reads the metadata file and extracts the relevant information
#'
#' @param path The path to the directory containing the metadata file
#' @return data.frame
#'
#' @importFrom rlang .data
#'
#' @export
#'
read_metaData <- function(path) {

    # Read the metadata from the last file in the list (assuming it is in JSON format)
    metaData_path <- stringr::str_glue("{path}/metadata.json")
    metaData <- jsonlite::read_json(metaData_path)$data[[1]]$studyResults

    # Extract relevant metadata from the metadata list and store in a data frame
    info_table <- data.frame(
      resultId = numeric(0),
      componentId = numeric(0),
      studyState = character(0),
      startTime = as.POSIXct(numeric(0)),  # Initialize as empty POSIXct
      endTime = as.POSIXct(numeric(0)),    # Initialize as empty POSIXct
      duration = numeric(0),
      file = character(0),
      fileSize = numeric(0),
      stringsAsFactors = FALSE             # Ensure no automatic conversion to factors
    )

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

        # check if the file exists or not
        file_path <- stringr::str_glue(
          "{path}/study_result_{resultData$id}/comp-result_{compData$id}/data.txt"
        )
        file_path <- ifelse(file.exists(file_path), file_path, NA)

        # check the file size and participant ID
        if (!is.na(file_path)) {
          fileSize <- file.info(file_path)$size / 1024
        } else {
          fileSize <- NA
        }

        # Add the metadata to the data frame
        info_table <- info_table %>%
          dplyr::add_row(
            resultId = resultData$id,
            componentId = compData$id,
            studyState = compData$componentState,
            startTime = startTime,
            endTime = endTime,
            duration = duration,
            file = file_path,
            fileSize = fileSize
          )

      }
    }

  # Return the combined data frame
  return(info_table)
}
