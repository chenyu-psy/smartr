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
#' @importFrom dplyr add_row filter mutate
#' @importFrom httr GET add_headers write_disk status_code
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
                           extractInfo = NULL,
                           attachments = FALSE) {

  # Set the default data path
  if (is.null(dataPath))
    dataPath = "./JATOS_DATA/"

  # Check whether the attachments should be downloaded
  if (!attachments) {
    data_url = str_glue("{url}/data")
  }

  # Rename the variable `batchId` to avoid conflicts with the function argument
  batchId_list <- batchId

  # Create the authorization header with the specified token
  headers = c(`Authorization` = str_glue("Bearer {token}"))

  # Download the metadata from the JATOS server and read it
  metaData_path <- str_glue("{dataPath}metadata.json")

  tryCatch({
    res <- GET(
      url = str_glue("{url}/metadata?studyId={studyId}"),
      add_headers(.headers = headers),
      write_disk(metaData_path, overwrite = TRUE)
    )

    if (status_code(res) == 200) {
      message(str_glue("Successfully downloaded meta data for study {studyId}."))
      metaData = read_metaData(metaData_path)
    } else {
      warning(str_glue("Failed to download meta data. Status code: {status_code(res)}"))
    }
  }, error = function(e) {
    warning(str_glue("Error during download: {e$message}"))
  })

  # Download the data from the JATOS server (using batchID)
  for (batch in batchId) {
    # Create the file paths for the zipped and unzipped data
    file_zip <- str_glue("{tempdir()}JATOS_DATA_{batch}.jrzip")
    file_unzip <- str_glue("{dataPath}JATOS_DATA_{batch}")

    tryCatch({
      res <- GET(
        url = str_glue("{data_url}?batchId={batch}"),
        add_headers(.headers = headers),
        write_disk(file_zip, overwrite = TRUE)
      )

      if (status_code(res) == 200) {
        message(str_glue("Successfully downloaded data for batch {batch}."))
        filelist = utils::unzip(file_zip, exdir = file_unzip, overwrite = TRUE)
      } else {
        warning(str_glue("Failed to download data. Status code: {status_code(res)}"))
      }
    }, error = function(e) {
      warning(str_glue("Error during download: {e$message}"))
    })

  }

  # Update the file path to the metadata
  metaData <- read_metaData(metaData_path)

  # Read the key information from the data file
  if (!is.null(extractInfo)) {
    info_table <- extract_data_info(metaData, extractInfo)
  } else {
    info_table <- metaData
  }

  # Return the combined data frame
  return(info_table)
}


#' Extract Key Information from Data Files
#'
#' @description
#' This module provides functions for extracting key-value pairs from text-based data files.
#' It includes two main functions:
#' \itemize{
#'   \item \code{read_keys_info}: Extracts key-value pairs from a single file
#'   \item \code{extract_data_info}: Processes multiple files and returns results as a tibble
#' }
#'
#' @name data_extraction
NULL

#' Read Key Information from a Data File
#'
#' @description
#' This function extracts key-value pairs from a text-based data file.
#'
#' @param file A string specifying the path to the data file.
#' @param keys A character vector of keys to extract from the file.
#' @param warn Logical. If `TRUE` (default), warnings are displayed when issues occur.
#'
#' @return A named list where each key contains its extracted values, or NA if an issue occurs.
#'
#' @examples
#' \dontrun{
#'   # Extract author and date from a JSON file
#'   read_keys_info("data/metadata.json", c("author", "date"))
#' }
#'
#' @importFrom stringr str_match_all
#' @importFrom purrr map
#'
#' @export
read_keys_info <- function(file = NULL, keys = NULL, warn = TRUE) {
  # Input validation
  if (is.null(file)) {
    if (warn) message("No file specified.")
    return(NA)
  }

  # Check if the path exists and is a regular file
  if (!file.exists(file)) {
    if (warn) message("The specified path does not exist: ", file)
    return(NA)
  }

  if (dir.exists(file) && !file.info(file)$isdir) {
    if (warn) message("The specified path is a directory, not a file: ", file)
    return(NA)
  }

  if (is.null(keys)) {
    if (warn) message("No keys provided.")
    return(NA)
  } else if (!is.character(keys)) {
    if (warn) message("Keys must be a character vector.")
    return(NA)
  }

  # Define helper function to extract values for a given key
  extract_values <- function(key, file) {
    tryCatch({
      # Read file content
      file_content <- suppressMessages(readLines(file, warn = FALSE))

      # Collapse lines into a single string (important for multi-line JSON)
      file_content <- paste(file_content, collapse = " ")

      # Adjust regex pattern to robustly capture JSON key-value pairs
      pattern <- sprintf('"%s"\\s*:\\s*(?:"([^"]+)"|([0-9]+))', key)

      # Extract matches
      matches <- stringr::str_match_all(file_content, pattern)[[1]]

      # If matches are found, extract the non-empty column (either string or number)
      if (nrow(matches) > 0) {
        extracted_values <- ifelse(is.na(matches[, 2]), matches[, 3], matches[, 2])
        return(unique(extracted_values))
      } else {
        return(NA)
      }
    },
    error = function(e) {
      if (warn) message("Error extracting key '", key, "' from file '", file, "': ", e$message)
      return(NA)
    })
  }

  # Apply extraction function to all keys using purrr::map()
  extractList <- purrr::map(setNames(keys, keys), ~ extract_values(.x, file))

  return(extractList)
}

#' Extract Information from Multiple Data Files
#'
#' @description
#' This function reads specified keys from multiple data files and returns them as a tibble with
#' the extracted information added as columns.
#'
#' @param metaData A data frame or tibble containing at least a column named 'file' with paths to data files.
#' @param extractInfo A character vector of keys to extract from each data file.
#' @param warn Logical. Whether to show warnings during extraction. Default is FALSE.
#'
#' @return A tibble containing the original metadata plus columns for each extracted key.
#'
#' @examples
#' \dontrun{
#'   # Assuming metaData is a tibble with a 'file' column containing file paths
#'   result <- extract_data_info(metaData, c("author", "date", "version"))
#' }
#'
#' @importFrom dplyr mutate select rename_with
#' @importFrom purrr map safely
#' @importFrom tidyr unnest_wider
#' @importFrom stringr str_remove
#' @importFrom rlang .data
#'
#' @export
extract_data_info <- function(metaData, extractInfo, warn = FALSE) {
  # Input validation
  if (!is.data.frame(metaData)) {
    stop("'metaData' must be a data frame or tibble")
  }

  if (!"file" %in% colnames(metaData)) {
    stop("'metaData' must contain a column named 'file'")
  }

  if (!is.character(extractInfo) || length(extractInfo) == 0) {
    stop("'extractInfo' must be a non-empty character vector")
  }

  if (!is.logical(warn)) {
    stop("'warn' must be a logical value (TRUE or FALSE)")
  }

  # Create a safe version of read_keys_info that won't fail
  safe_read_keys <- purrr::safely(read_keys_info, otherwise = NA)

  # Extract information from files
  info_table <- metaData %>%
    dplyr::mutate(
      # Process each file and extract the requested information
      # Use safely to prevent errors from stopping the entire process
      extracted_result = purrr::map(
        .data$file,
        ~ safe_read_keys(.x, extractInfo, warn = warn)
      ),
      # Extract the result component from the safely output
      extracted = purrr::map(.data$extracted_result, "result")
    ) %>%
    # Remove the intermediate column
    dplyr::select(-extracted_result) %>%
    # Expand list columns into separate columns
    tidyr::unnest_wider(extracted, names_sep = "_")

  # Remove the original extracted column if it exists after unnesting
  if ("extracted_1" %in% names(info_table)) {
    info_table <- dplyr::select(info_table, -"extracted_1")
  }

  # Rename the columns to remove the "extracted_" prefix
  info_table <- dplyr::rename_with(
    info_table,
    ~ stringr::str_remove(.x, "^extracted_"),
    dplyr::starts_with("extracted_")
  )

  return(info_table)
}





