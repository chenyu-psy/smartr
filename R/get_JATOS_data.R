#' Download data from JATOS
#'
#' This function downloads result data from a JATOS (Just Another Tool for Online Studies) server
#' using an API token and specified study ID and batch ID. It then unzips and extracts the relevant data,
#' including metadata, file names, and file contents. The function can download all data for specified
#' batch IDs or only download missing data based on comparison with local files.
#'
#' @param token A character string containing the API token for authentication. You can create a new token
#'        on JATOS. See: https://www.jatos.org/JATOS-API.html#personal-access-tokens
#' @param url A character string specifying the server address. The default value is the address of the lab server.
#' @param studyId A character string or a numeric containing the unique code of the study, available on JATOS.
#' @param batchId A character vector or numeric vector containing one or more unique codes for batch sessions in the study.
#'                If it is null, the batchId will be retrieved from the metadata.
#' @param dataPath A character string specifying the path used to save data. If NULL, data will be saved
#'        in a "JATOS_DATA" folder in the working directory.
#' @param attachments A logical value indicating whether to download attachments. Default is FALSE.
#' @param method A character string specifying the download method. Options are:
#'        - "all": Download all data for the specified batch IDs (default)
#'        - "missing": Only download data that is missing or has changed based on file size comparison
#'
#' @return A data frame containing metadata about the downloaded JATOS results.
#'
#' @importFrom rlang .data
#' @importFrom dplyr add_row filter mutate
#' @importFrom httr GET add_headers write_disk status_code
#' @importFrom jsonlite read_json
#' @importFrom stringr str_glue str_extract_all str_remove
#'
#' @examples
#' \dontrun{
#' # Download all data for a specific study and batch
#' data <- get_JATOS_data(
#'   token = "your_api_token",
#'   studyId = "your_study_id",
#'   batchId = c("batch_id_1", "batch_id_2"),
#'   dataPath = "./data/"
#' )
#'
#' # Download only missing data
#' missing_data <- get_JATOS_data(
#'   token = "your_api_token",
#'   studyId = "your_study_id",
#'   batchId = c("batch_id_1", "batch_id_2"),
#'   method = "missing"
#' )
#' }
#'
#' @export
#'
get_JATOS_data <- function(token,
                           url = "https://coglab.xyz/jatos/api/v1/results",
                           studyId,
                           batchId = NULL,
                           dataPath = NULL,
                           attachments = FALSE,
                           method = "all") {

  # Input validation
  if (missing(token) || !is.character(token) || length(token) != 1) {
    stop("Token must be a single character string")
  }

  if (!is.character(url) || length(url) != 1) {
    stop("URL must be a single character string")
  }

  if (missing(studyId) || !is.character(studyId) || length(studyId) != 1) {
    stop("Study ID must be a single character string")
  }

  if (!is.null(dataPath) && (!is.character(dataPath) || length(dataPath) != 1)) {
    stop("Data path must be NULL or a single character string")
  }

  if (!is.logical(attachments) || length(attachments) != 1) {
    stop("Attachments must be a logical value (TRUE or FALSE)")
  }

  if (!method %in% c("all", "missing")) {
    stop('Method must be either "all" or "missing"')
  }

  # Set the default data path and ensure it exists
  if (is.null(dataPath)) {
    dataPath <- "./JATOS_DATA/"
  }

  # Ensure the data path ends with a slash
  if (!endsWith(dataPath, "/")) {
    dataPath <- paste0(dataPath, "/")
  }

  # Create the directory if it doesn't exist
  if (!dir.exists(dataPath)) {
    dir.create(dataPath, recursive = TRUE)
  }

  # Determine the URL based on whether attachments should be downloaded
  data_url <- ifelse(!attachments, stringr::str_glue("{url}/data"), url)

  # Create the authorization header with the specified token
  headers <- c(`Authorization` = stringr::str_glue("Bearer {token}"))

  # Download the metadata from the JATOS server
  metadata_path <- file.path(dataPath, "metadata.json")
  metadata <- download_metadata(url, studyId, headers, metadata_path)

  # If batchId is NULL, get unique batch IDs from metadata
  batch_ids <- ifelse(is.null(batchId), unique(metaData$batchId), batchId)

  # Download the data based on the specified method
  if (method == "all") {
    download_all_data(data_url, headers, batch_ids, dataPath)
  } else if (method == "missing") {
    download_missing_data(metadata, batch_ids, data_url, headers, dataPath)
  }

  # Update the metadata after downloading
  updated_metadata <- read_metaData(metadata_path) %>%
    filter(.data$batchId %in% batch_ids)

  # Return the metadata
  return(updated_metadata)
}

#' Download metadata from JATOS
#'
#' Helper function to download metadata from the JATOS server.
#'
#' @param url Base URL for the JATOS API
#' @param study_id ID of the study
#' @param headers HTTP headers including authorization
#' @param metadata_path Path where metadata will be saved
#'
#' @return A data frame containing the metadata
#'
#' @keywords internal
download_metadata <- function(url, study_id, headers, metadata_path) {
  tryCatch({
    res <- httr::GET(
      url = stringr::str_glue("{url}/metadata?studyId={study_id}"),
      httr::add_headers(.headers = headers),
      httr::write_disk(metadata_path, overwrite = TRUE)
    )

    if (httr::status_code(res) == 200) {
      message(stringr::str_glue("Successfully downloaded metadata for study {study_id}."))
      metadata <- read_metaData(metadata_path)
      return(metadata)
    } else {
      warning(stringr::str_glue("Failed to download metadata. Status code: {httr::status_code(res)}"))
      return(NULL)
    }
  }, error = function(e) {
    warning(stringr::str_glue("Error during metadata download: {e$message}"))
    return(NULL)
  })
}

#' Download all data for specified batch IDs
#'
#' Helper function to download all data for the specified batch IDs.
#'
#' @param data_url URL for downloading data
#' @param headers HTTP headers including authorization
#' @param batch_ids Vector of batch IDs
#' @param data_path Path where data will be saved
#'
#' @keywords internal
download_all_data <- function(data_url, headers, batch_ids, data_path) {
  for (batch in batch_ids) {
    # Create the file paths for the zipped and unzipped data
    file_zip <- file.path(tempdir(), stringr::str_glue("JATOS_DATA_{batch}.jrzip"))
    file_unzip <- file.path(data_path, stringr::str_glue("JATOS_DATA_{batch}"))

    # Create the directory if it doesn't exist
    if (!dir.exists(file_unzip)) {
      dir.create(file_unzip, recursive = TRUE)
    }

    tryCatch({
      res <- httr::GET(
        url = stringr::str_glue("{data_url}?batchId={batch}"),
        httr::add_headers(.headers = headers),
        httr::write_disk(file_zip, overwrite = TRUE)
      )

      if (httr::status_code(res) == 200) {
        message(stringr::str_glue("Successfully downloaded data for batch {batch}."))
        filelist <- utils::unzip(file_zip, exdir = file_unzip, overwrite = TRUE)
      } else {
        warning(stringr::str_glue("Failed to download data for batch {batch}. Status code: {httr::status_code(res)}"))
      }
    }, error = function(e) {
      warning(stringr::str_glue("Error during download for batch {batch}: {e$message}"))
    })
  }
}



#' Download missing data based on metadata comparison
#'
#' Helper function to download only missing or changed data based on file size comparison.
#'
#' @param metadata Data frame containing metadata
#' @param batch_ids Vector of batch IDs
#' @param data_url URL for downloading data
#' @param headers HTTP headers including authorization
#' @param data_path Path where data will be saved
#'
#' @keywords internal
download_missing_data <- function(metadata, batch_ids, data_url, headers, data_path) {
  # Filter the metadata to obtain the participants whose data has not been downloaded
  filtered_metadata <- metadata %>%
    dplyr::filter(.data$batchId %in% batch_ids) %>%
    dplyr::mutate(localSize = ifelse(is.na(.data$file), 0, round(file.info(.data$file)$size / 1024, 2))) %>%
    dplyr::filter(.data$fileSize > 0.5, .data$fileSize > .data$localSize)

  # Download data for each participant
  for (i in seq_len(nrow(filtered_metadata))) {
    # Extract batch ID and result ID
    batch_id <- filtered_metadata$batchId[i]
    result_id <- filtered_metadata$resultId[i]

    # Create the file paths for the zipped and unzipped data
    file_zip <- file.path(tempdir(), stringr::str_glue("JATOS_DATA_{result_id}.jrzip"))
    file_unzip <- file.path(data_path, stringr::str_glue("JATOS_DATA_{batch_id}"))

    # Create the directory if it doesn't exist
    if (!dir.exists(file_unzip)) {
      dir.create(file_unzip, recursive = TRUE)
    }

    tryCatch({
      res <- httr::GET(
        url = stringr::str_glue("{data_url}?studyResultId={result_id}"),
        httr::add_headers(.headers = headers),
        httr::write_disk(file_zip, overwrite = TRUE)
      )

      if (httr::status_code(res) == 200) {
        filelist <- utils::unzip(file_zip, exdir = file_unzip, overwrite = TRUE)
      } else {
        warning(stringr::str_glue("Failed to download data for result {result_id}. Status code: {httr::status_code(res)}"))
      }
    }, error = function(e) {
      warning(stringr::str_glue("Error during download for result {result_id}: {e$message}"))
    })
  }
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
#' @param info A character vector of keys to extract from each data file.
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
extract_data_info <- function(metaData, info, warn = FALSE) {
  # Input validation
  if (!is.data.frame(metaData)) {
    stop("'metaData' must be a data frame or tibble")
  }

  if (!"file" %in% colnames(metaData)) {
    stop("'metaData' must contain a column named 'file'")
  }

  if (!is.character(info) || length(info) == 0) {
    stop("'info' must be a non-empty character vector")
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
        ~ safe_read_keys(.x, info, warn = warn)
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





