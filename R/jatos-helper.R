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
#' @param batchId A character vector or numeric vector containing one or more unique codes for batch sessions in the study.
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
#' # Download data for specified batches
#' data <- get_JATOS_data(
#'   token = "your_api_token",
#'   batchId = c("batch_id_1", "batch_id_2"),
#'   dataPath = "./data/",
#'   attachments = FALSE
#' )
#' }
#'
#' @export
#'
get_JATOS_data <- function(token,
                           url = "https://coglab.xyz/jatos/api/v1/results",
                           batchId,
                           dataPath = NULL,
                           attachments = FALSE) {

  # Input validation
  if (missing(token) || !is.character(token) || length(token) != 1) {
    stop("Token must be a single character string")
  }

  if (!is.character(url) || length(url) != 1) {
    stop("URL must be a single character string")
  }

  if (!is.null(dataPath) && (!is.character(dataPath) || length(dataPath) != 1)) {
    stop("Data path must be NULL or a single character string")
  }

  if (!is.logical(attachments) || length(attachments) != 1) {
    stop("Attachments must be a logical value (TRUE or FALSE)")
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
  multi_query <- paste0("batchId=", batchId, collapse = "&")
  metadata_path <- file.path(dataPath, "metadata.json")
  tryCatch({
    res <- httr::GET(
      url = stringr::str_glue("{url}/metadata?{multi_query}"),
      httr::add_headers(.headers = headers),
      httr::write_disk(metadata_path, overwrite = TRUE)
    )

    if (httr::status_code(res) == 200) {
      message(stringr::str_glue("Successfully downloaded metadata for batch IDs: {paste(batchId, collapse = ' ')}."))
    } else {
      stop(stringr::str_glue("Failed to download metadata. Status code: {httr::status_code(res)}"))
    }
  }, error = function(e) {
    stop(stringr::str_glue("Error during metadata download: {e$message}"))
  })

  # Read the metadata from the downloaded JSON file
  metadata <- read_metaData(metadata_path)

  # Download the data
  download_missing_data(metadata, data_url, headers, dataPath)

  # Update the metadata after downloading
  updated_metadata <- read_metaData(metadata_path)

  # Return the metadata
  return(updated_metadata)
}


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



#' Download missing data based on metadata comparison
#'
#' Helper function to download only missing or changed data based on file size comparison.
#'
#' @param metadata Data frame containing metadata
#' @param data_url URL for downloading data
#' @param headers HTTP headers including authorization
#' @param data_path Path where data will be saved
#'
#' @keywords internal
download_missing_data <- function(metadata, data_url, headers, data_path) {
  # Filter metadata to find data that needs downloading
  filtered_metadata <- metadata %>%
    # Use file size to check if the files need to be updated
    dplyr::mutate(
      local_size = ifelse(is.na(.data$file), 0, round(file.info(.data$file)$size / 1024, 2))
    ) %>%
    dplyr::filter(
      .data$fileSize > 0.5,
      .data$fileSize > .data$local_size
    )

  # Check if there are any files to download
  if (nrow(filtered_metadata) == 0) {
    message("There is no data needing to be downloaded.")
    return(invisible())
  }

  # Get unique batch IDs
  batch_ids <- unique(filtered_metadata$batchId)

  # Process each batch
  for (batch_id in batch_ids) {
    # Get result IDs for current batch
    result_ids <- filtered_metadata %>%
      dplyr::filter(.data$batchId == batch_id) %>%
      dplyr::pull(.data$resultId)

    # Define file paths
    zip_file_path <- file.path(tempdir(), stringr::str_glue("JATOS_DATA_{batch_id}.jrzip"))
    unzip_dir_path <- file.path(data_path, stringr::str_glue("JATOS_DATA_{batch_id}"))

    # Create directory if it doesn't exist
    if (!dir.exists(unzip_dir_path)) {
      dir.create(unzip_dir_path, recursive = TRUE)
    }

    # Determine download method based on number of result IDs
    use_result_ids_method <- length(result_ids) <= 20
    download_successful <- FALSE

    # Try downloading using result IDs if appropriate
    if (use_result_ids_method) {
      result_id_query <- paste0("studyResultId=", result_ids, collapse = "&")

      tryCatch({
        response <- httr::GET(
          url = stringr::str_glue("{data_url}?{result_id_query}"),
          httr::add_headers(.headers = headers),
          httr::write_disk(zip_file_path, overwrite = TRUE)
        )

        if (httr::status_code(response) == 200) {
          message(stringr::str_glue("Successfully downloaded missing data for batch {batch_id} using result IDs."))
          utils::unzip(zip_file_path, exdir = unzip_dir_path, overwrite = TRUE)
          download_successful <- TRUE
        } else if (httr::status_code(response) == 414) {
          message(stringr::str_glue("URI too long for batch {batch_id}. Retrying with batch ID..."))
        } else {
          warning(stringr::str_glue(
            "Failed to download data using result IDs for batch {batch_id}. ",
            "Status code: {httr::status_code(response)}"
          ))
        }
      }, error = function(e) {
        warning(stringr::str_glue(
          "Error during download using result IDs for batch {batch_id}: {e$message}"
        ))
      })
    }

    # Fallback to batch ID method if result IDs method failed or wasn't used
    if (!download_successful) {
      tryCatch({
        response <- httr::GET(
          url = stringr::str_glue("{data_url}?batchId={batch_id}"),
          httr::add_headers(.headers = headers),
          httr::write_disk(zip_file_path, overwrite = TRUE)
        )

        if (httr::status_code(response) == 200) {
          message(stringr::str_glue("Successfully downloaded missing data for batch {batch_id} using batch ID."))
          utils::unzip(zip_file_path, exdir = unzip_dir_path, overwrite = TRUE)
        } else {
          warning(stringr::str_glue(
            "Failed to download data for batch {batch_id} using batch ID. ",
            "Status code: {httr::status_code(response)}"
          ))
        }
      }, error = function(e) {
        warning(stringr::str_glue(
          "Error during download for batch {batch_id} using batch ID: {e$message}"
        ))
      })
    }
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


#' Read and combine JSON files into a data frame
#'
#' @description
#' This function reads multiple JSON files, combines them into a single data structure,
#' and returns a unified data frame. It handles file reading, text manipulation, and JSON parsing.
#' Non-existent files and NA values are automatically removed with warnings.
#'
#' @param files A character vector of file paths to JSON files. NA values and non-existent files are automatically removed.
#'
#' @return A data frame containing the combined data from all JSON files.
#'
#' @examples
#' \dontrun{
#' files <- c("data1.json", "data2.json")
#' combined_data <- read_json_data(files)
#' }
#'
#' @importFrom purrr map_vec
#' @importFrom stringr str_c
#' @importFrom jsonlite fromJSON
#' @importFrom brio read_file
#'
#' @export
read_json_data <- function(files) {
  # Input validation
  if (!is.character(files) & !is.factor(files)) {
    stop("'files' must be a vector of characters file paths", call. = FALSE)
  }

  # Remove NA values from the files vector
  if (any(is.na(files))) {
    na_count <- sum(is.na(files))
    warning(sprintf("Removed %d NA value(s) from the files vector", na_count), call. = FALSE)
    files <- files[!is.na(files)]
  }

  # Check if any files remain after removing NAs
  if (length(files) == 0) {
    warning("No valid files provided after removing NA values", call. = FALSE)
    return(data.frame())
  }

  # Check if all files exist and remove non-existent files
  files_exist <- file.exists(files)
  if (!all(files_exist)) {
    missing_files <- files[!files_exist]
    warning(
      sprintf("Removed %d non-existent file(s): %s",
              length(missing_files),
              paste(missing_files, collapse = ", ")),
      call. = FALSE
    )
    files <- files[files_exist]
  }

  # Check if any files remain after removing non-existent files
  if (length(files) == 0) {
    warning("No valid files remain after removing non-existent files", call. = FALSE)
    return(data.frame())
  }

  # Read all files with progress indicator
  tryCatch({
    combined_text <- purrr::map_vec(
      files,
      .f = brio::read_file,
      .progress = TRUE
    )

    # Combine all text into a single string
    single_text <- stringr::str_c(combined_text, collapse = "")

    # Modify the text to ensure proper JSON format when combining multiple arrays
    # This replaces the pattern }][ with },{ to merge adjacent JSON arrays
    modified_text <- gsub(
      pattern = "\\}\\]\\s{0,}\\[\\{",
      replacement = "\\},\\{",
      x = single_text,
      perl = TRUE
    )

    # Parse the modified JSON text into a data frame
    data <- jsonlite::fromJSON(modified_text)

    return(data)
  },
  error = function(e) {
    stop("Error processing JSON files: ", e$message, call. = FALSE)
  })
}
