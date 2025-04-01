#' Relabel Factor or Character Vectors
#'
#' This function relabels the values of a factor or character vector based on the provided labels.
#'
#' @param x A factor or character vector to be relabeled.
#' @param labels A named vector or a named list specifying the new labels. Each name in `labels` must correspond to a unique value in `x`.
#' @param factor Logical. If `TRUE` (default), the output is returned as a factor; otherwise, as a character vector.
#' @return A factor or character vector with relabeled values.
#' @examples
#' relabel(c("low", "medium", "high"), list("low" = "L", "medium" = "M", "high" = "H"))
#' relabel(factor(c("low", "medium", "high")), c("low" = "L", "medium" = "M", "high" = "H"))
#' relabel(c("apple", "banana", "cherry"), c("apple" = "A", "banana" = "B", "cherry" = "C"), factor = FALSE)
#' @export
#'
relabel <- function(x, labels, factor = TRUE) {

  # Ensure `x` is a factor, or character vector
  if (!is.factor(x) && !is.character(x)) {
    stop("`x` must be a factor, or character vector.")
  }

  # Ensure `labels` is a vector or a list that can be converted to a vector
  if (!is.vector(labels) && !is.list(labels) || is.vector(labels) && is.null(names(labels))) {
    stop("`labels` must be a vector with names or a list.")
  }


  # If `labels` is a list, ensure each element has only one value
  if (is.list(labels) && any(lengths(labels) != 1)) {
    stop("Each element of `labels` must have exactly one value.")
  }

  # Convert named list to named vector if necessary
  if (is.list(labels)) {
    labels <- unlist(labels, use.names = TRUE)
  }

  # Convert factors to characters for processing
  if (is.factor(x)) {
    x <- as.character(x)
  }

  # Check if all unique non-NA values in `x` are present in `labels`
  if (!all(unique(x) %in% names(labels))) {
    missing_vals <- setdiff(unique(x), names(labels))

    if (any(is.na(missing_vals))) {
      missing_vals <- missing_vals[!is.na(missing_vals)] # remove NA values from `missing_vals`
      warning("There is an NA value in `x` that will not be relabeled.")
    }

    if (length(missing_vals) > 0) {
      warning(paste("The following values in `x` are missing from `labels` and will not be relabeled: ", paste(missing_vals, collapse = ", "), ".", sep = ""))

      # Add missing values to `labels` with the same names
      labels <- c(labels, setNames(as.character(missing_vals), as.character(missing_vals)))
    }

  }


  # If `x` is character, ensure `labels` has names
  if (is.character(x) && is.null(names(labels))) {
    stop("If `x` is a character vector, `labels` must have names corresponding to unique values in `x`.")
  }

  # Ensure `x` is a character vector if `factor = FALSE`
  if (is.factor(x)) {
    x <- as.character(x)
  }

  # Relabel using factors or direct mapping
  if (factor) {
    return(factor(x, levels = names(labels), labels = labels))
  } else {
    return(unname(labels[x]))
  }
}




#' Read the key information from a data file
#'
#' This function extracts key-value pairs from a text-based data file.
#'
#' @param file A string specifying the path to the data file.
#' @param keys A character vector of keys to extract from the file.
#' @param warn Logical. If `TRUE`, warnings are displayed when issues occur.
#' @return A named list where each key contains its extracted values, or NA if an issue occurs.
#'
#' @importFrom stringr str_extract_all
#' @importFrom purrr map
#'
#' @export
#'
read_keys_info <- function(file = NULL, keys = NULL, warn = TRUE) {


  # Check if the file exists
  if (is.null(file) || !file.exists(file)) {
    if (warn) message("The specified file does not exist: ", file)
    return(NA)
  }

  # Check if keys are provided and are character type
  if (is.null(keys)) {
    message("No keys provided.")
    return(NA)
  } else if (!is.character(keys)) {
    message("Keys must be a character vector.")
    return(NA)
  }


  # Define function to extract values for a given key
  extract_values <- function(key, file) {

    # Read file content
    file_content <- suppressMessages(readLines(file, warn = FALSE))  # Suppress warnings for incomplete final line

    # Collapse lines into a single string (important for multi-line JSON)
    file_content <- paste(file_content, collapse = " ")

    # Adjust regex pattern to robustly capture JSON key-value pairs
    pattern <- sprintf('"%s"\\s*:\\s*(?:"([^"]+)"|([0-9]+))', key)

    # Extract matches
    matches <- stringr::str_match_all(file_content, pattern)[[1]]

    # If matches are found, extract the non-empty column (either string or number)
    if (nrow(matches) > 0) {
      extracted_values <- ifelse(is.na(matches[,2]), matches[,3], matches[,2])
      return(unique(extracted_values))
    } else {
      return(NA)
    }
  }


  # Apply extraction function to all keys using purrr::map()
  extractList <- purrr::map(setNames(keys, keys), ~ extract_values(.x, file))

  return(extractList)
}



