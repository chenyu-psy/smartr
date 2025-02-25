#' @title Compare Log Marginal Likelihood of Different Models
#'
#' @description
#' This function compares the log marginal likelihood (logml) of different models
#' based on saved samples.
#'
#' @param pars List. A named list of parameters to compare.
#' @param sample_path Character. The directory where the sample files are stored.
#' @param sample_prefix Character. The prefix of the sample filenames.
#'
#' @return A data frame containing log marginal likelihoods for different models.
#'
#' @importFrom dplyr mutate select across everything
#' @importFrom fs path
#' @importFrom stats median
#' @export
compare_logml <- function(pars, sample_path, sample_prefix) {

  # ---- Input Validation ----
  if (!is.list(pars) || length(names(pars)) == 0) stop("`pars` must be a named list.")
  if (!dir.exists(sample_path)) stop("`sample_path` does not exist: ", sample_path)
  if (!is.character(sample_prefix) || sample_prefix == "") stop("`sample_prefix` must be a non-empty character string.")

  # ---- Create Comparison Table ----
  table_model_info <- do.call(base::expand.grid, args = pars) %>%
    mutate(
      part_name = apply(across(everything()), 1, function(row)
        paste(names(row), row, sep = "", collapse = "_")
      ),
      sample_file = path(sample_path, paste0(sample_prefix, "_", .data$part_name, ".rds")),
      logml = NA_real_
    )

  # Check if all sample files exist before reading
  missing_files <- !file.exists(table_model_info$sample_file)
  if (any(missing_files)) {
    stop("The following sample files are missing:\n",
         paste(table_model_info$sample_file[missing_files], collapse = "\n"))
  }

  # ---- Extract Log Marginal Likelihood (logml) ----
  for (i in seq_len(nrow(table_model_info))) {

    # Read sample file with error handling
    sample_current <- tryCatch(
      readRDS(as.character(table_model_info$sample_file[i])),
      error = function(e) stop("Error reading file: ", table_model_info$sample_file[i])
    )

    # Extract and store logml
    table_model_info$logml[i] <- median(sample_current$logml, na.rm = TRUE)
  }

  # Remove unnecessary columns before returning
  table_model_info <- select(table_model_info, -c(part_name, sample_file))

  return(table_model_info)
}
