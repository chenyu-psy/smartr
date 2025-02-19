#' Perform Parallel Model Comparisons
#'
#' @description
#' This function compares multiple models using parallel execution. It fits models, runs bridge sampling,
#' and calculates Bayes Factors.
#'
#' @param fun Function. The function used to fit models (supports `brms` and `bmm` models).
#' @param pars List. A named list of parameter values to compare.
#' @param form_fun Function. A function to generate model formulas (default is `NULL`).
#' @param model_fun Function. A function to generate models (supports `bmm` models only, default is `NULL`).
#' @param prior_fun Function. A function to generate prior distributions (default is `NULL`).
#' @param args List. Additional arguments passed to the model-fitting function.
#' @param sampler_args List. Arguments for `bridge_sampler` (default: `{cores = 1, repetition = 10, maxiter = 1000}`).
#' @param model_name Character. The base name for models (default: `"Model"`).
#' @param model_path Character. The directory to store models.
#' @param sample_path Character. The directory to store bridge sampling results.
#' @param maxCore Integer. The maximum number of cores to use (default is system limit).
#' @param sample_check Logical. If `TRUE`, checks whether samples exist before running (default: `TRUE`).
#'
#' @return A data frame containing the model comparison table.
#'
#' @import dplyr
#' @import fs
#' @importFrom parallel parallel
#' @importFrom purrr list_assign
#' @export
parallel_model_comparison <- function(
    fun,
    pars,
    form_fun = NULL,
    model_fun = NULL,
    prior_fun = NULL,
    args,
    sampler_args = list(cores = 1, repetition = 10, maxiter = 1000),
    model_name = "Model",
    model_path,
    sample_path,
    maxCore = NULL,
    sample_check = TRUE) {

  # ---- Input Validation ----
  if (!is.function(fun)) stop("`fun` must be a valid function.")
  if (!is.list(pars) || length(names(pars)) == 0) stop("`pars` must be a named list.")
  if (!dir.exists(model_path)) stop("`model_path` does not exist: ", model_path)
  if (!dir.exists(sample_path)) stop("`sample_path` does not exist: ", sample_path)

  # Generate sample names
  sample_name <- if (grepl("Model|model", model_name, ignore.case = TRUE)) {
    gsub("Model", "Sample", model_name, ignore.case = TRUE)
  } else {
    paste("Sample", model_name, sep = "_")
  }

  # Temporary file for storing model info
  file_model_table <- tempfile(fileext = ".rds")

  # ---- Create Model Table ----
  table_model_info <- do.call(base::expand.grid, args = pars) %>%
    mutate(
      part_name = apply(across(everything()), 1, function(row)
        paste(names(row), row, sep = "", collapse = "_")
      ),
      model_name = paste0(model_name, "_", part_name),
      sample_name = paste0(sample_name, "_", part_name),
      comparison = NA,
      BF = NA,
      logBF = NA,
      reliability = NA,
      best_model = NA,
      model_file = path(model_path, paste0(model_name, ".rds")),
      sample_file = path(sample_path, paste0(sample_name, ".rds")),
      model_ck = as.integer(file.exists(model_file)),
      sample_ck = as.integer(file.exists(sample_file))
    ) %>%
    select(-part_name)

  saveRDS(table_model_info, file = file_model_table)

  # ---- Run Models and Bridge Sampling in Parallel ----
  for (i in seq_len(nrow(table_model_info))) {

    # Reload the model table
    table_model_info <- readRDS(file_model_table)

    # Skip if the sample already exists
    if (sample_check && table_model_info$sample_ck[i] == 1) next

    # Model label
    current_model_label <- table_model_info$model_name[i]

    # ---- Prepare Model Arguments ----
    # Copy arguments
    model_args = args

    # get the current parameter values
    char_table <- table_model_info
    char_table[] <- lapply(char_table, function(x) if (is.factor(x)) as.character(x) else x)
    par_values <- as.list(char_table[i, names(pars)])

    # Update model arguments
    if (is.function(form_fun)) model_args$formula <-  do.call(form_fun, args = par_values)
    if (is.function(model_fun)) model_args$model <- do.call(model_fun, args = par_values)
    if (is.function(prior_fun)) model_args$prior <- do.call(prior_fun, args = par_values)
    model_args[["file"]] <- gsub(".rds", "", table_model_info$model_file[i])

    # ---- Run Model ----
    smart_runFun(
      fun = fun,
      args = model_args,
      untilFinished = NULL,
      maxCore = maxCore,
      priority = 1,
      name = current_model_label
    )

    # Get model index
    job_info <- view_job()
    index_model <- which(job_info$name == current_model_label)

    # ---- Run Bridge Sampling ----
    smart_runFun(
      fun = function(table_path, iModel, sampler_args) {
        table_model_info <- readRDS(table_path)

        # File paths
        model_file <- table_model_info$model_file[iModel]
        sample_file <- table_model_info$sample_file[iModel]

        if (!file.exists(sample_file)) {
          model <- readRDS(model_file)
          sampler_args$samples <- model
          sample <- do.call(brms::bridge_sampler, sampler_args)
          saveRDS(sample, sample_file)
        }
      },
      untilFinished = index_model,
      args = list(
        table_path = file_model_table,
        iModel = i,
        sampler_args = sampler_args
      ),
      cores = sampler_args$cores,
      maxCore = maxCore,
      priority = 1,
      name = table_model_info$sample_name[i]
    )

  }

  return(table_model_info)
}
