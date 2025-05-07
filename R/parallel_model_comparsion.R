#' Fit many models (optionally compare them) in parallel
#'
#' @description
#' Fits a grid of models in parallel (or in sequence) and, if
#' `sampler_args` is supplied, runs `bridge_sampler()` on each fitted
#' model and stores the results for later Bayes‑factor computation.
#'
#' @param fun Function. A **model fitting function** (supports `brms` and `bmm` models).
#' @param loop_args List. List of arguments whose elements define the grid over which the models are built (e.g., different formulas, priors, or data subsets). The values are passed to `form_fun`, `model_fun`, or `prior_fun`.
#' @param form_fun Function. A function to generate model formulas given `loop_args` (default is `NULL`).
#' @param model_fun Function. A function to generate models given `loop_args` (supports `bmm` models only, default is `NULL`).
#' @param prior_fun Function. A function to generate prior distributions given `loop_args` (default is `NULL`).
#' @param args List. List of arguments always passed to the model-fitting function (i.e., `fun`) such as `data`, `iter`, `chains`, …
#' @param model_name Character. The base name for models (default: `"Model"`).
#' @param model_path Character. The directory to store model `.rds``files.
#' @param sampler_args List or NULL. Arguments for `bridge_sampler` (default: `list(cores = 1, repetition = 10, maxiter = 1000)`). If `NULL`, bridge sampling is skipped.
#' @param sample_path Character. The directory to store bridge-sampling results. Defaults to NULL
#' @param sample_check Logical. If `TRUE`, checks whether samples exist before running (default: `TRUE`).
#' @param maxCore Integer. The maximum number of cores to use (default is system limit).
#'
#' @return Model files and optional sample objects are written to `model_path` and `sample_path`.
#'
#' @examples
#' \dontrun{
#' grid  <- list(prior_sd = c(1, 2, 3))
#' parallel_model_fit(
#'   fun        = brms::brm,
#'   loop_args  = grid,
#'   prior_fun  = function(prior_sd) set_prior(paste0("normal(0,", prior_sd, ")")),
#'   args       = list(formula = bf(y ~ x), data = df, iter = 2000, chains = 4),
#'   model_path = "./models/"
#' )
#' }
#'
#' @import dplyr
#' @importFrom purrr list_assign
#' @export
parallel_model_fit <- function(
    fun,
    loop_args,
    form_fun   = NULL,
    model_fun  = NULL,
    prior_fun  = NULL,
    args,
    model_name   = "Model",
    model_path,
    sampler_args = list(cores = 1, repetition = 10, maxiter = 1000),
    sample_path  = NULL,
    sample_check = TRUE,
    maxCore      = NULL) {

  # ---- Input Validation ----
  if (!is.function(fun))                  stop("`fun` must be a function.")
  if (!is.list(loop_args) || !length(names(loop_args))) stop("`loop_args` must be a named list.")
  if (!dir.exists(model_path))            stop("`model_path` does not exist: ", model_path)

  # Only check sample_path / sample_check when bridge sampling is requested
  if (!is.null(sampler_args)) {
    if (is.null(sample_path) || !dir.exists(sample_path))
      stop("`sample_path` must be an existing directory when sampler_args is not NULL.")
  } else {
    sample_check <- FALSE      # ignore any sample‑checking instruction
  }

  ## ---- helper: build sample name only if needed ----
  sample_name <- if (!is.null(sampler_args)) {
    if (grepl("Model|model", model_name, ignore.case = TRUE)) {
      gsub("Model", "Sample", model_name, ignore.case = TRUE)
    } else {
      paste("Sample", model_name, sep = "_")
    }
  }

  # -------------------------------------------------------------------
  #  Create model table
  # -------------------------------------------------------------------
  grid_args <- c(loop_args, stringsAsFactors = FALSE)
  table_model_info <- do.call(expand.grid, grid_args) %>%
    mutate(
      part_name  = apply(across(everything()), 1,
                         \(row) paste(names(row), row, sep = "", collapse = "_")),
      model_name = paste0(model_name, "_", part_name),
      model_file = file.path(model_path, paste0(model_name, ".rds")),
      model_ck   = as.integer(file.exists(model_file))
    )

  # add sample‑related columns only when needed
  if (!is.null(sampler_args)) {
    table_model_info <- table_model_info %>%
      mutate(
        sample_name = paste0(sample_name, "_", part_name),
        sample_file = file.path(sample_path, paste0(sample_name, ".rds")),
        sample_ck   = as.integer(file.exists(sample_file)),
        comparison  = NA,  BF = NA, logBF = NA, reliability = NA, best_model = NA
      )
  }

  table_model_info <- table_model_info %>% select(-part_name)
  file_model_table <- tempfile(fileext = ".rds")
  saveRDS(table_model_info, file_model_table)

  # -------------------------------------------------------------------
  #  Loop over models
  # -------------------------------------------------------------------
  for (i in seq_len(nrow(table_model_info))) {

    table_model_info <- readRDS(file_model_table)   # reload

    # --- skip if sample already exists (only when we care about samples)
    if (!is.null(sampler_args) &&
        sample_check &&
        table_model_info$sample_ck[i] == 1) next

    current_model_label <- table_model_info$model_name[i]

    # ---------------- build argument list ----------------
    model_args <- args
    par_vals   <- as.list(table_model_info[i, names(loop_args)])

    if (is.function(form_fun))  model_args$formula <- do.call(form_fun,  par_vals)
    if (is.function(model_fun)) model_args$model   <- do.call(model_fun, par_vals)
    if (is.function(prior_fun)) model_args$prior   <- do.call(prior_fun, par_vals)

    model_args$file <- sub("\\.rds$", "", table_model_info$model_file[i])

    # ---------------- run model (smart_runFun) -----------
    smart_runFun(
      fun          = fun,
      args         = model_args,
      untilFinished = NULL,
      maxCore      = maxCore,
      priority     = 1,
      name         = current_model_label
    )

    # ---------------- bridge sampling --------------------
    if (!is.null(sampler_args)) {
      job_info    <- view_job()
      index_model <- which(job_info$name == current_model_label)

      smart_runFun(
        fun = function(table_path, iModel, sampler_args) {
          tab <- readRDS(table_path)
          mod_file  <- tab$model_file[iModel]
          samp_file <- tab$sample_file[iModel]
          if (!file.exists(samp_file)) {
            mod <- readRDS(mod_file)
            sampler_args$samples <- mod
            samp <- do.call(brms::bridge_sampler, sampler_args)
            saveRDS(samp, samp_file)
          }
        },
        untilFinished = index_model,
        args   = list(table_path = file_model_table,
                      iModel     = i,
                      sampler_args = sampler_args),
        cores  = sampler_args$cores,
        maxCore = maxCore,
        priority = 1,
        name   = table_model_info$sample_name[i]
      )
    }
  }
}
