#' Parallel Map over Parameter Grid
#'
#' Applies a function in parallel across all combinations of parameters defined in a design grid.
#' Supports both static and dynamically-generated arguments, as well as optional pre-checks, result saving, and post-analysis.
#'
#' @param fun Function. The main function to be applied to each parameter combination.
#' @param design Named list. Each element defines a parameter and its possible values; all combinations are evaluated.
#' @param ... Additional arguments passed to \code{fun}. Can be static arguments or generator functions with the prefix \code{fun_}.
#' @param cores Integer. Number of cores to use for parallel processing (default: 1).
#' This argument would be passed to the main function if it accepts it.
#' If \code{fun_cores} exists, it will be ignored.
#' @param auto_save NULL or list. If a list, provides information for saving results (see \code{save_results}).
#'   By default, uses \code{save_results()} (i.e., saves to "./results/" as RDS files).
#'   If NULL, results are not saved to disk.
#' @param pre_check NULL or function. Optional function called before each run to determine whether to skip the run.
#'   See \strong{Details} for usage and required signature.
#' @param post_analysis NULL or function. Optional function called after all runs for post-processing.
#'   See \strong{Details} for usage and required signature.
#' @param maxCore Integer. Maximum number of cores to use for parallel processing (default: system limit).
#'
#' @details
#' \strong{Dynamic Argument Generators:}
#' Arguments to \code{...} with the prefix \code{fun_} (e.g., \code{fun_formula}) must be functions.
#' These are called with the current row of \code{design} to generate arguments for \code{fun}.
#'
#' \strong{Pre-check Function:}
#' If provided, \code{pre_check} is called before each run to determine whether to skip that run.
#' The function signature must include \code{...} to allow for additional variables.
#' The following arguments are passed automatically and can be used directly in your \code{pre_check} function:
#' \itemize{
#'   \item \code{vars}: A named list of the current parameter values.
#'   \item \code{run_args}: The arguments used for the current run (i.e., \code{run_args}).
#'   \item \code{label}: The label string for the current parameter combination.
#'   \item \code{save_args}: The current \code{auto_save} list.
#' }
#' The function should return \code{TRUE} to skip the run, or \code{FALSE} to proceed.
#'
#' \strong{Post-analysis Function:}
#' If provided, \code{post_analysis} is called after all runs for post-processing.
#' The function signature must include \code{...} to allow for additional variables.
#' The following arguments are passed automatically and can be used directly in your \code{post_analysis} function:
#' \itemize{
#'  \item \code{result}: The result output from the main function.
#'   \item \code{vars}: The parameter values for the last run (\code{param_values}).
#'   \item \code{run_args}: The arguments used for the last run (\code{run_args}).
#'   \item \code{file}: The file path for the saved result (\code{tmp_file}), if saving is enabled.
#' }
#'
#' @export
parallel_map <- function(
    fun,
    design = list(),
    ...,
    cores = 1,
    auto_save = save_results(),
    pre_check = NULL,
    post_analysis = NULL,
    maxCore = NULL
) {

  # ---- Input Validation ----
  if (!is.function(fun)) stop("Argument `fun` must be a function.")
  if (!is.list(design) || !length(names(design))) stop("Argument `design` must be a named list.")

  args <- list(...)
  arg_names <- names(args)
  arg_funs <- arg_names[grepl("^fun_", arg_names)]
  static_args <- args[!grepl("^fun_", arg_names)]
  dynamic_arg_names <- sub("^fun_", "", arg_funs)

  # Ensure all dynamic argument generators are functions
  for (af in arg_funs) {
    if (!is.function(args[[af]])) {
      stop(sprintf("Argument `%s` must be a function.", af))
    }
  }

  if (!is.null(post_analysis) && !is.function(post_analysis)) {
    stop("Argument `post_analysis` must be a function or NULL.")
  }

  # Check that all provided arguments are defined in the main function
  fun_arg_names <- names(formals(fun))
  all_passed_args <- c(names(static_args), dynamic_arg_names)
  extra_args <- setdiff(all_passed_args, fun_arg_names)
  if (length(extra_args) > 0) {
    if ("..." %in% fun_arg_names) {
      warning(sprintf(
        "Argument(s) `%s` in the function `%s` might be ignored.",
        paste(extra_args, collapse = "`, `"),
        deparse(substitute(fun))
      ))
    } else {
      stop(sprintf(
        "Function `%s` does not have argument(s) `%s`.",
        deparse(substitute(fun)),
        paste(extra_args, collapse = "`, `")
      ))
    }
  }

  # Check that all required fields for auto_save are present
  if (!is.null(auto_save)) {
    required_fields <- c("name_prefix", "sep", "format", "path", "save_fun")
    missing_fields <- setdiff(required_fields, names(auto_save))
    if (length(missing_fields) > 0) {
      stop(sprintf("auto_save is missing required fields: %s", paste(missing_fields, collapse = ", ")))
    }
  }

  # Add `cores` to the argument list if required
  if ("cores" %in% fun_arg_names && !"fun_cores" %in% arg_funs) static_args$cores <- cores

  # ---- Create Design Grid ----
  grid_args <- c(design, stringsAsFactors = FALSE)
  param_grid <- do.call(expand.grid, grid_args)
  if (nrow(param_grid) == 0) return(invisible(NULL))

  # ---- Parallel Execution ----
  for (i in seq_len(nrow(param_grid))) {
    # Build argument list for this run
    run_args <- static_args
    design_vars <- names(design)
    param_values <- as.list(param_grid[i, design_vars])

    # Generate dynamic arguments
    for (arg in dynamic_arg_names) {
      required_args <- names(formals(args[[paste0("fun_", arg)]]))
      tmp_values <- param_values[required_args]
      run_args[[arg]] <- do.call(args[[paste0("fun_", arg)]], tmp_values)
    }

    # Generate a label for the current job (your original logic)
    current_label <- paste0(names(param_values), unlist(param_values), collapse = if (!is.null(auto_save)) auto_save$sep else "_")

    # --- Pre-check ----

    # Prepare arguments for pre-check
    pre_check_args <- list(
      vars  = param_values,
      run_args = run_args,
      label = current_label,
      save_args = auto_save)

    if (!is.null(pre_check)) {
      if (!is.function(pre_check)) stop("pre_check must be a function or NULL.")
      skip_run <- do.call(pre_check, pre_check_args)
      if (skip_run) next
    }

    # ---- Run Function ----
    run_function <- function(fun, args, auto_save = NULL) {

      # apply the function with the provided arguments
      result <- do.call(fun, args)

      # If post_analysis is provided, run it after the main function
      if (!is.null(post_analysis)) {
        post_args <- list(
          result = result,
          vars = param_values,
          run_args = run_args
        )
        post_result <- do.call(post_analysis, post_args)
      } else {
        post_result <- result
      }

      # Save the post result if auto_save is provided
      if (!is.null(auto_save)) {

        save_args <- auto_save$args

        tmp_file_name <- paste0(auto_save$name_prefix, auto_save$sep, current_label, ".", auto_save$format)
        tmp_file <- file.path(auto_save$path, tmp_file_name)
        save_args$file <- tmp_file

        do.call(auto_save$save_fun, c(list(post_result), save_args))
      }

    }

    # Set a label for the current job
    job_name <- paste0("Job_", current_label)

    # Run the function (replace with your parallel execution logic)
    smart_runFun(
      fun = run_function,
      args = list(
        fun = fun,
        args = run_args,
        auto_save = auto_save
      ),
      untilFinished = NULL,
      maxCore = maxCore,
      priority = 1,
      name = job_name
    )
  }

  invisible(NULL)
}



#' Save Results Utility
#'
#' Prepares and validates the parameters for saving results to disk.
#'
#' @param path Character. Directory path where results will be saved. Default is "./results/".
#' @param name_prefix Character. Prefix for the saved file names. Default is "Results".
#' @param sep Character. Separator used in file names. Default is "_".
#' @param format Character. Format for saving results ("rds", "csv", or custom). Default is "rds".
#' @param save_fun Function. Custom function to save results. If NULL, uses default based on `format`.
#' @param ... Additional arguments to be passed to the saving function.
#'
#' @return A list containing the validated and prepared arguments for saving results.
#' @examples
#' res <- save_results(path = "output", format = "csv")
#' print(res)
save_results <- function(
    path = "./results/",
    name_prefix = "Results",
    sep = "_",
    format = "rds",
    save_fun = NULL,
    ...
) {
  # Ensure path ends with a "/"
  if (substr(path, nchar(path), nchar(path)) != "/") {
    path <- paste0(path, "/")
  }

  # Create directory if it doesn't exist
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
    message(sprintf("Directory '%s' did not exist and was created.", path))
  }

  # Determine the saving function if not provided
  if (is.null(save_fun)) {
    if (format == "rds") {
      save_fun <- saveRDS
    } else if (format %in% c("csv", "tsv")) {
      save_fun <- write.table
    } else {
      stop("Please provide a specific function to save the results, as the format is not 'rds', 'csv', or 'tsv'.")
    }
  } else {
    # Validate that save_fun is a function
    if (!is.function(save_fun)) {
      stop("Argument `save_fun` must be a function.")
    }
  }

  # Collect additional arguments
  args <- list(...)

  # Return all relevant information as a list
  list(
    path = path,
    name_prefix = name_prefix,
    sep = sep,
    format = format,
    save_fun = save_fun,
    args = args
  )
}

