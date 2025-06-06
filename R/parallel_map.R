#' Parallel Map Function
#'
#' Applies a function in parallel over a grid of parameter combinations, supporting both static and dynamically-generated arguments.
#'
#' @param fun A function to be applied in parallel.
#' @param design A named list. Specifies the grid of parameter values to iterate over.
#' @param ... Additional arguments passed to `fun`. These can be:
#'   - Static arguments (e.g., `formula`, `data`), or
#'   - Generator functions with the prefix `fun_` (e.g., `fun_formula`, `fun_prior`), which will be called with the values from the current row of `design` to generate arguments for `fun`.
#' @param cores Integer. Number of cores to use for parallel processing (default: 1). If `cores` is included in `...`, do not provide it again.
#' @param fun_file NULL or function. A generator function for output file paths. If NULL, no file saving is performed.
#' @param post_analysis NULL or function. A function to apply to the results. The function must include `...` in its signature to allow additional arguments to be passed.
#' @param maxCore Integer. Maximum number of cores to use (default: system limit).
#'
#' @import dplyr
#' @export
parallel_map <- function(
    fun,
    design = list(),
    ...,
    cores = 1,
    fun_file = NULL,
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

  if (!is.null(fun_file) && !is.function(fun_file)) {
    stop("Argument `fun_file` must be a function or NULL.")
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

  # Add `cores` to the argument list if required
  if ("cores" %in% fun_arg_names) static_args$cores <- cores

  # Handle file output argument
  manual_save <- TRUE
  if (is.null(fun_file)) manual_save <- FALSE


  # ---- Create Design Grid ----
  grid_args <- c(design, stringsAsFactors = FALSE)
  param_grid <- do.call(expand.grid, grid_args)

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

    # Handle file saving if manual_save is TRUE
    if (manual_save) {
      tmp_file <- do.call(fun_file, param_values)
      run_function <- function(fun, args, file) {
        result <- do.call(fun, args)
        saveRDS(result, file = file)
        invisible(result)
      }
    } else {
      tmp_file <- NULL
      run_function <- function(fun, args, file = NULL) {
        do.call(fun, args)
      }
    }

    # Set a label for the current job
    job_name <- paste0("Job_", paste0(names(param_values),unlist(param_values),collapse = "_"))

    # Run the function (replace with your parallel execution logic)
    smart_runFun(
      fun = run_function,
      args = list(
        fun = fun,
        args = run_args,
        file = tmp_file
      ),
      untilFinished = NULL,
      maxCore = maxCore,
      priority = 1,
      name = job_name
    )
  }

  # ---- Post-Processing ----
  if (!is.null(post_analysis)) {

    job_name <- paste0("Post_Analysis_", paste0(names(param_values),unlist(param_values),collapse = "_"))

    smart_runFun(
      fun = post_analysis,
      args = list(
        args = run_args,
        values = param_values,
        file = tmp_file
      ),
      untilFinished = nrow(view_job()),
      maxCore = maxCore,
      priority = 1,
      name = job_name
    )

  }

}
