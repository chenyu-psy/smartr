#' Run a Function as a Job with a Waiting List
#'
#' @description This function runs a function in parallel while managing execution with a waiting list.
#' It uses the `job` package to manage job logs and execution.
#'
#' @param fun A function to be run in parallel.
#' @param args A list of arguments for `fun`.
#' @param untilFinished NULL or a numeric vector. If provided, the function will not run until the specified jobs are completed.
#' @param cores The number of cores required to run the function.
#' @param maxCore The maximum number of cores available for execution.
#' @param priority The priority level of the function. Higher priority jobs are executed first.
#' @param checkInt The time interval (in seconds) for checking the job log.
#' @param name A name for the job. If `NULL`, a random identifier is generated.
#' @param export A character vector specifying objects to export to the global environment.
#'
#' @importFrom rlang .data
#' @export
smart_runFun <- function(
    fun,
    args,
    untilFinished = NULL,
    cores = NULL,
    maxCore = NULL,
    priority = 1,
    checkInt = 17,
    name = NULL,
    export = FALSE
) {

  # Define job log path
  job_log_path <- tempdir()

  # Initialize or read job log
  if (file.exists(file.path(job_log_path, "job_log.rds"))) {
    Table_job_status <- view_job(path = job_log_path)
  } else {
    init_job(path = job_log_path)
    Table_job_status <- view_job(path = job_log_path)
  }

  # Detect available cores
  machineCore <- parallel::detectCores(logical = FALSE)

  # Check for duplicate `cores` definitions
  if (!is.null(cores) && "cores" %in% names(args)) {
    stop("The `cores` parameter is defined both in the function arguments and within `args`. Please specify it only once.")
  }

  # Assign `cores` if it is defined inside `args`
  cores <- if (is.null(cores) && "cores" %in% names(args)) args$cores else if (is.null(cores)) 1 else cores

  # If `threads` is provided in the args, recalculate the cores
  if ("threads" %in% names(args)) {
    cores <- cores * args$threads$threads
  }

  # Assign `maxCore` if not defined
  maxCore <- if (is.null(maxCore)) machineCore else maxCore

  # Validate core constraints
  if (cores > maxCore) {
    stop("The requested `cores` exceed the available `maxCore` resources.")
  }
  if (maxCore > machineCore) {
    stop("The `maxCore` exceeds the total number of available cores on this machine.")
  }

  # Assign job name
  if (is.null(name)) {
    name <- if (is.character(export)) export else ids::random_id(n = 1, bytes = 5)
  }

  # Process `untilFinished` parameter
  if (is.null(untilFinished)) {
    untilFinished <- "0"
  } else if (is.numeric(untilFinished)) {
    untilFinished <- paste(untilFinished, collapse = ",")
  } else {
    stop("`untilFinished` must be a numeric or character vector.")
  }

  # Append job to log
  append_job(
    name = name,
    cores = cores,
    untilFinished = untilFinished,
    priority = priority,
    path = job_log_path
  )

  # Get the current job index
  Table_job_status <- view_job(path = job_log_path)
  current_index <- max(Table_job_status$index)

  # Run job in background
  job::job({

    # import functions
    update_job <- getFromNamespace("update_job", "smartr")
    append_job <- getFromNamespace("append_job", "smartr")

    message("\nThe task is now in the waiting list ...")

    while (TRUE) {
      Table_job_status <- view_job(path = job_log_path)

      # Identify running and completed jobs
      running_jobs <- dplyr::filter(Table_job_status, .data$status == "running")
      completed_jobs <- dplyr::filter(Table_job_status, .data$status == "completed") %>%
        dplyr::pull(index)
      completed_jobs <- c(0, completed_jobs)

      # Check used cores
      used_cores <- sum(running_jobs$cores)

      # Update waiting list
      Table_waiting <- Table_job_status %>%
        dplyr::filter(.data$status == "pending") %>%
        dplyr::rowwise() %>%
        dplyr::mutate(queue = length(setdiff(as.numeric(unlist(strsplit(.data$untilFinished, ","))), completed_jobs))) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(dplyr::desc(.data$priority), .data$queue, .data$index) %>%
        dplyr::mutate(rank = dplyr::row_number())

      WaitIndex <- Table_waiting %>%
        dplyr::filter(.data$index == current_index) %>%
        dplyr::pull(rank)

      # Check if job can be executed
      can_run <- WaitIndex == 1 && Table_waiting$cores[WaitIndex] <= (maxCore - used_cores) && Table_waiting$queue[WaitIndex] == 0

      if (can_run) {
        cat("\rThe task is now running ...\n\n")
        break
      } else {
        sleep_time <- checkInt + stats::runif(1, 0, 5)
        for (i in as.integer(sleep_time):0) {
          cat(paste0("\rWaiting list position: ", WaitIndex, "; next update in ", i, "s.  "))
          flush.console()
          Sys.sleep(1)
        }
        Sys.sleep(runif(1, 0, 1))
      }
    }

    # Start execution
    start_time <- Sys.time()
    message("\nTask started at ", format(start_time, "%Y-%m-%d %H:%M:%S"))

    update_job(current_index, status = "running", path = job_log_path)

    tryCatch(
      expr = {
        results <- do.call(fun, args)
        if (is.character(export)) assign(export, results, envir = environment())

        update_job(current_index, status = "completed", path = job_log_path)
      },
      error = function(e) {
        update_job(current_index, status = "failed", path = job_log_path)
        message("Error: ", e)
      },
      finally = {
        end_time <- Sys.time()
        message("\nTask completed at ", format(end_time, "%Y-%m-%d %H:%M:%S"))
        message("\nElapsed time: ", round(as.numeric(end_time - start_time, units = "hours"), 2), " hours.")
      }
    )

  }, import = "auto", title = name)

  # Adjust waiting time based on queue size
  Table_job_status <- view_job(path = job_log_path)
  n_running <- sum(Table_job_status$status == "pending")
  delay <- dplyr::case_when(
    n_running <= 5  ~ 9,
    n_running <= 10 ~ 19,
    n_running <= 20 ~ 39,
    n_running <= 30 ~ 59,
    TRUE           ~ 119
  )
  Sys.sleep(delay)
}
