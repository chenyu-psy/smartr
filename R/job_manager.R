#' Initialize a job log.
#'
#' @description
#' Creates an empty job log as a data frame with model information and stores it as an RDS file.
#'
#' @param path Character. Optional. The directory where the job log should be stored.
#' If `NULL`, the log is saved in a temporary directory.
#'
#' @examples
#' file_path <- init_job()  # Saves in temp directory
#' file_path <- init_job("path/to/directory")  # Saves in a specified directory
#'
#' @export
init_job <- function(path = NULL) {

  # Define table structure
  table_cols <- c("index", "name", "cores", "untilFinished", "priority",
                  "status", "startTime", "endTime", "duration")

  table_status <- stats::setNames(data.frame(matrix(ncol = length(table_cols), nrow = 0)), table_cols)

  # Determine file path
  if (!is.null(path)) {
    if (!dir.exists(path)) {
      stop("The specified directory does not exist: ", path)
    }
    file_path <- file.path(path, "job_log.rds")
  } else {
    file_path <- file.path(tempdir(), "job_log.rds")
  }

  # Save the log file
  saveRDS(table_status, file_path)

  # Inform the user
  message("The job log has been initialized.")

}


#' View the status of a job.
#'
#' @description
#' Retrieves job statuses from the job log. If no job index or name is specified, returns all jobs.
#'
#' @param .x Character or numeric. The job name (character) or index (numeric).
#' If `NULL`, returns all jobs.
#'
#' @param path Character. Optional. The directory where the job log is stored.
#' If `NULL`, the function looks in the temporary directory.
#'
#' @return A data frame containing the status of the specified job(s).
#'
#' @importFrom dplyr filter
#' @importFrom rlang .data
#'
#' @examples
#' view_job()  # View all jobs
#' view_job(1)  # View job with index 1
#' view_job("job_name")  # View job with name "job_name"
#'
#' @export
view_job <- function(.x = NULL, path = NULL) {

  # Determine file path
  file_path <- if (!is.null(path)) {
    file.path(path, "job_log.rds")
  } else {
    file.path(tempdir(), "job_log.rds")
  }

  # Check if the log file exists
  if (!file.exists(file_path)) {
    stop("Job log file not found at: ", file_path,
         "\nMake sure you have initialized the job log using `init_job()`.")
  }

  # Load job log
  table_status <- readRDS(file_path)

  # Filter based on input
  if (!is.null(.x)) {
    if (is.character(.x)) {
      table_status <- dplyr::filter(table_status, .data$name %in% .x)
    } else if (is.numeric(.x)) {
      table_status <- dplyr::filter(table_status, .data$index %in% .x)
    } else {
      stop("`.x` should be either a character (job name) or numeric (job index).")
    }
  }

  return(table_status)
}



#' Append a new job to the job log.
#'
#' @description
#' Adds a new job entry to the job log, assigning an index, priority, and status.
#'
#' @param name Character. The name of the job. This must be provided.
#' @param cores Integer. The number of cores allocated to the job. Default is `1`.
#' @param untilFinished Logical. Whether the job should wait for previous jobs to finish before running. Default is `FALSE`.
#' @param priority Numeric. The priority of the job, with higher numbers indicating higher priority. Default is `0`.
#' @param path Character. Optional. The directory where the job log is stored. If `NULL`, the function looks in the temporary directory.
#'
#' @examples
#' append_job(name = "Task_A", cores = 2, untilFinished = TRUE, priority = 1)
#' append_job(name = "Task_B", cores = 4, priority = 3)
#'
#' @export
append_job <- function(
    name = NULL,
    cores = 1,
    untilFinished = FALSE,
    priority = 0,
    path = NULL) {

  # Validate input parameters
  if (is.null(name) || !is.character(name)) {
    stop("`name` must be provided and must be a character string.")
  }

  if (!is.numeric(cores) || cores < 1 || cores %% 1 != 0) {
    stop("`cores` must be a positive integer.")
  }

  if (!is.logical(untilFinished)) {
    stop("`untilFinished` must be a logical value (`TRUE` or `FALSE`).")
  }

  if (!is.numeric(priority)) {
    stop("`priority` must be a numeric value.")
  }

  # Determine file path
  file_path <- if (!is.null(path)) {
    file.path(path, "job_log.rds")
  } else {
    file.path(tempdir(), "job_log.rds")
  }

  # Check if job log file exists
  if (!file.exists(file_path)) {
    stop("Job log file not found at: ", file_path,
         "\nMake sure you have initialized the job log using `init_job()`.")
  }

  # Read the job log
  table_status <- readRDS(file_path)

  # Determine job index
  index <- if (nrow(table_status) == 0) 1 else max(table_status$index, na.rm = TRUE) + 1

  # Create a new job entry
  job_entry <- data.frame(
    index = index,
    name = as.character(name),
    cores = as.integer(cores),
    untilFinished = as.logical(untilFinished),
    priority = as.numeric(priority),
    status = "pending",
    startTime = NA,
    endTime = NA,
    duration = NA,
    stringsAsFactors = FALSE
  )

  # Append new job entry
  table_status <- rbind(table_status, job_entry)

  # Save updated job log
  saveRDS(table_status, file_path)

}



#' Update the job status.
#'
#' @description
#' Updates the status of a job in the job log. If setting a job to `"running"`, it records the start time.
#' If setting a job to `"completed"` or `"failed"`, it records the end time and calculates duration.
#'
#' @param .x Character or numeric. The job name (character) or index (numeric).
#' @param status Character. The new status of the job. Must be one of `"pending"`, `"running"`, `"completed"`, or `"failed"`.
#' @param path Character. Optional. The directory where the job log is stored.
#' If `NULL`, the function looks in the temporary directory.
#'
#' @examples
#' update_job(1, "running")  # Mark job with index 1 as running
#' update_job("Task_A", "completed")  # Mark job with name "Task_A" as completed
#'
#' @export
update_job <- function(.x, status, path = NULL) {

  # Define valid statuses
  valid_statuses <- c("pending", "running", "completed", "failed")

  # Validate status
  if (!status %in% valid_statuses) {
    stop("Invalid `status`. It must be one of: ", paste(valid_statuses, collapse = ", "), ".")
  }

  # Determine file path
  file_path <- if (!is.null(path)) {
    file.path(path, "job_log.rds")
  } else {
    file.path(tempdir(), "job_log.rds")
  }

  # Check if job log file exists
  if (!file.exists(file_path)) {
    stop("Job log file not found at: ", file_path,
         "\nMake sure you have initialized the job log using `init_job()`.")
  }

  # Load job log
  table_status <- readRDS(file_path)

  # Identify job index
  if (is.numeric(.x)) {
    job_index <- .x
  } else if (is.character(.x)) {
    job_index <- table_status[which(table_status$name == .x), "index"]
  } else {
    stop("`.x` should be either a numeric (job index) or character (job name).")
  }

  # Validate job existence
  if (length(job_index) == 0 || is.na(job_index)) {
    stop("No job found with the specified index or name.")
  }

  # Update the job status
  job_row <- which(table_status$index == job_index)

  if (status == "running") {
    start_time <- Sys.time()
    table_status[job_row, "startTime"] <- format(start_time, "%Y-%m-%d %H:%M:%S")
  }
  else if (status %in% c("completed", "failed")) {
    start_time <- as.POSIXct(table_status[job_row, "startTime"])

    if (is.na(start_time)) {
      stop("Cannot mark job as 'completed' or 'failed' before it has been 'running'.")
    }

    end_time <- Sys.time()
    duration <- round(difftime(end_time, start_time, units = "hours"), 2)

    table_status[job_row, "endTime"] <- format(end_time, "%Y-%m-%d %H:%M:%S")
    table_status[job_row, "duration"] <- duration
  }

  # Apply status update
  table_status[job_row, "status"] <- status

  # Save updated job log
  saveRDS(table_status, file_path)

}


#' Remove job(s) from the job log.
#'
#' @description
#' Removes a job (or multiple jobs) from the job log by index or name.
#'
#' @param .x Character or numeric. The job name (character) or index (numeric) of the job(s) to be removed.
#' @param path Character. Optional. The directory where the job log is stored.
#' If `NULL`, the function looks in the temporary directory.
#'
#' @importFrom dplyr filter
#' @importFrom rlang .data
#'
#' @examples
#' remove_job(1)  # Remove job with index 1
#' remove_job("Task_A")  # Remove job with name "Task_A"
#'
#' @export
remove_job <- function(.x, path = NULL) {

  # Determine file path
  file_path <- if (!is.null(path)) {
    file.path(path, "job_log.rds")
  } else {
    file.path(tempdir(), "job_log.rds")
  }

  # Check if job log file exists
  if (!file.exists(file_path)) {
    stop("Job log file not found at: ", file_path,
         "\nMake sure you have initialized the job log using `init_job()`.")
  }

  # Read the job log
  table_status <- readRDS(file_path)

  # Check if .x exists in job log
  if (is.numeric(.x)) {
    if (!any(table_status$index %in% .x)) {
      stop("No job found with the specified index: ", .x)
    }
    table_status <- dplyr::filter(table_status, !.data$index %in% .x)
  } else if (is.character(.x)) {
    if (!any(table_status$name %in% .x)) {
      stop("No job found with the specified name: ", .x)
    }
    table_status <- dplyr::filter(table_status, !.data$name %in% .x)
  } else {
    stop("`.x` should be either a numeric (job index) or character (job name).")
  }

  # Save updated job log
  saveRDS(table_status, file_path)

  # Inform the user
  message("Job '", .x, "' removed from job log.")
}
