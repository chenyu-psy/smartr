
#' Append a new job to the job log
#'
#' @description This function will append a new job to the job log.
#'
#' @param name The name of the job.
#' @param cores The number of cores used by the job.
#' @param untilFinished Boolean value indicating whether the job should not run
#' until previous jobs are finished.
#' @param priority The priority of the job.
#' @param path The path to the job log.
#'
append_job <- function(
    name = NULL,
    cores = 1,
    untilFinished = FALSE,
    priority = 0,
    path = NULL) {

  # read the job log
  if (!is.null(path)) {
    file_path = file.path(path, "job_log.rds")
  } else {
    file_path = file.path(tempdir(), "job_log.rds")
  }
  Table_status = readRDS(file_path)

  # get the index
  if (nrow(Table_status) == 0) {
    index = 1
  } else {
    index = max(Table_status$index) + 1
  }

  # create a new row
  new_row = data.frame(
    index = index,
    name = name,
    cores = cores,
    untilFinished = untilFinished,
    priority = priority,
    status = "pending",
    startTime = NA,
    endTime = NA,
    duration = NA
  )

  # append the new row to the job log
  Table_status = rbind(Table_status, new_row)

  # save the job log
  saveRDS(Table_status, file_path)
}
