
#' Update the job status
#'
#' @description This function will update the status of the job.
#'
#' @param .x The index or name of the job.
#' @param status The status of the job.
#' @param path The path to the job log.
#'
#'@export
#'
#'@keyword Internal
#'
update_job <- function(.x, status, path=NULL) {

  # read the job log
  if (!is.null(path)) {
    file_path = file.path(path, "job_log.rds")
  } else {
    file_path = file.path(tempdir(), "job_log.rds")
  }
  Table_status = readRDS(file_path)

  # get the index
  if (is.numeric(.x)) {
    index = .x
  } else if (is.character(.x)) {
    index = Table_status[which(Table_status$name == .x), "index"]
  } else {
    stop("The input must be either a numeric or a character.")
  }

  # update the job
  if (status == "running") {
    startTime = as.character(Sys.time())
    Table_status[which(Table_status$index == .x), "startTime"] = startTime
  } else if (status == "completed" || status == "failed") {

    # calculate the duration
    startTime = Table_status[which(Table_status$index == .x), "startTime"]
    startTime = as.POSIXct(startTime)
    endTime = as.character(Sys.time())
    duration = difftime(endTime, startTime, units = "hours") %>% round(2)

    # update the job
    Table_status[which(Table_status$index == .x), "endTime"] = endTime
    Table_status[which(Table_status$index == .x), "duration"] = duration
  }

  Table_status[which(Table_status$index == .x), "status"] = status

  # save the job log
  saveRDS(Table_status, file_path)

}
