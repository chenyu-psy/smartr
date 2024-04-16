
#' Update the job status
#'
#' @description This function will update the status of the job.
#'
#' @param .x The index or name of the job.
#' @param status The status of the job.
#'
update_job <- function(.x, status) {

  # read the job log
  file_path = file.path(tempdir(), "job_log.rds")
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
    Table_status[which(Table_status$index == .x), "startTime"] = Sys.time()
  } else if (status == "finished") {

    # calculate the duration
    startTime = Table_status[which(Table_status$index == .x), "startTime"]
    endTime = Sys.time()
    duration = difftime(endTime, startTime, units = "secs")

    # update the job
    Table_status[which(Table_status$index == .x), "endTime"] = endTime
    Table_status[which(Table_status$index == .x), "duration"] = duration
  }

  # save the job log
  saveRDS(Table_status, file_path)

}
