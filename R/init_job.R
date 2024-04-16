#' Initialize job log.
#'
#' @description
#' This function will create a data frame including model information and store it as a temporary file.
#'
#'@export
init_job <- function(path = NULL) {

  table_cols <- c("index", "name", "useCore", "untilFinished", "priority", "status", "startTime", "endTime", "duration")

  Table_status <- stats::setNames(data.frame(matrix(ncol = length(table_cols), nrow = 0)), table_cols)

  file_path = file.path(tempdir(), "job_log.rds")

  saveRDS(Table_status, file_path)

}
