
#' View the status of the job
#'
#' @description This function will return the status of the job.
#'
#' @param .x The index or name of the job. If NULL, return all jobs.
#'
#' @param path The path to the job log.
#'
#' @return A data frame containing the status of the job.
#'
#' @importFrom rlang .data
#'
#' @export
#'
view_job <- function(.x=NULL, path=NULL) {

  if (!is.null(path)) {
    file_path = file.path(path, "job_log.rds")
  } else {
    file_path = file.path(tempdir(), "job_log.rds")
  }

  Table_status = readRDS(file_path)

  if (is.character(.x)) {
    Table_status %>% dplyr::filter(.data$name %in% .x) %>% return()
  } else if (is.numeric(.x)) {
    Table_status %>% dplyr::filter(.data$index %in% .x) %>% return()
  } else if (is.null(.x)) {
    return(Table_status)
  }

}
