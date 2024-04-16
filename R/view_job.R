
#' View the status of the job
#'
#' @description This function will return the status of the job.
#'
#' @param .x The index or name of the job. If NULL, return all jobs.
#'
#' @return A data frame containing the status of the job.
#'
#' @export
#'
view_job <- function(.x=NULL) {

  file_path = file.path(tempdir(), "job_log.rds")
  Table_status = readRDS(file_path)

  if (is.character(.x)) {
    Table_status %>% filter(name %in% .x) %>% return()
  } else if (is.numeric(.x)) {
    Table_status %>% filter(index %in% .x) %>% return()
  } else if (is.null(.x)) {
    return(Table_status)
  }

}
