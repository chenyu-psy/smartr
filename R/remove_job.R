

#' Remove job(s) from the job log
#'
#' @description This function will remove job(s) from the job log.
#'
#' @param .x The index or name of the job to be removed.
#'
#' @param path The path to the job log.
#'
#' @importFrom rlang .data
#'
#' @export
remove_job <- function(.x, path = NULL) {

  # read the job log
  if (!is.null(path)) {
    file_path = file.path(path, "job_log.rds")
  } else {
    file_path = file.path(tempdir(), "job_log.rds")
  }
  Table_status = readRDS(file_path)

  # remove the job
  if (is.numeric(.x)) {
    Table_status = Table_status %>% dplyr::filter(!.data$index %in% .x)
  } else if (is.character(.x)) {
    Table_status = Table_status %in% dplyr::filter(!.data$name %in% .x)
  } else {
    stop("The input must be either a numeric or a character.")
  }

  # save the job log
  saveRDS(Table_status, file_path)
}
