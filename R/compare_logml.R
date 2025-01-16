#' @title Compare log marginal likelihood of different models
#'
#' @description This function compares the log marginal likelihood of different models.
#'
#' @param pars A list of parameters to compare.
#' @param sample_path The path to the samples.
#' @param sample_prefix The prefix of the samples.
#'
#' @return A table with the log marginal likelihood of the different models.
#' @export

compare_logml <- function(pars, sample_path, sample_prefix) {

  # Table for the model comparison
  Table_model_info <- do.call(base::expand.grid, args = pars) %>%
    dplyr::mutate(
      part_name = apply(across(everything()), 1, function(row)
        paste(names(row), row, sep = "", collapse = "_")
      ),
      logml = NA,
      sample_file = paste0(sample_path, sample_prefix, "_", part_name, ".rds")
    )

  for (i in 1:nrow(Table_model_info)) {

    Sample_currect <- readRDS(as.character(Table_model_info[i, "sample_file"]))

    Table_model_info[i, "logml"] = median(Sample_currect$logml)
  }

  Table_model_info <- Table_model_info %>%
    select(-c(part_name,sample_file))

  return(Table_model_info)
}
