#' @title Run the model comparison sequentially and calculate Bayes Factor
#'
#' @description This function calculates the Bayes Factor between the best model and competing models. The favored model will replace the best model and compare with the next model.
#'
#' @param pars A list of parameters to compare. Each parameter is a list of values.
#' @param sample_path The path to store the sample.
#' @param sample_prefix The prefix of the sample.
#' @param favorBF The favor of the Bayes Factor. The default is `3`.
#' @param shuffle Logical. If `TRUE`, the function will shuffle the order of the model comparison. The default is `FALSE`.
#'
#' @return A table of the model comparison.
#' @export

compare_best <- function(pars, sample_path, sample_prefix, favorBF = 3, shuffle = FALSE) {

  # Table for the model comparison
  Table_model_info <- do.call(base::expand.grid, args = pars) %>%
    dplyr::mutate(
      part_name = apply(dplyr::across(dplyr::everything()), 1, function(row)
        paste(names(row), row, sep = "", collapse = "_")
      ),
      comparison = NA,
      BF = NA,
      logBF = NA,
      reliability = NA,
      best_model = NA,
      sample_file = paste0(sample_path, sample_prefix, "_", .data$part_name,".rds")
    )

  if (shuffle) {
    Table_model_info <- Table_model_info[sample(nrow(Table_model_info)), ]
  }

  Table_model_info$best_model[1] = 1

  for (i in 2:nrow(Table_model_info)) {

    # get the best model from the last row
    index_best_sample = Table_model_info$best_model[i - 1]

    Sample_best <-
      readRDS(as.character(Table_model_info[index_best_sample, "sample_file"]))
    Sample_currect <-
      readRDS(as.character(Table_model_info[i, "sample_file"]))

    BF <- bridgesampling::bf(Sample_currect, Sample_best)

    Table_model_info[i, c("comparison", "BF", "logBF", "reliability", "best_model")] <- list(
      stringr::str_glue("Model {i} vs. Model {index_best_sample}"),
      BF$bf_median_based,
      log(BF$bf_median_based),
      paste0(round(log(min(BF$bf)), 2), " ~ ", round(log(max(BF$bf)), 2)),
      ifelse(BF$bf_median_based > favorBF, i, index_best_sample)
    )

  }

  Table_model_info <- Table_model_info %>%
    dplyr::select(dplyr::all_of(names(pars)), .data$comparison, .data$BF, .data$logBF, .data$reliability, .data$best_model)

  return(Table_model_info)

}
