#' @title Run the model comparison and calculate Bayes Factor
#'
#' @description This function does all the model comparisons and counts the wins, fails, and unclear times for each model.
#'
#' @param pars A list of parameters to compare. Each parameter is a list of values.
#' @param sample_path The path to store the sample.
#' @param sample_prefix The prefix of the sample.
#' @param favorBF The favor of the Bayes Factor. The default is `3`.
#'
#' @return A table of the model comparison.
#' @export

compare_each <- function(pars, sample_path, sample_prefix, favorBF = 3) {
  # Table for the model comparison
  Table_model_info <- do.call(base::expand.grid, args = pars) %>%
    dplyr::mutate(
      part_name = apply(dplyr::across(dplyr::everything()), 1, function(row)
        paste(names(row), row, sep = "", collapse = "_")
      ),
      win = NA,
      fail = NA,
      unclear = NA,
      sample_file = paste0(sample_path, sample_prefix, "_", .data$part_name, ".rds")
    )

  for (i in 1:nrow(Table_model_info)) {

    Sample_currect <- readRDS(as.character(Table_model_info[i, "sample_file"]))

    win <- 0
    fail <- 0
    unclear <- c()

    for (c in 1:nrow(Table_model_info)) {

      if (c == i) next

      competiting <- readRDS(as.character(Table_model_info[c, "sample_file"]))
      BF <- bridgesampling::bf(Sample_currect, competiting)
      if (BF$bf_median_based > 3) {
        win <- win + 1
      } else if (BF$bf_median_based < 0.33) {
        fail  <- fail + 1
      } else {
        unclear <- c(unclear, paste0("model", c, ":", round(BF$bf_median_based, 2)))
      }

    }

    Table_model_info[i, "win"] <- win
    Table_model_info[i, "fail"] <- fail
    Table_model_info[i, "unclear"] <- paste0(unclear, collapse = "; ")

  }

  Table_model_info <- Table_model_info %>%
    dplyr::select(-c(.data$sample_file, .data$part_name))

  return(Table_model_info)

}
