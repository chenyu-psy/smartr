

#' Do all the model comparisons.
#'
#' @description This function is used to compare the models. The function will run the model in parallel and calculate the Bayes Factor.
#'
#' @param fun The function to run the model. The default is `brms::brm`.
#' @param pars A list of parameters to compare. Each parameter is a list of values.
#' @param form_fun The function to generate the formula. The default is `NULL`.
#' @param model_fun The function to generate the model. This parameter only supports the `bmm` model.
#' @param prior_fun The function to generate the prior. The default is `NULL`.
#' @param args The arguments of the function to run in parallel.
#' @param favorBF The favor of the Bayes Factor. The default is `3`.
#' @param model_name The name of the model. The default is `Model`.
#' @param model_path The path to store the model.
#' @param sample_path The path to store the sample.
#' @param bf_path The path to store the Bayes Factor.
#' @param maxCore The maximum number of cores that can be used to run the function.
#' @param sample_check Logical. If `TRUE`, the function will check whether the sample exists. The default is `TRUE`.
#'
#' @return A table of the model comparison.
#'
#'@importFrom rlang .data
#'@export
#'
parallel_model_comparsion <- function(
    fun = brms::brm,
    pars,
    form_fun = NULL,
    model_fun = NULL,
    prior_fun = NULL,
    args,
    favorBF = 3,
    model_name = "Model",
    model_path,
    sample_path,
    bf_path,
    maxCore = NULL,
    sample_check = TRUE) {

  # sample name
  if (stringr::str_detect(model_name, "Model|model")) {
    sample_name <-
      gsub("Model", "Sample", model_name, ignore.case = TRUE)
  } else {
    sample_name <- paste("Sample", model_name, sep = "_")
  }

  # temporary file used to store the model information
  File_model_table <- tempfile(fileext = ".rds")

  # Table for the model comparison
  Table_model_info <-
    do.call(base::expand.grid, args = pars) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      part_name = base::paste(
        names(.),
        base::unlist(dplyr::c_across(1:base::ncol(.))),
        sep = "",
        collapse = "_"
      ),
      model_name = base::paste(model_name, .data$part_name, sep = "_"),
      sample_name = base::paste(sample_name, .data$part_name, sep = "_"),
      comparison = NA,
      BF = NA,
      logBF = NA,
      reliability = NA,
      best_model = NA
    ) %>%
    dplyr::select(!.data$part_name) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      model_file = base::paste0(model_path, .data$model_name, ".rds"),
      sample_file = base::paste0(sample_path, .data$sample_name, ".rds"),
      model_ck = base::ifelse(file.exists(.data$model_file), 1, 0),
      sample_ck = base::ifelse(file.exists(.data$sample_file), 1, 0)
    )


  base::saveRDS(Table_model_info, file = File_model_table)

  # fit the model and run bridge sampler
  for (i in 1:nrow(Table_model_info)) {

    # check if the model has been run, if so, skip
    Table_model_info <- readRDS(File_model_table)
    if (sample_check & Table_model_info[i, "sample_ck"] == 1)
      next

    # model name
    current_model_label <- Table_model_info$model_name[i]

    # run the model in job
    smart_runFun(
      fun = function(fun,
                     form_fun,
                     model_fun,
                     prior_fun,
                     args,
                     path,
                     iModel) {

        Table_model_info <- readRDS(path)

        par_values <- as.list(Table_model_info[iModel,names(pars)])

        # Import the model information
        if (!is.null(form_fun))
          args[["formula"]] <- do.call(form_fun, args = par_values)
        if (!is.null(model_fun))
          args[["model"]] <- do.call(model_fun, args = par_values)
        if (!is.null(prior_fun))
          args[["prior"]] <- do.call(prior_fun, args = par_values)
        args[["file"]] <- gsub(".rds", "", Table_model_info[iModel, "model_file"])

        # Run the model
        do.call(fun, args = args)

      },
      untilFinished = FALSE,
      args = list(
        fun = fun,
        form_fun = form_fun,
        model_fun = model_fun,
        prior_fun = prior_fun,
        args = args,
        path = File_model_table,
        iModel = i
      ),
      cores = args$cores,
      maxCore = maxCore,
      priority = 2,
      name = current_model_label
    )

    # get the index of model
    Job_info <- view_job()
    index_model <-
      which(Job_info$name == current_model_label)


    # bridge sampling
    smart_runFun(
      fun = function(table_path, iModel, cores = 1) {
        Table_model_info <- readRDS(table_path)

        # Import the model information
        model_file <- Table_model_info$model_file[iModel]
        sample_file <- Table_model_info$sample_file[iModel]

        if (!file.exists(sample_file)) {
          # read the model
          model <- readRDS(model_file)

          # Run the bridge sampling
          sample <- brms::bridge_sampler(
            samples = model,
            cores = cores,
            repetition = 10,
            maxiter = 1000
          )

          # save the sample
          saveRDS(sample, sample_file)
        }

      },
      untilFinished = index_model,
      args = list(
        table_path = File_model_table,
        iModel = i,
        cores = args$cores
      ),
      maxCore = maxCore,
      priority = 1,
      name = Table_model_info$sample_name[i]
    )

  }

  # calculate BF ---------------------------------------------------------------

  smart_runFun(
    fun = function(pars_names, path, bf_path, favorBF) {
      Table_model_info <- readRDS(path)

      # set first model as the best model
      Table_model_info[1, "best_model"] = 1

      # Compare models and calculate BF if there is more than one model.
      if (nrow(Table_model_info) >= 2) {
        for (i in 2:nrow(Table_model_info)) {

          # get the best model from the last row
          index_best_sample = Table_model_info$best_model[i - 1]

          name_best_sample <-
            Table_model_info[index_best_sample, "sample_name"]
          name_current_sample <-
            Table_model_info[i, "sample_name"]

          Sample_best <-
            readRDS(as.character(Table_model_info[index_best_sample, "sample_file"]))
          Sample_currect <-
            readRDS(as.character(Table_model_info[i, "sample_file"]))

          BF <- bridgesampling::bf(Sample_currect, Sample_best)

          Table_model_info[i, "comparison"] = stringr::str_glue("Model {i} vs. Model {index_best_sample}")
          Table_model_info[i, "BF"] = BF$bf_median_based
          Table_model_info[i, "logBF"] = log(BF$bf_median_based)
          Table_model_info[i, "reliability"] = paste(round(log(min(BF$bf)),2),round(log(max(BF$bf)),2),sep = " ~ ")
          Table_model_info[i, "best_model"] = ifelse(BF$bf_median_based > favorBF, i, index_best_sample)
        }
      }

      # save the table
      save_path = stringr::str_glue("{bf_path}BayesFactor.csv")

      # select columns and save the table
      Table_model_info <- Table_model_info %>%
        dplyr::select(dplyr::all_of(pars_names),
                      .data$comparison,
                      .data$BF,
                      .data$logBF,
                      .data$reliability,
                      .data$best_model)
      utils::write.csv(Table_model_info, save_path)
    },
    untilFinished = NULL,
    args = list(
      pars_names = names(pars),
      path = File_model_table,
      bf_path = bf_path,
      favorBF = favorBF
    ),
    cores = 1,
    maxCore = maxCore,
    priority = 0,
    name = "Bayes Factor"
  )
}
