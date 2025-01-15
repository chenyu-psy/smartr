

#' Do all the model comparisons.
#'
#' @description This function is used to compare the models. The function will run the model in parallel and calculate the Bayes Factor.
#'
#' @param fun The function to run the model supports only the `brms` model and the `bmm` model.
#' @param pars A list of parameters to compare. Each parameter is a list of values.
#' @param form_fun The function to generate the formula. The default is `NULL`.
#' @param model_fun The function to generate the model. This parameter only supports the `bmm` model.
#' @param prior_fun The function to generate the prior. The default is `NULL`.
#' @param args The arguments of the function to run in parallel.
#' @param model_name The name of the model. The default is `Model`.
#' @param model_path The path to store the model.
#' @param sample_path The path to store the sample.
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
    model_name = "Model",
    model_path,
    sample_path,
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
  Table_model_info <- do.call(base::expand.grid, args = pars) %>%
    dplyr::mutate(
      part_name = apply(across(everything()), 1, function(row)
        paste(names(row), row, sep = "", collapse = "_")
      ),
      model_name = paste0(model_name, "_", part_name),
      sample_name = paste0(sample_name, "_", part_name),
      comparison = NA,
      BF = NA,
      logBF = NA,
      reliability = NA,
      best_model = NA,
      model_file = paste0(model_path, model_name, ".rds"),
      sample_file = paste0(sample_path, sample_name, ".rds"),
      model_ck = as.integer(file.exists(model_file)),
      sample_ck = as.integer(file.exists(sample_file))
    ) %>%
    dplyr::select(-part_name)

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

}
