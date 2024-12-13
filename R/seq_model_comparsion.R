

#' Run a sequence of model comparisons.
#'
#' @description This function runs a sequence of model comparisons. It supports the comparison of multiple models with different parameters.
#' In the same parameter, you can test multiple values. The function will automatically generate all the combinations of the parameters.
#' the comparsion for the next parameter will be based on the best model of the previous parameter.
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
seq_model_comparsion <- function(fun = brms::brm,
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

  # Extract the best parameter combination
  Pars_best <- do.call(base::expand.grid, args = pars)[1,]

  # Adjust input_pars for single or multiple arguments
  if (length(pars) == 1) {
    # For a single argument, keep the structure as a list with one element
    input_pars <- pars
  } else {
    # For multiple arguments, use the first row of the expanded grid
    input_pars <- as.list(Pars_best)
    firs_par = names(pars)[1]
    input_pars[[firs_par]] <- unname(unlist(pars[firs_par]))
  }

  Table_model_info <- do.call(base::expand.grid, args = input_pars) %>%
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

  # test assumptions for each parameter
  for (pr in 1:length(pars)) {

    # parameter name
    par <- names(pars)[pr]

    # save the indices of the models and samples
    indices_models_samples = c()

    # test assumptions for each parameter step by step
    for (i in 1:length(pars[[par]])) {

      # check if the model has been run, if so, skip
      Table_model_info <- readRDS(File_model_table)
      if (sample_check & Table_model_info[i, "sample_ck"] == 1)
        next

      # model name
      current_model_label <- paste0("Model ", i, ": ", par, "-", pars[[par]][i])

      # run the model in job
      smart_runFun(
        fun = function(fun,
                       form_fun,
                       model_fun,
                       prior_fun,
                       args,
                       par_names,
                       path,
                       iModel) {

          # read the model table
          Table_model_info <- readRDS(path)

          # Import the model arguments
          if (!is.null(form_fun))
            args[["formula"]] <-
              do.call(form_fun, args = as.list(Table_model_info[iModel, par_names]))
          if (!is.null(model_fun))
            args[["model"]] <-
              do.call(model_fun, args = as.list(Table_model_info[iModel, par_names]))
          if (!is.null(prior_fun))
            args[["prior"]] <-
              do.call(prior_fun, args = as.list(Table_model_info[iModel, par_names]))
          args[["file"]] <-
            gsub(".rds", "", Table_model_info[iModel, "model_file"])

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
          par_names = names(pars),
          path = File_model_table,
          iModel = i
        ),
        cores = args$cores,
        maxCore = maxCore,
        priority = -(3*pr-2),
        name = current_model_label
      )

      # get the index of model
      Job_info <- view_job()
      index_model <-
        which(Job_info$name == current_model_label)

      # save the indices of the models and samples
      indices_models_samples = c(indices_models_samples, index_model)

      # current sample label
      current_sample_label <- paste0("Sample ", i, ": ", par, "-", pars[[par]][i])


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
        priority = -(3*pr-1),
        name = current_sample_label
      )

    # get the index of sample
    Job_info <- view_job()
    index_sample <- which(Job_info$name == current_sample_label)

    # save the indices of the models and samples
    indices_models_samples = c(indices_models_samples, index_sample)

    }


    # calculate BF -------------------------------------------------------------

    smart_runFun(
      fun = function(pars_names, par, path, bf_path, favorBF) {
        Table_model_info <- readRDS(path)

        # set first model as the best model
        Table_model_info[1, "best_model"] = 1

        # Compare models and calculate BF if there is more than one model
        if (nrow(Table_model_info) >= 2) {
          for (i in 2:nrow(Table_model_info)) {
            # Get the index of the best model from the previous row
            index_best_sample <- Table_model_info$best_model[i - 1]

            # Extract sample names and file paths
            name_best_sample <- Table_model_info$sample_name[index_best_sample]
            name_current_sample <- Table_model_info$sample_name[i]

            file_best_sample <- Table_model_info$sample_file[index_best_sample]
            file_current_sample <- Table_model_info$sample_file[i]

            # Load the best and current samples
            Sample_best <- readRDS(as.character(file_best_sample))
            Sample_current <- readRDS(as.character(file_current_sample))

            # Calculate Bayes Factor
            BF <- bridgesampling::bf(Sample_current, Sample_best)

            # Update Table_model_info with comparison details
            Table_model_info[i, c("comparison", "BF", "logBF", "reliability", "best_model")] <- list(
              stringr::str_glue("Model {i} vs. Model {index_best_sample}"),
              BF$bf_median_based,
              log(BF$bf_median_based),
              paste0(round(log(min(BF$bf)), 2), " ~ ", round(log(max(BF$bf)), 2)),
              ifelse(BF$bf_median_based > favorBF, i, index_best_sample)
            )
          }
        }

        # Define the path for saving the Bayes Factor table
        save_path <- stringr::str_glue("{bf_path}BayesFactor_{par}.csv")

        # Select relevant columns for the table
        selected_columns <- c(pars_names, "comparison", "BF", "logBF", "reliability", "best_model")
        Table_model_info_clean <- Table_model_info %>%
          dplyr::select(dplyr::all_of(selected_columns))

        # Save the table as a CSV file
        utils::write.csv(Table_model_info_clean, save_path)

        # Update temporary table if the current parameter is not the last parameter
        if (par != pars_names[length(pars_names)]) {
          # Get the index and parameter values for the best model
          index_best_sample <- as.numeric(Table_model_info[nrow(Table_model_info), "best_model"])
          pars_best <- Table_model_info[index_best_sample, ]

          # Determine the next parameter to test
          next_par <- pars_names[which(pars_names == par) + 1]

          # Prepare input parameters for the next grid
          input_pars <- pars_best %>% dplyr::select(pars_names) %>% as.list()
          input_pars[[next_par]] <- unname(unlist(pars[next_par]))

          # Create the new table for testing
          table_to_test <- do.call(expand.grid, args = input_pars) %>%
            dplyr::mutate(
              part_name = apply(across(everything()), 1, function(row)
                paste(names(row), row, sep = "", collapse = "_")
              ),
              model_name = paste0(model_name, "_", part_name),
              sample_name = paste0(sample_name, "_", part_name),
              model_file = paste0(model_path, model_name, ".rds"),
              sample_file = paste0(sample_path, sample_name, ".rds"),
              model_ck = as.integer(file.exists(model_file)),
              sample_ck = as.integer(file.exists(sample_file)),
              comparison = NA,
              BF = NA,
              logBF = NA,
              reliability = NA,
              best_model = NA
            ) %>%
            dplyr::select(-part_name)

          # Save the updated table
          saveRDS(table_to_test, file = path)
        }
      },
      untilFinished = indices_models_samples,
      args = list(
        pars_names = names(pars),
        par = par,
        path = File_model_table,
        bf_path = bf_path,
        favorBF = favorBF
      ),
      cores = 1,
      maxCore = maxCore,
      priority = -3*pr,
      name = paste0("Bayes Factor: ", par)
    )
  }
}
