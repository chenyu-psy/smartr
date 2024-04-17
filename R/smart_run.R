
#' Run the code in parallel with the waiting list
#'
#'@description This function is used to run the code in parallel with the waiting list.
#'It relies on the `job` package to manage the job log.
#'
#'@param ... The code to run in parallel.
#'@param untilFinished Logical. If `TRUE`, the code will not run until the previous code is finished.
#'@param cores The number of cores required to run the code.
#'@param maxCore The maximum number of cores that can be used to run the code.
#'@param priority The priority of the code. The code with a higher priority will be run first.
#'@param checkInt The interval to check the job log.
#'@param name The name of the job.
#'
#'@export
#'
smart_run <- function(..., untilFinished = FALSE, cores = 1, maxCore = NULL, priority = 1,  checkInt = 17, name = NULL){

  # read the job log
  job_log_path = tempdir()
  if (file.exists(file.path(job_log_path, "job_log.rds"))) {
    Table_job_status <- view_job(path = job_log_path)
  } else {
    init_job(path = job_log_path)
    Table_job_status <- view_job(path = job_log_path)
  }

  # Read the number of cores on the computer
  machineCore = parallel::detectCores(logical = FALSE)

  # if the maximum cores is not defined, read the maximum cores from the computer
  if (is.null(maxCore)) {
    maxCore = machineCore
  }

  # check if the current model requires more cores than the maximum number of cores available on the computer
  if (cores > maxCore) {
    stop("The current model requires more cores than the maximum number of cores available on the computer.")
  }

  # check whether the maxCore was smaller than the need of the cores
  if (maxCore > machineCore) {
    stop("The current model requires more cores than the maximum number of cores available on the computer.")
  }

  # if the name is not defined, generate a random name
  if (is.null(name)) {
    name = ids::random_id(n=1,bytes=5)
  }

  # get the code
  args = match.call()[-1]  # args excluding function name
  if (length(args) == 0)
    stop("Must have exactly one code block.")
  code = args[[1]]

  # check whether the format of the code is correct
  if (code[[1]] != quote(`{`))
    stop("invalid code input. Did you remember to put the code in {curly brackets}?")
  code_str = paste0(code[-1], collapse = "\n")

  # append the job to the job log
  append_job(
    name = name,
    cores = cores,
    untilFinished = FALSE,
    priority = priority,
    path = job_log_path
  )

  # get the current index
  Table_job_status <- view_job(path = job_log_path)
  current_index = max(Table_job_status$index)

  # run the code in the job
  job::job({

    # set up the progress bar
    message("\nThe task is now in the waiting list ...")
    pb = txtProgressBar(min = 0, max = current_index, initial = 0, style = 3)

    # check how many models are running,
    # wait until the model meet the running condition
    while (TRUE) {

      # read the job log
      Table_job_status <- smartr::view_job(path = job_log_path)

      # how many models are running
      Table_running <- Table_job_status %>%
        dplyr::filter(status == "running")

      # how many cores have been used
      usingCore = sum(Table_running$cores)

      # adjust the waiting list
      Table_waiting <- Table_job_status %>%
        dplyr::filter(status == "pending") %>%
        dplyr::arrange(priority, index)

      # get the waiting index
      WaitIndex = which(Table_waiting$index == current_index)

      # check if the model can be run
      if (untilFinished) {
        running_check = WaitIndex == 1 & cores <= (maxCore - usingCore) & nrow(Table_running) == 0
      } else {
        running_check = WaitIndex == 1 & cores <= (maxCore - usingCore)
      }

      if (running_check) {
        # update the progress bar
        setTxtProgressBar(pb, value = current_index)
        close(pb)
        break
      } else {
        # update progress bar
        setTxtProgressBar(pb, value = current_index - WaitIndex)
        # wait for a while to check the job log
        Sys.sleep(checkInt * WaitIndex + runif(1, 0,5))
      }
    }

    # start running the model --------------------------------------------------

    # print the start time
    start_time = Sys.time()
    message(paste0("\nThe task starts to run at ", start_time))

    # update the job log
    smartr::update_job(
      current_index,
      status = "running",
      path = job_log_path
    )

    tryCatch(
      expr = {

        # Run the model
        eval(parse(text = code_str))

        # Adjust the status of the model as completed.
        smartr::update_job(
          current_index,
          status = "completed",
          path = job_log_path
        )

        return(NA)

      },
      error = function(e){

        # Adjust the status of the model as running.
        smartr::update_job(
          current_index,
          status = "failed",
          path = job_log_path
        )

        # message error
        message(e)

        return(NA)
      },
      finally = {
        # print the end time
        end_time = Sys.time()
        message(paste0("\nThe task has done at ", end_time))
        message(paste0("\nIt takes ", round(as.numeric(end_time - start_time, units = 'hours'),2), " hours."))
      }
    )


  },import = "auto", title = name)

  Sys.sleep(9)

}
