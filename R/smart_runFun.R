
#' Run the function in job with the waiting list
#'
#'@description This function is used to run the code in parallel with the waiting list.
#'It relies on the `job` package to manage the job log.
#'
#'@param fun The function to run in parallel.
#'@param args The arguments of the function to run in parallel.
#'@param untilFinished Logical or index. If `TRUE`, the function will not run until all the previous functions are finished.
#'If it is an index or a vector of indices, the function will not run until the functions with the same index are finished.
#'@param cores The number of cores required to run the function
#'@param maxCore The maximum number of cores that can be used to run the function
#'@param priority The priority of the function The code with a higher priority will be run first.
#'@param checkInt The interval to check the job log.
#'@param name The name of the job.
#'@param export The name of the object to export to the global environment.
#'
#'@importFrom rlang .data
#'
#'@export
#'
smart_runFun <- function(
    fun,
    args,
    untilFinished = FALSE,
    cores = NULL,
    maxCore = NULL,
    priority = 1,
    checkInt = 17,
    name = NULL,
    export = FALSE){

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

  # if cores is defined in both args and argument, stop the function
  if (!is.null(cores) & "cores" %in% names(args)) {
    stop("The cores is defined in both args and argument.")
  }

  # if cores is only defined in args, assign the value to cores
  if (is.null(cores) & "cores" %in% names(args)) {
    cores = args$cores
  } else if (is.null(cores)) {
    cores = 1
  }

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

  # if the name is not defined, either use the export name or generate a random name
  if (is.null(name)) {
    if (is.character(export)) {
      name = export
    } else
    name = ids::random_id(n=1,bytes=5)
  }

  # if the untilFinished is not defined, set it to 0
  if (is.null(untilFinished)) {
    untilFinished = "0"
  } else if (is.numeric(untilFinished)) {
    untilFinished = paste(untilFinished,collapse=",")
  } else if (untilFinished==FALSE) {
    untilFinished = "0"
  } else if (untilFinished==TRUE) {
    if (nrow(Table_job_status)==0) {
      untilFinished = "0"
    } else {
      untilFinished = paste(1:nrow(Table_job_status),collapse=",")
    }
  }

  # append the job to the job log
  append_job(
    name = name,
    cores = cores,
    untilFinished = paste(untilFinished,collapse=","),
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

    # check how many models are running,
    # wait until the model meet the running condition
    while (TRUE) {

      # read the job log
      Table_job_status <- view_job(path = job_log_path)

      # how many models are running
      Table_running <- Table_job_status %>%
        dplyr::filter(.data$status == "running")

      # The list of models that are completed
      List_completed <- Table_job_status %>%
        dplyr::filter(.data$status == "completed") %>%
        dplyr::pull(index)

      List_completed <- c(0, List_completed)

      # how many cores have been used
      usingCore = sum(Table_running$cores)

      # adjust the waiting list
      Table_waiting <- Table_job_status %>%
        dplyr::filter(.data$status == "pending") %>%
        dplyr::rowwise() %>%
        dplyr::mutate(queue = length(setdiff(as.numeric(unlist(strsplit(.data$untilFinished, ","))),List_completed))) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(dplyr::desc(.data$priority), .data$queue, .data$index) %>%
        dplyr::mutate(rank = dplyr::row_number())

      # get the waiting index
      WaitIndex = Table_waiting %>%
        dplyr::filter(.data$index == current_index) %>%
        dplyr::pull(rank)

      # check if the model can be run
      needCores <- Table_waiting$cores[WaitIndex]
      queue <- Table_waiting$queue[WaitIndex]
      running_check = WaitIndex == 1 & needCores <= (maxCore - usingCore) & queue == 0

      if (running_check) {

        cat(sprintf("\rThe task is now running ...                                                \n\n"))
        flush.console()
        break
      } else {
        # update the progress
        progValue = (nrow(Table_job_status) - WaitIndex)/nrow(Table_job_status)*100
        sleep_time = checkInt + stats::runif(1, 0,5)
        for (i in as.integer(sleep_time):0) {
          cat(paste0("\rThe task is in position ", WaitIndex, " of the waiting list; the next update will be in ",i,"s.  "))
          flush.console()
          Sys.sleep(1)
        }
        Sys.sleep(runif(1,0,1))
      }
    }

    # start running the model --------------------------------------------------

    # print the start time
    start_time = Sys.time()
    message(paste0("\nThe task starts to run at ", format(start_time, "%Y-%m-%d %H:%M:%S")))

    # update the job log
    update_job(
      current_index,
      status = "running",
      path = job_log_path
    )

    tryCatch(
      expr = {

        # Run the model
        results <- do.call(fun, args)

        if (is.character(export)) assign(export, results, envir = environment())

        rm(results)

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
        message(paste0("\nThe task has done at ", format(end_time, "%Y-%m-%d %H:%M:%S")))
        message(paste0("\nIt takes ", round(as.numeric(end_time - start_time, units = 'hours'),2), " hours."))
      }
    )


  },import = "auto", title = name)

  # extend the waiting time according to the number of ongoing models
  Table_job_status <- view_job(path = job_log_path)
  n_running = sum(Table_job_status$status == "pending")
  delay = dplyr::case_when(
    n_running <= 5 ~ 9,
    n_running <= 10 ~ 19,
    n_running <= 20 ~ 39,
    n_running <= 30 ~ 59,
    TRUE ~ 119
  )
  Sys.sleep(delay)

}
