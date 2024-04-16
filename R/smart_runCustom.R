
smart_runCustom <- function(..., untilFinished = FALSE, core = 1, maxCore = NULL, priority = 1,  checkInt = 19, name = NULL){

  # read the job log
  job_log_path = file.path(tempdir(), "job_log.rds")
  if (file.exists(job_log_path)) {
    Table_job_status <- view_job()
  } else {
    init_job()
    Table_job_status <- view_job()
  }

  # Read the number of cores on the computer
  machineCore = parallel::detectCores(logical = FALSE)

  # if the maximum cores is not defined, read the maximum cores from the computer
  if (is.null(maxCore)) {
    maxCore = machineCore
  }

  # check if the current model requires more cores than the maximum number of cores available on the computer
  if (core > maxCore) {
    stop("The current model requires more cores than the maximum number of cores available on the computer.")
  }

  # check whether the maxCore was smaller than the need of the core
  if (maxCore > machineCore) {
    stop("The current model requires more cores than the maximum number of cores available on the computer.")
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
    name = ids::random_id(n=1,bytes=5),
    useCore = core,
    untilFinished = FALSE,
    priority = priority
  )

  # get the current index
  Table_job_status <- view_job()
  current_index = max(Table_job_status$index)

  # run the code in the job
  job::job({

    # set up the progress bar
    message("\nThe task is now in the waiting list ...")
    pb = txtProgressBar(min = 0, max = job_index, initial = 0, style = 3)

    # check how many models are running,
    # wait until the model meet the running condition
    while (TRUE) {

      # read the job log
      Table_job_status <- view_job()

      # how many models are running
      Table_running <- Table_job_status %>%
        filter(status == "running")

      # how many cores have been used
      usingCore = sum(Table_running$core)

      # adjust the waiting list
      Table_waiting <- Table_job_status %>%
        filter(status == "pending") %>%
        arrange(priority, index)

      # get the waiting index
      WaitIndex = which(Table_waiting$index == current_index)

      #' if the model are in the first place
      #' if there are sufficient cores
      #' if `untilFinished` is FALSE, run the model
      #' otherwise, wait for a while and check the job log again
      running_check = WaitIndex == 1 & core <= (maxCore - usingCore) & !untilFinished

      if (running_check) {
        # update the progress bar
        setTxtProgressBar(pb, value = current_index)
        close(pb)
        break
      } else {
        # update progress bar
        setTxtProgressBar(pb, value = job_index - WaitIndex)
        # wait for a while to check the job log
        Sys.sleep(checkInt * WaitIndex + runif(1, 0,5))
      }
    }

    # start running the model --------------------------------------------------

    # print the start time
    start_time = Sys.time()
    message(str_glue("The task starts to run at {start_time}"))

    # update the job log
    update_job(
      current_index,
      status = "running"
    )

    tryCatch(
      expr = {
        # Adjust the status of the model as running.
        Table_job_status <- read_rds(log_file)
        Table_job_status[which(Table_job_status$index == job_index), "status"] = "running"
        write_rds(Table_job_status, log_file)

        # Run the model
        eval(parse(text = code_str))

        # Adjust the status of the model as completed.
        update_job(
          current_index,
          status = "completed"
        )

        return(NA)

      },
      error = function(e){

        # Adjust the status of the model as running.
        update_job(
          current_index,
          status = "failed"
        )

        # message error
        message(e)

        return(NA)
      },
      finally = {
        # print the end time
        end_time = Sys.time()
        message(str_glue("The task has done at {end_time}"))
        message(str_glue("It takes {round(as.numeric(end_time - start_time, units = 'hours'),2)} hours."))
      }
    )


  })

  Sys.sleep(9)

}
