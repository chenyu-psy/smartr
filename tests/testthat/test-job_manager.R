# Load required packages
library(testthat)
library(smartr)
library(dplyr)

# Setup: Define a temporary directory for testing
test_path <- tempdir()
file <- file.path(test_path, "job_log.rds")

# ---- Test: init_job() ----
test_that("init_job creates a valid job log file", {
  init_job(test_path)
  expect_true(file.exists(file))

  # Check if the file contains an empty data frame with correct columns
  job_log <- readRDS(file)
  expect_s3_class(job_log, "data.frame")
  expect_identical(colnames(job_log), c("index", "name", "cores", "untilFinished",
                                        "priority", "status", "startTime", "endTime", "duration"))
})

# ---- Test: append_job() ----
test_that("append_job adds a job to the job log", {
  init_job(test_path)  # Ensure the job log is initialized
  job_log_before <- view_job(path = test_path)

  append_job(name = "TestJob", cores = 2, untilFinished = TRUE, priority = 1, path = test_path)

  job_log_after <- view_job(path = test_path)

  expect_equal(nrow(job_log_after), nrow(job_log_before) + 1)
  expect_true("TestJob" %in% job_log_after$name)
  expect_true(all(c("index", "name", "cores", "untilFinished", "priority", "status", "startTime", "endTime", "duration") %in% colnames(job_log_after)))
})

# ---- Test: view_job() ----
test_that("view_job retrieves job information", {
  job_log <- view_job(path = test_path)

  # Ensure at least one job exists
  expect_true(nrow(job_log) > 0)

  # Retrieve job by name and index
  job_by_name <- view_job("TestJob", path = test_path)
  job_by_index <- view_job(1, path = test_path)

  expect_s3_class(job_by_name, "data.frame")
  expect_s3_class(job_by_index, "data.frame")
  expect_true(nrow(job_by_name) > 0)
  expect_true(nrow(job_by_index) > 0)
})

# ---- Test: update_job() ----
test_that("update_job changes job status correctly", {
  update_job(1, "running", path = test_path)
  job_log <- view_job(1, path = test_path)

  expect_equal(job_log$status, "running")
  expect_false(is.na(job_log$startTime))

  update_job(1, "completed", path = test_path)
  job_log <- view_job(1, path = test_path)

  expect_equal(job_log$status, "completed")
  expect_false(is.na(job_log$endTime))
  expect_false(is.na(job_log$duration))
})

# ---- Test: remove_job() ----
test_that("remove_job deletes a job from the log", {
  job_log_before <- view_job(path = test_path)

  remove_job(1, path = test_path)

  job_log_after <- view_job(path = test_path)

  expect_equal(nrow(job_log_after), nrow(job_log_before) - 1)
  expect_false(1 %in% job_log_after$index)
})

# Cleanup: Remove test job log file
unlink(file.path(test_path, "job_log.rds"))
