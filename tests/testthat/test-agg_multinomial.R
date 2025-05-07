# tests/testthat/test-agg_multinomial.R

test_that("agg_multinomial handles character vector inputs correctly", {
  # Create test data
  test_data <- data.frame(
    subject = rep(c("S1", "S2"), each = 3),
    condition = rep(c("A", "B", "C"), 2),
    resp1 = c(1, 0, 1, 0, 1, 0),
    resp2 = c(0, 1, 0, 1, 0, 1),
    resp3 = c(1, 1, 0, 0, 1, 1)
  )

  # Test with character vector for responses and group
  result <- agg_multinomial(
    data = test_data,
    responses = c("resp1", "resp2", "resp3"),
    group = c("subject", "condition")
  )

  # Check structure
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 6)  # 2 subjects × 3 conditions
  expect_equal(ncol(result), 6)  # 2 group vars + 3 response vars + nRet

  # Check column names
  expected_cols <- c("subject", "condition", "resp1", "resp2", "resp3", "nRet")
  expect_equal(colnames(result), expected_cols)

  # Check values
  expect_equal(result$nRet[result$subject == "S1" & result$condition == "A"], 2)
  expect_equal(result$resp1[result$subject == "S1" & result$condition == "A"], 1)
})

test_that("agg_multinomial handles tidyverse-style inputs correctly", {
  skip_if_not_installed("dplyr")

  # Create test data
  test_data <- data.frame(
    subject = rep(c("S1", "S2"), each = 3),
    condition = rep(c("A", "B", "C"), 2),
    resp1 = c(1, 0, 1, 0, 1, 0),
    resp2 = c(0, 1, 0, 1, 0, 1),
    resp3 = c(1, 1, 0, 0, 1, 1)
  )

  # Test with tidyselect for responses and unquoted variables for group
  result <- agg_multinomial(
    data = test_data,
    responses = dplyr::starts_with("resp"),
    group = c(subject, condition)  # Using unquoted variable names
  )

  # Check structure
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 6)  # 2 subjects × 3 conditions
  expect_equal(ncol(result), 6)  # 2 group vars + 3 response vars + nRet

  # Check column names
  expected_cols <- c("subject", "condition", "resp1", "resp2", "resp3", "nRet")
  expect_equal(colnames(result), expected_cols)
})

test_that("agg_multinomial combines responses into matrix when DV_name is provided", {
  # Create test data
  test_data <- data.frame(
    subject = rep(c("S1", "S2"), each = 3),
    condition = rep(c("A", "B", "C"), 2),
    resp1 = c(1, 0, 1, 0, 1, 0),
    resp2 = c(0, 1, 0, 1, 0, 1),
    resp3 = c(1, 1, 0, 0, 1, 1)
  )

  # Test with DV_name provided
  result <- agg_multinomial(
    data = test_data,
    responses = c("resp1", "resp2", "resp3"),
    group = c("subject", "condition"),
    DV_name = "responses"
  )

  # Check structure
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 6)  # 2 subjects × 3 conditions
  expect_equal(ncol(result), 4)  # 2 group vars + responses + nRet

  # Check column names
  expected_cols <- c("subject", "condition", "responses", "nRet")
  expect_equal(colnames(result), expected_cols)

  # Check that responses contains matrices
  expect_true(is.matrix(result$responses[[1]]))

  # Check matrix dimensions
  expect_equal(ncol(result$responses[[1]]), 3)

  # Check matrix column names (should be numeric)
  expect_equal(colnames(result$responses[[1]]), c("1", "2", "3"))
})

test_that("agg_multinomial keeps original column names when numeric=FALSE", {
  # Create test data
  test_data <- data.frame(
    subject = rep(c("S1", "S2"), each = 3),
    condition = rep(c("A", "B", "C"), 2),
    resp1 = c(1, 0, 1, 0, 1, 0),
    resp2 = c(0, 1, 0, 1, 0, 1),
    resp3 = c(1, 1, 0, 0, 1, 1)
  )

  # Test with numeric=FALSE
  result <- agg_multinomial(
    data = test_data,
    responses = c("resp1", "resp2", "resp3"),
    group = c("subject", "condition"),
    DV_name = "responses",
    numeric = FALSE
  )

  # Check matrix column names (should be original names)
  expect_equal(colnames(result$responses[[1]]), c("resp1", "resp2", "resp3"))
})

test_that("agg_multinomial works with no grouping variables", {
  # Create test data
  test_data <- data.frame(
    resp1 = c(1, 0, 1, 0, 1, 0),
    resp2 = c(0, 1, 0, 1, 0, 1),
    resp3 = c(1, 1, 0, 0, 1, 1)
  )

  # Test with no group
  result <- agg_multinomial(
    data = test_data,
    responses = c("resp1", "resp2", "resp3")
  )

  # Check structure
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)  # No grouping, so just one row
  expect_equal(ncol(result), 4)  # 3 response vars + nRet

  # Check values - update to match actual behavior
  expect_equal(result$nRet, 10)  # Total of all responses
  expect_equal(result$resp1, 3)
  expect_equal(result$resp2, 3)
  expect_equal(result$resp3, 4)
})

test_that("agg_multinomial handles custom nDV_name", {
  # Create test data
  test_data <- data.frame(
    subject = rep(c("S1", "S2"), each = 3),
    resp1 = c(1, 0, 1, 0, 1, 0),
    resp2 = c(0, 1, 0, 1, 0, 1)
  )

  # Test with custom nDV_name
  result <- agg_multinomial(
    data = test_data,
    responses = c("resp1", "resp2"),
    group = "subject",
    nDV_name = "total_responses"
  )

  # Check column names
  expect_true("total_responses" %in% colnames(result))
  expect_false("nRet" %in% colnames(result))

  # Check values
  expect_equal(result$total_responses[result$subject == "S1"], 3)
  expect_equal(result$total_responses[result$subject == "S2"], 3)
})

test_that("agg_multinomial handles NA values correctly", {
  # Create test data with NAs
  test_data <- data.frame(
    subject = rep(c("S1", "S2"), each = 2),
    resp1 = c(1, NA, 0, 1),
    resp2 = c(NA, 1, 1, NA)
  )

  # Test with NAs
  result <- agg_multinomial(
    data = test_data,
    responses = c("resp1", "resp2"),
    group = "subject"
  )

  # Check values (NAs should be treated as 0 for sums)
  expect_equal(result$resp1[result$subject == "S1"], 1)
  expect_equal(result$resp2[result$subject == "S1"], 1)
  expect_equal(result$nRet[result$subject == "S1"], 2)
})

test_that("agg_multinomial throws appropriate errors for invalid inputs", {
  # Create test data
  test_data <- data.frame(
    subject = rep(c("S1", "S2"), each = 3),
    resp1 = c(1, 0, 1, 0, 1, 0),
    resp2 = c(0, 1, 0, 1, 0, 1)
  )

  # Test with non-existent response variables - use character vector
  expect_error(
    agg_multinomial(
      data = test_data,
      responses = "nonexistent",  # Single non-existent column
      group = "subject"
    ),
    "variables are not found in the data"
  )

  # Test with non-existent group variables
  expect_error(
    agg_multinomial(
      data = test_data,
      responses = "resp1",
      group = "nonexistent"
    ),
    "variables are not found in the data"
  )
})



test_that("agg_multinomial works with a single response variable", {
  # Create test data
  test_data <- data.frame(
    subject = rep(c("S1", "S2"), each = 3),
    condition = rep(c("A", "B", "C"), 2),
    resp1 = c(1, 0, 1, 0, 1, 0)
  )

  # Test with a single response
  result <- agg_multinomial(
    data = test_data,
    responses = "resp1",
    group = c("subject", "condition")
  )

  # Check structure
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 6)  # 2 subjects × 3 conditions
  expect_equal(ncol(result), 4)  # 2 group vars + 1 response var + nRet

  # Check values
  expect_equal(result$resp1[result$subject == "S1" & result$condition == "A"], 1)
  expect_equal(result$nRet[result$subject == "S1" & result$condition == "A"], 1)
})

test_that("agg_multinomial handles tibble inputs", {
  skip_if_not_installed("tibble")

  # Create test tibble
  test_tibble <- tibble::tibble(
    subject = rep(c("S1", "S2"), each = 3),
    condition = rep(c("A", "B", "C"), 2),
    resp1 = c(1, 0, 1, 0, 1, 0),
    resp2 = c(0, 1, 0, 1, 0, 1)
  )

  # Test with tibble input
  result <- agg_multinomial(
    data = test_tibble,
    responses = c("resp1", "resp2"),
    group = c("subject", "condition")
  )

  # Check structure
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 6)  # 2 subjects × 3 conditions
  expect_equal(ncol(result), 5)  # 2 group vars + 2 response vars + nRet
})

test_that("agg_multinomial works with mixed input styles", {
  # Create test data
  test_data <- data.frame(
    subject = rep(c("S1", "S2"), each = 3),
    condition = rep(c("A", "B", "C"), 2),
    resp1 = c(1, 0, 1, 0, 1, 0),
    resp2 = c(0, 1, 0, 1, 0, 1),
    resp3 = c(1, 1, 0, 0, 1, 1)
  )

  # Test with character vector for responses and tidyverse-style for group
  result <- agg_multinomial(
    data = test_data,
    responses = c("resp1", "resp2", "resp3"),
    group = c(subject)  # Using unquoted variable name
  )

  # Check structure
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)  # 2 subjects
  expect_equal(ncol(result), 5)  # 1 group var + 3 response vars + nRet

  # Check column names
  expected_cols <- c("subject", "resp1", "resp2", "resp3", "nRet")
  expect_equal(colnames(result), expected_cols)
})

test_that("agg_multinomial handles zero responses correctly", {
  # Create test data with some zero responses
  test_data <- data.frame(
    subject = rep(c("S1", "S2"), each = 2),
    condition = rep(c("A", "B"), 2),
    resp1 = c(0, 0, 1, 0),
    resp2 = c(0, 1, 0, 0)
  )

  # Test with zero responses
  result <- agg_multinomial(
    data = test_data,
    responses = c("resp1", "resp2"),
    group = c("subject", "condition")
  )

  # Check values
  expect_equal(result$resp1[result$subject == "S1" & result$condition == "A"], 0)
  expect_equal(result$resp2[result$subject == "S1" & result$condition == "A"], 0)
  expect_equal(result$nRet[result$subject == "S1" & result$condition == "A"], 0)
})

test_that("agg_multinomial handles logical responses", {
  # Create test data with logical values
  test_data <- data.frame(
    subject = rep(c("S1", "S2"), each = 2),
    resp1 = c(TRUE, FALSE, TRUE, FALSE),
    resp2 = c(FALSE, TRUE, FALSE, TRUE)
  )

  # Test with logical responses
  result <- agg_multinomial(
    data = test_data,
    responses = c("resp1", "resp2"),
    group = "subject"
  )

  # Check values (TRUE should be counted as 1)
  expect_equal(result$resp1[result$subject == "S1"], 1)
  expect_equal(result$resp2[result$subject == "S1"], 1)
  expect_equal(result$nRet[result$subject == "S1"], 2)
})
