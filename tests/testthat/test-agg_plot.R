library(testthat)
library(smartr)  # Load your package
library(dplyr)

# Test data
test_data <- data.frame(
  participant = rep(1:20, each = 3),
  condition = rep(sample(c("A","B"), 20, replace = TRUE), each = 3),
  setsize = rep(c(4,5,6), times = 20)
) %>%
  mutate(
    value = case_when(
      condition == "A" & setsize == 4 ~ rnorm(n(), mean = 5, sd = 1),
      condition == "A" & setsize == 5 ~ rnorm(n(), mean = 6, sd = 1),
      condition == "A" & setsize == 6 ~ rnorm(n(), mean = 7, sd = 1),
      condition == "B" & setsize == 4 ~ rnorm(n(), mean = 4, sd = 1),
      condition == "B" & setsize == 5 ~ rnorm(n(), mean = 5, sd = 1),
      condition == "B" & setsize == 6 ~ rnorm(n(), mean = 6, sd = 1)
    )
  )

# Test 1: Basic functionality with tidyverse-style variables
test_that("agg_plot works with tidyverse-style variables", {
  result <- agg_plot(test_data, y = value, between = condition, within = setsize, group = participant)

  expect_s3_class(result, "data.frame")
  expect_true(all(c("condition", "setsize", "mean", "se", "ci") %in% colnames(result)))
  expect_equal(nrow(result), 6)  # 2 condition * 3 setsize
})

# Test 2: Basic functionality with string-style variables
test_that("agg_plot works with string-style variables", {
  result <- agg_plot(test_data, y = "value", between = "condition", within = "setsize", group = "participant")

  expect_s3_class(result, "data.frame")
  expect_true(all(c("condition", "setsize", "mean", "se", "ci") %in% colnames(result)))
  expect_equal(nrow(result), 6)  # 2 condition * 3 setsize
})

# Test 3: Mixed-style variables
test_that("agg_plot works with mixed-style variables", {
  result <- agg_plot(test_data, y = "value", between = "condition", within = "setsize", group = participant)

  expect_s3_class(result, "data.frame")
  expect_true(all(c("condition", "setsize", "mean", "se", "ci") %in% colnames(result)))
  expect_equal(nrow(result), 6)  # 2 condition * 3 setsize
})

# Test 4: No within-subject factor
test_that("agg_plot works without within-subject factor", {
  result <- agg_plot(test_data, y = value, between = condition, group = participant)

  expect_s3_class(result, "data.frame")
  expect_true(all(c("condition", "mean", "se", "ci") %in% colnames(result)))
  expect_equal(nrow(result), 2)  # 2 conditions
})

# Test 5: No between-subject factor
test_that("agg_plot works without between-subject factor", {
  result <- agg_plot(test_data, y = value, within = setsize, group = participant)

  expect_s3_class(result, "data.frame")
  expect_true(all(c("setsize", "mean", "se", "ci") %in% colnames(result)))
  expect_equal(nrow(result), 3)  # 3 set sizes
})

# Test 7: Missing data
test_that("agg_plot handles missing data", {
  missing_data <- test_data
  missing_data$value[1:5] <- NA

  result <- agg_plot(missing_data, y = value, between = condition, within = setsize)

  expect_s3_class(result, "data.frame")
  expect_true(all(!is.na(result$mean)))  # Means should still be computed
})

# Test 8: Invalid input types
test_that("agg_plot throws errors for invalid input types", {
  expect_error(agg_plot(test_data, y = 123, between = condition))  # y is not a column name
  expect_error(agg_plot(test_data, y = value, between = 123))  # between is not a column name
  expect_error(agg_plot(test_data, y = value, within = 123))  # within is not a column name
})

