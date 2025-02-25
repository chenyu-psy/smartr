library(testthat)
library(smartr)


test_that("relabel correctly transforms factor and character vectors", {

  # Test case 1: Relabel character vector using named list
  expect_equal(
    relabel(c("low", "medium", "high"), list("low" = "L", "medium" = "M", "high" = "H")),
    factor(c("L", "M", "H"), levels = c("L", "M", "H"))
  )

  # Test case 2: Relabel factor vector using named vector
  expect_equal(
    relabel(factor(c("low", "medium", "high")), c("low" = "L", "medium" = "M", "high" = "H")),
    factor(c("L", "M", "H"), levels = c("L", "M", "H"))
  )

  # Test case 3: Relabel character vector with factor = FALSE
  expect_equal(
    relabel(c("low", "medium", "high"), c("low" = "L", "medium" = "M", "high" = "H"), factor = FALSE),
    c("L", "M", "H")
  )

  # Test case 4: Relabel factor vector with factor = FALSE
  expect_equal(
    relabel(factor(c("low", "medium", "high")), c("low" = "L", "medium" = "M", "high" = "H"), factor = FALSE),
    c("L", "M", "H")
  )

  # Test case 5: Relabel with missing values in `x`
  expect_warning(
    relabel(c("low", "medium", "high", "unknown"), c("low" = "L", "medium" = "M", "high" = "H")),
    "The following values in `x` are missing from `labels`"
  )

  # Test case 6: Handle NA values properly
  expect_warning(
    relabel(c("low", "medium", NA), c("low" = "L", "medium" = "M")),
    "There is an NA value in `x` that will not be relabeled."
  )

  # Test case 7: Incorrect input type for `x` (numeric vector)
  expect_error(
    relabel(1:3, c("1" = "A", "2" = "B", "3" = "C")),
    "`x` must be a factor, or character vector."
  )

  # Test case 8: Incorrect input type for `labels` (unnamed vector)
  expect_error(
    relabel(c("low", "medium", "high"), c("L", "M", "H")),
    "`labels` must be a vector with names or a list."
  )

  # Test case 9: Incorrect input type for `labels` (list with multiple values per key)
  expect_error(
    relabel(c("low", "medium", "high"), list("low" = c("L", "LL"), "medium" = "M", "high" = "H")),
    "Each element of `labels` must have exactly one value."
  )
})
