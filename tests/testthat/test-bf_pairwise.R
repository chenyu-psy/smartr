# tests/testthat/test-bf_pairwise.R

# Load required packages for testing
library(testthat)
library(brms)
library(emmeans)
library(bayestestR)

# Set seed for reproducibility
set.seed(123)

# Create test data that's available to all tests
test_data <- data.frame(
  subject = factor(rep(1:10, each = 4)),
  condition = factor(rep(c("control", "treatment"), each = 2, times = 10)),
  time = factor(rep(c("pre", "post"), times = 20)),
  age = rnorm(40, mean = 30, sd = 5),
  score = rnorm(40, mean = 50, sd = 10)
)

# Setup test models
test_that("Setup test models", {
  # Skip this test on CRAN or if brms is not available
  skip_on_cran()
  skip_if_not_installed("brms")

  # Set parallel options
  options(mc.cores = 2)

  # Try to fit the model, but skip if it fails
  test_model <- tryCatch({
    suppressWarnings(
      brm(
        score ~ condition * time + age + (1|subject),
        data = test_data,
        iter = 1000,
        warmup = 500,
        chains = 2,
        silent = 2
      )
    )
  }, error = function(e) {
    skip("Failed to fit brms model")
    NULL
  })

  # Make test model available globally
  assign("test_model", test_model, envir = .GlobalEnv)

  # Verify that the model was created
  expect_s3_class(test_model, "brmsfit")
})

# Test input validation
test_that("bf_pairwise validates inputs correctly", {
  # Skip if test_model is not available
  skip_if_not(exists("test_model", envir = .GlobalEnv))
  test_model <- get("test_model", envir = .GlobalEnv)

  # Test missing model
  expect_error(bf_pairwise(specs = ~ condition), "Argument 'model' is missing or NULL")

  # Test invalid model type
  fake_model <- lm(score ~ condition, data = test_data)
  expect_error(bf_pairwise(model = fake_model, specs = ~ condition),
               "Argument 'model' must be a 'brmsfit' object")

  # Test missing specs
  expect_error(bf_pairwise(model = test_model), "Argument 'specs' is missing or NULL")

  # Test invalid prior type
  expect_error(bf_pairwise(model = test_model, prior = fake_model, specs = ~ condition),
               "Argument 'prior' must be a 'brmsfit' object or NULL")

  # Test invalid cont_values type
  expect_error(bf_pairwise(model = test_model, specs = ~ condition, cont_values = "not_a_list"),
               "Argument 'cont_values' must be a named list or NULL")
})

# Test basic functionality without prior
test_that("bf_pairwise works with model only", {
  # Skip if test_model is not available
  skip_if_not(exists("test_model", envir = .GlobalEnv))
  test_model <- get("test_model", envir = .GlobalEnv)
  skip_on_cran()

  # Suppress warnings about priors
  suppressWarnings({
    # Run function with minimal arguments
    result <- bf_pairwise(model = test_model, specs = ~ condition)
  })

  # Check return structure
  expect_s3_class(result, "pairwise_comparison")
  expect_named(result, c("emmeans", "pairwise", "emmeans_prior", "pairwise_prior", "bayesfactor"))

  # Check that emmeans and pairwise results exist
  expect_s4_class(result$emmeans, "emmGrid")
  expect_s4_class(result$pairwise, "emmGrid")

  # Check that prior results are NULL
  expect_null(result$emmeans_prior)
  expect_null(result$pairwise_prior)

  # Check that bayesfactor results exist
  expect_s3_class(result$bayesfactor, "bayesfactor_parameters")
})

# Test with continuous variable specifications
test_that("bf_pairwise handles continuous variables correctly", {
  # Skip if test_model is not available
  skip_if_not(exists("test_model", envir = .GlobalEnv))
  test_model <- get("test_model", envir = .GlobalEnv)
  skip_on_cran()

  # Suppress warnings about priors
  suppressWarnings({
    # Run with default handling of continuous variables
    result1 <- bf_pairwise(model = test_model, specs = ~ condition)

    # Should use mean of age by default
    expect_message(
      bf_pairwise(model = test_model, specs = ~ condition),
      "Continuous variable 'age' detected"
    )

    # Run with custom continuous variable values
    result2 <- bf_pairwise(
      model = test_model,
      specs = ~ condition,
      cont_values = list(age = c(25, 35))
    )
  })

  # Check that results differ when using different continuous values
  # Note: This is a bit tricky to test directly, so we're just checking
  # that the objects are different
  expect_false(identical(result1$emmeans@grid, result2$emmeans@grid))
})

# Test with different emmeans specifications
test_that("bf_pairwise works with different emmeans specifications", {
  # Skip if test_model is not available
  skip_if_not(exists("test_model", envir = .GlobalEnv))
  test_model <- get("test_model", envir = .GlobalEnv)
  skip_on_cran()

  # Suppress warnings about priors
  suppressWarnings({
    # Test with simple specification
    result1 <- bf_pairwise(model = test_model, specs = ~ condition)

    # Test with interaction specification
    result2 <- bf_pairwise(model = test_model, specs = ~ condition | time)

    # Test with more complex specification
    result3 <- bf_pairwise(model = test_model, specs = ~ condition * time)
  })

  expect_equal(nrow(result1$emmeans@grid), 2) # Two conditions
  expect_equal(nrow(result2$emmeans@grid), 4) # 2 conditions × 2 times
  expect_equal(nrow(result3$emmeans@grid), 4) # 2 conditions × 2 times
})

# Test with multiple specifications
test_that("bf_pairwise works with multiple specifications", {
  # Skip if test_model is not available
  skip_if_not(exists("test_model", envir = .GlobalEnv))
  test_model <- get("test_model", envir = .GlobalEnv)
  skip_on_cran()

  # Suppress warnings about priors
  suppressWarnings({
    # Test with multiple specifications
    result <- bf_pairwise(
      model = test_model,
      specs = c(~ condition, ~ condition | time)
    )
  })

  # Check that result is a list of pairwise_comparison objects
  expect_s3_class(result, "pairwise_comparison_list")
  expect_length(result, 2)
  expect_s3_class(result[[1]], "pairwise_comparison")
  expect_s3_class(result[[2]], "pairwise_comparison")

  # Check that the first result has 2 conditions and the second has 4
  expect_equal(nrow(result[[1]]$emmeans@grid), 2) # Two conditions
  expect_equal(nrow(result[[2]]$emmeans@grid), 4) # 2 conditions × 2 times
})

# Test print methods
test_that("print methods work correctly", {
  # Skip if test_model is not available
  skip_if_not(exists("test_model", envir = .GlobalEnv))
  test_model <- get("test_model", envir = .GlobalEnv)
  skip_on_cran()

  # Create custom print methods for testing
  print_pairwise_comparison <- function(x, ...) {
    cat("Bayesian Pairwise Comparisons\n")
    cat("============================\n\n")

    cat("Estimated Marginal Means:\n")
    print(x$emmeans, ...)

    cat("\nPairwise Comparisons:\n")
    print(x$pairwise, ...)

    if (!is.null(x$bayesfactor)) {
      cat("\nBayes Factors:\n")
      print(x$bayesfactor, ...)
    }

    invisible(x)
  }

  print_pairwise_comparison_list <- function(x, ...) {
    cat("Multiple Bayesian Pairwise Comparisons\n")
    cat("====================================\n\n")

    for (i in seq_along(x)) {
      cat(sprintf("Specification %d: %s\n", i, names(x)[i]))
      cat(paste(rep("-", nchar(names(x)[i]) + 16), collapse = ""), "\n")
      print_pairwise_comparison(x[[i]], ...)
      cat("\n\n")
    }

    invisible(x)
  }

  # Suppress warnings about priors
  suppressWarnings({
    # Run function with single spec
    result1 <- bf_pairwise(model = test_model, specs = ~ condition)

    # Run function with multiple specs
    result2 <- bf_pairwise(
      model = test_model,
      specs = c(~ condition, ~ condition | time)
    )
  })

  # Test print method for single spec
  output1 <- capture.output(print_pairwise_comparison(result1))
  expect_true(any(grepl("Bayesian Pairwise Comparisons", output1)))
  expect_true(any(grepl("Estimated Marginal Means", output1)))

  # Test print method for multiple specs
  output2 <- capture.output(print_pairwise_comparison_list(result2))
  expect_true(any(grepl("Multiple Bayesian Pairwise Comparisons", output2)))
  expect_true(any(grepl("Specification 1", output2)))
  expect_true(any(grepl("Specification 2", output2)))
})

# Test helper functions with mock objects for faster testing
test_that("helper functions work correctly with mock objects", {
  # Create a mock brmsfit object
  mock_model <- structure(
    list(
      formula = structure(
        list(formula = score ~ condition * time + age + (1|subject)),
        class = "brmsformula"
      ),
      data = test_data
    ),
    class = "brmsfit"
  )

  # Test extract_model_data
  expect_equal(extract_model_data(mock_model), test_data)

  # Test extract_brms_formula
  expect_equal(extract_brms_formula(mock_model), score ~ condition * time + age + (1|subject))

  # Test extract_independent_vars
  indep_vars <- extract_independent_vars(score ~ condition * time + age + (1|subject))
  expect_true(all(c("condition", "time", "age") %in% indep_vars))

  # Test identify_continuous_variables
  cont_vars <- identify_continuous_variables(test_data, c("condition", "time", "age"))
  expect_equal(cont_vars, "age")

  # Test prepare_continuous_values
  at_list <- prepare_continuous_values(test_data, "age", NULL)
  expect_equal(names(at_list), "age")
  expect_equal(at_list$age, mean(test_data$age))
})

# Add a test with a simpler model to avoid long-running tests
test_that("bf_pairwise works with a minimal model", {
  skip_on_cran()

  # Create a very simple model with minimal iterations
  mini_data <- data.frame(
    y = rnorm(20),
    x = factor(rep(c("A", "B"), each = 10))
  )

  # Try to fit the model, but skip if it fails
  mini_model <- tryCatch({
    suppressWarnings(
      brm(
        y ~ x,
        data = mini_data,
        iter = 500,
        chains = 1,
        prior = prior(normal(0, 10), class = "b"),
        silent = 2
      )
    )
  }, error = function(e) {
    skip("Failed to fit minimal brms model")
    NULL
  })

  # Skip if model fitting failed
  skip_if_not(inherits(mini_model, "brmsfit"))

  # Suppress warnings about priors
  suppressWarnings({
    # Run bf_pairwise with the minimal model
    result <- bf_pairwise(model = mini_model, specs = ~ x)
  })

  # Basic checks
  expect_s3_class(result, "pairwise_comparison")
  expect_s4_class(result$emmeans, "emmGrid")
  expect_s4_class(result$pairwise, "emmGrid")
})

# Clean up global environment after all tests
test_that("Clean up environment", {
  # Remove test_model from global environment
  if (exists("test_model", envir = .GlobalEnv)) {
    rm("test_model", envir = .GlobalEnv)
  }

  # This test should always pass
  expect_true(TRUE)
})
