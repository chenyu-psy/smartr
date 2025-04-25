#' Pairwise Bayesian Comparisons from a brms Model
#'
#' @description
#' Computes estimated marginal means, pairwise comparisons, and Bayes Factors for a fitted
#' \code{brmsfit} model. This function automates the process of conducting Bayesian pairwise
#' comparisons while handling continuous predictors appropriately.
#'
#' @param model A fitted \code{brmsfit} object (posterior model).
#' @param prior Optional fitted \code{brmsfit} object representing the prior-only model.
#'   If provided, it is used for Bayes Factor computation.
#' @param specs A formula or list of formulas specifying the marginal means to compute
#'   (see \code{\link[emmeans]{emmeans}}). Multiple formulas can be provided using
#'   \code{c(~ formula1, ~ formula2)}.
#' @param cont_values Optional named list specifying values of continuous variables at which
#'   to estimate marginal means (e.g., \code{list(age = c(25, 40))}).
#'
#' @return A list with class "pairwise_comparison" containing:
#' \describe{
#'   \item{emmeans}{An \code{emmGrid} object with the estimated marginal means for the posterior model.}
#'   \item{pairwise}{Pairwise comparisons of the marginal means for the posterior model.}
#'   \item{emmeans_prior}{An \code{emmGrid} object with the estimated marginal means for the prior model, if provided.}
#'   \item{pairwise_prior}{Pairwise comparisons for the prior model, if provided.}
#'   \item{bayesfactor}{Bayes Factors for each pairwise comparison.}
#' }
#'
#' If multiple formulas are provided in \code{specs}, returns a list with class
#' "pairwise_comparison_list" containing the results for each formula.
#'
#' @details
#' Continuous variables are automatically detected from the model formula and identified as numeric
#' or integer columns in the model's data. By default, the function uses the mean value of each
#' continuous variable when computing marginal means.
#'
#' If \code{cont_values} is provided, its values override the default (mean) for those variables.
#'
#' If \code{prior} is provided, it is processed in the same way as \code{model} and used for
#' Bayes Factor computation via \code{bayestestR::bayesfactor_parameters}.
#'
#' When \code{specs} is a list of formulas, the function processes each formula separately
#' and returns a list of results.
#'
#' @examples
#' \dontrun{
#' library(brms)
#' library(emmeans)
#' library(bayestestR)
#'
#' # Create example data
#' data <- data.frame(
#'   subject = factor(rep(1:20, each = 4)),
#'   IV1 = factor(rep(c("A", "B"), each = 2, times = 20)),
#'   IV2 = factor(rep(c("X", "Y"), times = 40)),
#'   ContVar = rnorm(80, 10, 3),
#'   DV = rnorm(80, 5, 1.5)
#' )
#'
#' # Fit models
#' fit <- brm(DV ~ IV1 * IV2 * ContVar + (1|subject),
#'            data = data, iter = 1000, chains = 2)
#' fit_prior <- brm(DV ~ IV1 * IV2 * ContVar + (1|subject),
#'                  data = data, sample_prior = "only", iter = 1000, chains = 2)
#'
#' # Run pairwise comparisons with a single spec
#' res1 <- bf_pairwise(
#'   model = fit,
#'   prior = fit_prior,
#'   specs = ~ IV1 | IV2
#' )
#'
#' # Run pairwise comparisons with multiple specs
#' res2 <- bf_pairwise(
#'   model = fit,
#'   prior = fit_prior,
#'   specs = c(~ IV1 | IV2, ~ IV2 | IV1)
#' )
#'
#' # View results
#' print(res1$emmeans)
#' print(res1$pairwise)
#' print(res1$bayesfactor)
#' }
#'
#' @importFrom emmeans emmeans contrast
#' @importFrom stats formula
#' @importFrom bayestestR bayesfactor_parameters
#' @export
bf_pairwise <- function(
    model,
    prior = NULL,
    specs,
    cont_values = NULL
) {
  # Validate input arguments
  validate_inputs(model, prior, specs, cont_values)

  # Handle multiple specs
  if (is.list(specs) && !inherits(specs, "formula") && length(specs) > 1) {
    results_list <- lapply(specs, function(spec) {
      bf_pairwise(model = model, prior = prior, specs = spec, cont_values = cont_values)
    })
    names(results_list) <- sapply(specs, function(spec) deparse(spec))
    class(results_list) <- "pairwise_comparison_list"
    return(results_list)
  }

  # For a single spec, continue with the original logic
  spec <- specs

  # Extract model data
  model_data <- extract_model_data(model)

  # Get independent variables from model formula
  # For brmsfit objects, we need to extract the formula properly
  model_formula <- extract_brms_formula(model)
  indep_vars <- extract_independent_vars(model_formula)

  # Identify continuous variables among the independent variables
  cont_vars <- identify_continuous_variables(model_data, indep_vars)

  # Prepare 'at' argument for emmeans with continuous variable values
  at_list <- prepare_continuous_values(model_data, cont_vars, cont_values)

  # Compute emmeans and pairwise comparisons for posterior model
  posterior_results <- compute_emmeans_and_pairs(model, spec, at_list)

  # Process prior model if provided
  prior_results <- NULL
  if (!is.null(prior)) {
    prior_results <- compute_emmeans_and_pairs(prior, spec, at_list)
  }

  # Compute Bayes Factors
  bf_results <- compute_bayes_factors(posterior_results$pairs,
                                      prior_pairs = prior_results$pairs)

  # Return results as a structured list
  structure(
    list(
      emmeans = posterior_results$emmeans,
      pairwise = posterior_results$pairs,
      emmeans_prior = if (!is.null(prior_results)) prior_results$emmeans else NULL,
      pairwise_prior = if (!is.null(prior_results)) prior_results$pairs else NULL,
      bayesfactor = bf_results
    ),
    class = "pairwise_comparison"
  )
}

#' Extract formula from a brmsfit object
#'
#' @param model A brmsfit object
#'
#' @return A formula object
#' @noRd
extract_brms_formula <- function(model) {
  if (!inherits(model, "brmsfit")) {
    stop("Model must be a brmsfit object", call. = FALSE)
  }

  # Try different ways to extract the formula
  if (!is.null(model$formula) && !is.null(model$formula$formula)) {
    # The formula is stored in model$formula$formula for brmsfit objects
    return(model$formula$formula)
  } else if (!is.null(model$formula)) {
    # Sometimes it might be directly in model$formula
    return(model$formula)
  } else {
    # Fallback to using the formula method
    return(stats::formula(model))
  }
}




#' Validate input arguments
#'
#' @param model The brmsfit model
#' @param prior The prior model (if any)
#' @param specs The formula or list of formulas for emmeans
#' @param cont_values Continuous variable values (if any)
#'
#' @return NULL (throws error if validation fails)
#' @noRd
validate_inputs <- function(model, prior, specs, cont_values) {
  if (missing(model) || is.null(model)) {
    stop("Argument 'model' is missing or NULL.", call. = FALSE)
  }

  if (!inherits(model, "brmsfit")) {
    stop("Argument 'model' must be a 'brmsfit' object.", call. = FALSE)
  }

  if (missing(specs) || is.null(specs)) {
    stop("Argument 'specs' is missing or NULL.", call. = FALSE)
  }

  # Handle both single formula and list of formulas
  if (inherits(specs, "formula") || is.character(specs)) {
    # Single formula or character string is fine
  } else if (is.list(specs)) {
    # Check if all elements in the list are formulas or character strings
    if (!all(sapply(specs, function(x) inherits(x, "formula") || is.character(x)))) {
      stop("All elements in 'specs' must be formula objects or character strings.", call. = FALSE)
    }
  } else {
    stop("'specs' must be a formula, character string, or a list of formulas or character strings.", call. = FALSE)
  }

  if (!is.null(prior) && !inherits(prior, "brmsfit")) {
    stop("Argument 'prior' must be a 'brmsfit' object or NULL.", call. = FALSE)
  }

  if (!is.null(cont_values) && !is.list(cont_values)) {
    stop("Argument 'cont_values' must be a named list or NULL.", call. = FALSE)
  }
}


#' Extract data from a brms model
#'
#' @param model A brmsfit object
#'
#' @return The data frame used for model fitting
#' @noRd
extract_model_data <- function(model) {
  if (!is.null(model$data)) {
    return(model$data)
  } else if (!is.null(model$fit@frame)) {
    return(model$fit@frame)
  } else {
    stop("Could not retrieve data from model object.", call. = FALSE)
  }
}

#' Identify continuous variables among the independent variables
#'
#' @param data The model data frame
#' @param indep_vars Character vector of independent variable names
#'
#' @return Character vector of continuous variable names
#' @noRd
identify_continuous_variables <- function(data, indep_vars) {
  # Initialize vector for continuous variables
  cont_vars <- character(0)

  # Check each independent variable
  for (var in indep_vars) {
    # Skip if variable is not in data
    if (!var %in% names(data)) {
      next
    }

    # Check if variable is numeric or integer (continuous)
    if (is.numeric(data[[var]]) || is.integer(data[[var]])) {
      # Ensure it's not a factor stored as numeric
      if (!is.factor(data[[var]]) && !is.logical(data[[var]])) {
        cont_vars <- c(cont_vars, var)
      }
    }
  }

  return(cont_vars)
}


#' Prepare values for continuous variables
#'
#' @param data The model data frame
#' @param cont_vars Character vector of continuous variable names
#' @param cont_values User-provided values (if any)
#'
#' @return A list of values for continuous variables
#' @noRd
prepare_continuous_values <- function(data, cont_vars, cont_values) {
  at_list <- list()

  if (length(cont_vars) > 0) {
    for (var in cont_vars) {
      # Use user-provided values if available, otherwise use mean
      if (!is.null(cont_values) && !is.null(cont_values[[var]])) {
        at_list[[var]] <- cont_values[[var]]
      } else {
        var_mean <- mean(data[[var]], na.rm = TRUE)
        at_list[[var]] <- var_mean
        message(sprintf(
          "Continuous variable '%s' detected. Using mean (%.3f) for EMMs. Override using 'cont_values'.",
          var, var_mean
        ))
      }
    }
  }

  return(at_list)
}

#' Compute emmeans and pairwise comparisons
#'
#' @param model A brmsfit model
#' @param spec Formula for emmeans
#' @param at_list List of values for continuous variables
#'
#' @return A list containing emmeans and pairs objects
#' @noRd
compute_emmeans_and_pairs <- function(model, spec, at_list) {
  # Compute emmeans with or without 'at' argument
  emm <- if (length(at_list) > 0) {
    emmeans::emmeans(model, spec, at = at_list)
  } else {
    emmeans::emmeans(model, spec)
  }

  # Compute pairwise comparisons
  pairs_emm <- emmeans::contrast(emm, method = "pairwise")

  return(list(emmeans = emm, pairs = pairs_emm))
}

#' Compute Bayes Factors for pairwise comparisons
#'
#' @param posterior_pairs Pairwise comparisons from posterior model
#' @param prior_pairs Pairwise comparisons from prior model (if any)
#'
#' @return Bayes factor results
#' @noRd
compute_bayes_factors <- function(posterior_pairs, prior_pairs = NULL) {
  if (!is.null(prior_pairs)) {
    bf_results <- bayestestR::bayesfactor_parameters(posterior_pairs, prior = prior_pairs)
  } else {
    bf_results <- bayestestR::bayesfactor_parameters(posterior_pairs)
  }

  return(bf_results)
}

#' Print method for pairwise_comparison objects
#'
#' @param x A pairwise_comparison object
#' @param ... Additional arguments passed to print methods
#'
#' @return x invisibly
#' @export
print.pairwise_comparison <- function(x, ...) {
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

#' Print method for pairwise_comparison_list objects
#'
#' @param x A pairwise_comparison_list object
#' @param ... Additional arguments passed to print methods
#'
#' @return x invisibly
#' @export
print.pairwise_comparison_list <- function(x, ...) {
  cat("Multiple Bayesian Pairwise Comparisons\n")
  cat("====================================\n\n")

  for (i in seq_along(x)) {
    cat(sprintf("Specification %d: %s\n", i, names(x)[i]))
    cat(paste(rep("-", nchar(names(x)[i]) + 16), collapse = ""), "\n")
    print(x[[i]], ...)
    cat("\n\n")
  }

  invisible(x)
}
