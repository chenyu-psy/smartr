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
#' @param spec A formula specifying the marginal means to compute
#'   (see \code{\link[emmeans]{emmeans}}).
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
#' @details
#' Continuous variables are automatically detected as numeric or integer columns in the model's data
#' (excluding factors, logicals, and the response variable). By default, the function uses the mean
#' value of each continuous variable when computing marginal means.
#'
#' If \code{cont_values} is provided, its values override the default (mean) for those variables.
#'
#' If \code{prior} is provided, it is processed in the same way as \code{model} and used for
#' Bayes Factor computation via \code{bayestestR::bayesfactor_parameters}.
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
#' # Run pairwise comparisons
#' res <- bf_pairs(
#'   model = fit,
#'   prior = fit_prior,
#'   spec = ~ IV1 | IV2
#' )
#'
#' # View results
#' print(res$emmeans)
#' print(res$pairwise)
#' print(res$bayesfactor)
#' }
#'
#' @importFrom emmeans emmeans contrast
#' @importFrom stats formula
#' @importFrom bayestestR bayesfactor_parameters
#' @export
bf_pairs <- function(
    model,
    prior = NULL,
    spec,
    cont_values = NULL
) {

  # Validate input arguments
  validate_inputs(model, prior, spec, cont_values)

  # Extract model data
  model_data <- extract_model_data(model)

  # Get response variable name from model formula
  resp_var <- extract_response_variable(model)

  # Identify continuous variables
  cont_vars <- identify_continuous_variables(model_data, resp_var)

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

#' Validate input arguments
#'
#' @param model The brmsfit model
#' @param prior The prior model (if any)
#' @param spec The formula for emmeans
#' @param cont_values Continuous variable values (if any)
#'
#' @return NULL (throws error if validation fails)
#' @noRd
validate_inputs <- function(model, prior, spec, cont_values) {
  if (missing(model) || is.null(model)) {
    stop("Argument 'model' is missing or NULL.", call. = FALSE)
  }

  if (!inherits(model, "brmsfit")) {
    stop("Argument 'model' must be a 'brmsfit' object.", call. = FALSE)
  }

  if (missing(spec) || is.null(spec)) {
    stop("Argument 'spec' is missing or NULL.", call. = FALSE)
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

#' Extract response variable name from a brms model
#'
#' @param model A brmsfit object
#'
#' @return The name of the response variable
#' @noRd
extract_response_variable <- function(model) {
  # Fixed approach to extract response variable from brms model
  # Using the internal structure of brmsfit objects

  # Try to get the formula from the model
  if (inherits(model, "brmsfit")) {
    # Access the formula directly from the model object
    formula_str <- deparse(model$formula$formula)

    # Extract the response variable (left side of ~)
    resp_var <- strsplit(formula_str, "~")[[1]][1]
    resp_var <- trimws(resp_var)

    # If there are multiple terms (e.g., cbind(y1, y2)), take the first one
    if (grepl("\\(", resp_var)) {
      resp_var <- gsub(".*\\(([^,]+).*", "\\1", resp_var)
      resp_var <- trimws(resp_var)
    }

    if (resp_var != "") {
      return(resp_var)
    }
  }

  # Fallback method using all.vars
  formula_obj <- stats::formula(model)
  resp_var <- all.vars(formula_obj)[1]

  if (is.null(resp_var) || length(resp_var) == 0) {
    warning("Could not identify response variable from model formula.", call. = FALSE)
    resp_var <- character(0)
  }

  return(resp_var)
}

#' Identify continuous variables in the model data
#'
#' @param data The model data frame
#' @param resp_var The response variable name
#'
#' @return Character vector of continuous variable names
#' @noRd
identify_continuous_variables <- function(data, resp_var) {
  # Define variables to exclude (factors, logicals, and response)
  exclude_vars <- c(
    names(Filter(is.factor, data)),
    names(Filter(is.logical, data))
  )

  # Add response variable to exclusions if it exists in data
  if (length(resp_var) > 0 && resp_var %in% names(data)) {
    exclude_vars <- c(resp_var, exclude_vars)
  }

  # Identify continuous variables (numeric or integer)
  is_continuous <- function(x) is.numeric(x) || is.integer(x)
  cont_vars <- setdiff(
    names(Filter(is_continuous, data)),
    exclude_vars
  )

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
