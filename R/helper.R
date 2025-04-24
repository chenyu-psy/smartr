#' Extract Independent Variables from an R Formula
#'
#' This function parses an R formula and extracts all independent variables
#' (predictors) from the right-hand side. It handles various formula types,
#' including those with random effects, interactions, nested functions, and
#' special operators.
#'
#' @param formula A formula object or a character string representing a formula.
#' @param simplify Logical. If TRUE, attempts to extract variable names from function
#'                calls like log(x). If FALSE, keeps the full function call. Default is FALSE.
#' @return A character vector containing the names of all independent variables.
#' @examples
#' extract_independent_vars("y ~ x1 + x2 + (1|group)")
#' extract_independent_vars("resp | trials(n) ~ v1 + v2 + (1 + v1 | subj)")
#' extract_independent_vars("y ~ log(x1) + x2*x3", simplify = TRUE)
#' @export
extract_independent_vars <- function(formula, simplify = FALSE) {
  # Input validation
  if (!inherits(formula, "formula") && !is.character(formula)) {
    stop("Input must be a formula object or a character string")
  }

  # Convert input to a formula object if it's a string
  if (is.character(formula)) {
    formula <- as.formula(formula)
  }

  # Extract the right-hand side of the formula
  formula_parts <- as.character(formula)

  # Determine which part contains the right-hand side
  if (length(formula_parts) >= 3) {
    # Standard formula like y ~ x or y | z ~ x
    rhs <- formula_parts[3]
  } else if (length(formula_parts) == 2) {
    # One-sided formula like ~ x + y
    rhs <- formula_parts[2]
  } else {
    stop("Invalid formula format: cannot extract right-hand side")
  }

  # Create a terms object from the right-hand side
  terms_obj <- terms(as.formula(paste("~", rhs)))

  # Extract all terms from the formula
  all_terms <- attr(terms_obj, "term.labels")

  # Remove random effects terms (containing "|")
  fixed_terms <- all_terms[!grepl("\\|", all_terms)]

  # Separate main effects and interaction terms
  main_effects <- fixed_terms[!grepl(":", fixed_terms)]
  interaction_terms <- fixed_terms[grepl(":", fixed_terms)]

  # Extract individual variables from interaction terms
  interaction_vars <- character(0)
  if (length(interaction_terms) > 0) {
    for (term in interaction_terms) {
      interaction_vars <- c(interaction_vars, unlist(strsplit(term, ":")))
    }
  }

  # Combine all variables and remove duplicates
  all_variables <- unique(c(main_effects, interaction_vars))

  # Optionally simplify function calls to extract variable names
  if (simplify) {
    # Extract variable names from function calls like log(x), poly(x, 2), etc.
    simplified_vars <- character(0)

    for (var in all_variables) {
      # Check if the term contains a function call
      if (grepl("\\(", var)) {
        # Extract variable names inside parentheses
        inner_vars <- gsub(".*\\(([^,)]+).*\\)", "\\1", var)
        simplified_vars <- c(simplified_vars, inner_vars)
      } else {
        simplified_vars <- c(simplified_vars, var)
      }
    }

    all_variables <- unique(simplified_vars)
  }

  return(all_variables)
}


#' Extract Dependent Variable(s) from an R Formula
#'
#' This function parses an R formula and extracts the dependent variable(s)
#' from the left-hand side. It handles various formula types, including
#' standard formulas, multivariate responses, and specialized formulas with
#' additional operators.
#'
#' @param formula A formula object or a character string representing a formula.
#' @return A character vector containing the name(s) of the dependent variable(s).
#'         Returns NULL for one-sided formulas (those starting with ~).
#' @examples
#' extract_dependent_vars("y ~ x1 + x2")
#' extract_dependent_vars("cbind(y1, y2) ~ x")
#' extract_dependent_vars("resp | trials(n) ~ v1 + v2")
#' extract_dependent_vars("~ x + y")  # Returns NULL (no dependent variable)
#' @export
extract_dependent_vars <- function(formula) {
  # Input validation
  if (!inherits(formula, "formula") && !is.character(formula)) {
    stop("Input must be a formula object or a character string")
  }

  # Convert input to a formula object if it's a string
  if (is.character(formula)) {
    formula <- as.formula(formula)
  }

  # Extract the formula parts
  formula_parts <- as.character(formula)

  # Check if it's a one-sided formula (no dependent variable)
  if (length(formula_parts) <= 2) {
    return(NULL)
  }

  # Extract the left-hand side (dependent variable part)
  lhs <- formula_parts[2]

  # Handle different types of left-hand sides
  if (grepl("cbind\\(", lhs)) {
    # Multivariate response like cbind(y1, y2)
    # Extract variables inside cbind()
    vars <- gsub("cbind\\((.*)\\)", "\\1", lhs)
    vars <- strsplit(vars, ",\\s*")[[1]]
    return(vars)
  } else if (grepl("\\|", lhs)) {
    # Handle specialized formulas like "resp | trials(n)"
    # Extract the part before the |
    vars <- strsplit(lhs, "\\s*\\|\\s*")[[1]][1]
    return(vars)
  } else {
    # Standard single response variable
    return(lhs)
  }
}

