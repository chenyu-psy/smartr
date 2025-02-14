#' Process Function Arguments
#'
#' @description
#' Converts function arguments (symbols or strings) into character vectors.
#' This is useful for functions that need to handle both string and symbol inputs.
#'
#' @param arg A function argument (symbol, string, or `NULL`).
#' @param data A data frame (used for validation).
#'
#' @return A character vector of variable names, or `NULL` if the input is `NULL`.
#'
#' @keywords internal
#' @noRd
process_arg <- function(arg, data = NULL) {
  # Capture the argument expression
  expr <- rlang::enexpr(arg)

  # Return NULL if input is NULL
  if (rlang::is_null(expr)) return(NULL)

  # Handle symbols and strings
  if (rlang::is_symbol(expr)) {
    return(rlang::as_name(expr))
  }

  if (is.character(expr)) {
    return(expr)
  }

  # Handle c() calls (e.g., c(x, y) or c("x", "y"))
  if (rlang::is_call(expr) && rlang::call_name(expr) == "c") {
    return(purrr::map_chr(rlang::call_args(expr), rlang::as_name))
  }

  # Handle other cases (e.g., direct column names)
  return(rlang::as_name(expr))
}
