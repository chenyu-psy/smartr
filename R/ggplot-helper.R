#' @title Geom errorbar with adjusted width
#' @description Create error bars with width adjusted based on group sizes
#' @param mapping Aesthetic mapping
#' @param data A data frame
#' @param group_var Name of the grouping variable (unquoted)
#' @param width Base width value to be multiplied by group size
#' @param ... Other arguments passed to geom_errorbar
#' @return A ggplot2 layer
#'
#' @importFrom ggplot2 geom_errorbar
#' @importFrom dplyr count rename left_join mutate select
#' @importFrom rlang enquo as_name
#'
geom_errorbar_adjusted <- function(mapping = NULL, data = NULL,
                                   group_var = NULL, width = 0.05,
                                   ...) {

  # Capture the group variable
  group_var_enquo <- rlang::enquo(group_var)

  # If data is provided, preprocess it
  if (!is.null(data) && !is.null(group_var)) {
    # Count occurrences for each level of the grouping variable
    counts <- data %>%
      dplyr::count(!!group_var_enquo) %>%
      dplyr::rename(group_count = n)

    # Join the counts back to the original data
    data <- data %>%
      dplyr::left_join(counts, by = rlang::as_name(group_var_enquo)) %>%
      dplyr::mutate(width = width * group_count) %>%
      dplyr::select(-group_count)
  }

  # Create the errorbar with the calculated width
  ggplot2::geom_errorbar(mapping = mapping, data = data, ...)
}

