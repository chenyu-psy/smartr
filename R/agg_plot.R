#' Aggregate Data for Plotting
#'
#' @description This function aggregates data for plotting by computing the mean, standard error, and confidence interval. It supports both between-subject and within-subject factors, and includes adjustments for within-subject variability (e.g., Morey correction).
#'
#' @param data A data frame containing the data to be aggregated.
#' @param y The variable to be aggregated (can be a string or tidyverse-style variable).
#' @param between The between-subject factor (can be a string or tidyverse-style variable). Default is `NULL`.
#' @param within The within-subject factor (can be a string or tidyverse-style variable). Default is `NULL`.
#' @param group The grouping variable (can be a string or tidyverse-style variable). Default is `NULL`.
#' @param ci The confidence interval level. Default is `0.95`.
#' @param zero.rm Logical indicating whether to remove cases where the mean and standard deviation are both zero. Default is `TRUE`.
#'
#' @return A data frame containing the aggregated data with the following columns:
#' - `mean`: The mean of the aggregated variable.
#' - `n`: The number of observations.
#' - `se`: The standard error of the mean.
#' - `ci`: The confidence interval.
#'
#' @importFrom rlang .data ensym sym
#' @importFrom dplyr across group_by summarize ungroup mutate n select distinct left_join filter all_of
#' @importFrom stats sd qt
#' @importFrom purrr possibly
#' @export
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   subject = rep(1:10, each = 2),
#'   condition = rep(c("A", "B"), times = 10),
#'   score = rnorm(20)
#' )
#' agg_plot(data, y = score, between = condition, group = subject)
#' }
agg_plot <- function(data, y, between = NULL, within = NULL, group = NULL, ci = 0.95, zero.rm = T) {

  # Input validation
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }
  if (ci <= 0 || ci >= 1) {
    stop("`ci` must be a value between 0 and 1.")
  }

  # Convert inputs to symbols
  possibly_null <- purrr::possibly(is.null, otherwise = FALSE)
  # Convert all inputs to symbols
  y <- ensym(y)
  between <- if (!possibly_null(between)) ensym(between)
  within <- if (!possibly_null(within)) ensym(within)
  group <- if (!possibly_null(group)) ensym(group)

  # Aggregate data at the participant level
  df <- data %>%
    group_by(across(c(!!group, !!between, !!within))) %>%
    summarize(sub_y = mean(!!y, na.rm = TRUE), .groups = "drop")

  # Adjust data for within-subject variability
  if (!is.null(within)) {
    df <- df %>%
      group_by(across(c(!!group, !!between))) %>%
      mutate(user_mean = mean(.data$sub_y, na.rm = TRUE)) %>%
      ungroup() %>%
      group_by(across(!!between)) %>%
      mutate(grand_mean = mean(.data$sub_y, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(y_adjusted = .data$sub_y - .data$user_mean + .data$grand_mean)
  } else {
    df <- df %>%
      mutate(y_adjusted = .data$sub_y)
  }

  # Aggregate data at the plot level
  df <- df %>%
    group_by(across(c(!!between, !!within))) %>%
    summarize(
      mean = mean(.data$y_adjusted, na.rm = TRUE),
      n = n(),
      se = sd(.data$y_adjusted, na.rm = TRUE) / sqrt(.data$n),
      ci = qt(1 - (1 - ci) / 2, .data$n - 1) * .data$se,
      .groups = "drop"
    )

  # Apply Morey correction for within-subject SE
  if (!is.null(within)) {
    if (!is.null(between)) {
      df <- df %>%
        group_by(across(!!between)) %>%
        mutate(
          morey_correction = sqrt(n() / (n() - 1)),
          se = .data$se * .data$morey_correction
        ) %>%
        ungroup()
    } else {
      df <- df %>%
        mutate(
          morey_correction = sqrt(n() / (n() - 1)),
          se = .data$se * .data$morey_correction
        )
    }
    df <- df %>% select(-morey_correction)
  }

  # Handle cases where mean = 0 and sd = 0
  filter_conditions <- data %>%
    group_by(across(c(!!between, !!within))) %>%
    summarize(
      mean = mean(!!y, na.rm = TRUE),
      sd = sd(!!y, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(.data$mean == 0, .data$sd == 0) %>%
    select(all_of(c(as.character(between), as.character(within)))) %>%
    distinct() %>%
    mutate(filter = TRUE)

  if (nrow(filter_conditions) > 0 & zero.rm) {
    df <- df %>%
      left_join(filter_conditions, by = c(as.character(between), as.character(within))) %>%
      mutate(
        filter = ifelse(is.na(.data$filter), FALSE, .data$filter),
        mean = ifelse(.data$filter, NA, .data$mean),
        se = ifelse(.data$filter, NA, .data$se),
        ci = ifelse(.data$filter, NA, .data$ci)
      ) %>%
      select(-.data$filter)
  }

  return(df)
}
