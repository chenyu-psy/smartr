
#' Aggregate data for plotting
#'
#'@description This function aggregates data for plotting. It computes the mean, standard error, and confidence interval of the data.
#'
#'@param data A data frame containing the data to be aggregated
#'@param y The variable to be aggregated
#'@param between The between-subject factor
#'@param within The within-subject factor
#'@param group The grouping variable
#'@param ci The confidence interval
#'
#'@return A data frame containing the aggregated data
#'
#'@importFrom rlang .data
#'
#'@export
#'
agg_plot <- function(data, y, between = NULL, within = NULL, group = NULL, ci = 0.95) {

  ### first aggregate the data on the level of participants
  df <- data %>%
    dplyr::group_by(dplyr::across(c({{group}}, {{between}}, {{within}}))) %>%
    dplyr::summarize(sub_y = base::mean({{y}}, na.rm = T)) %>% # When you have an env-variable that is a character vector
    dplyr::ungroup()

  ### adjust the data to compute the within-subject se
  if (!(is.null(within))) {
    df <- df %>%
      dplyr::group_by(dplyr::across(c({{group}}, {{between}}))) %>%
      dplyr::mutate(user_mean = base::mean(.data$sub_y, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(dplyr::across(c({{between}}))) %>%
      dplyr::mutate(grand_mean = base::mean(.data$sub_y, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(y_adjusted = .data$sub_y - .data$user_mean + .data$grand_mean)
  } else{
    df <- df %>%
      dplyr::mutate(y_adjusted = .data$sub_y)
  }

  ### aggregate data on level of plot
  df <- df %>%
    dplyr::group_by(dplyr::across(c({{between}}, {{within}}))) %>%
    dplyr::summarize(
      mean = base::mean(.data$y_adjusted, na.rm = TRUE),
      n = dplyr::n(),
      se = stats::sd(.data$y_adjusted, na.rm = TRUE) / base::sqrt(.data$n),
      ci = stats::qt(1 - (1 - ci) / 2, .data$n - 1) * .data$se,
    ) %>%
    dplyr::ungroup()

  ### add the Morey-correction to the se
  if (!(is.null(within))) {
    if (!(is.null(between))) {
      df <- df %>%
        dplyr::group_by(dplyr::across(c({{between}}))) %>%
        dplyr::mutate(
          morey_correction = base::sqrt(dplyr::n() / (dplyr::n() - 1)),
          se = .data$se * .data$morey_correction
        ) %>%
        dplyr::ungroup()
    } else{
      df <- df %>%
        dplyr::mutate(
          morey_correction = base::sqrt(dplyr::n() / (dplyr::n() - 1)),
          se = .data$se * .data$morey_correction
        )
    }
    df <- df %>% dplyr::select(-.data$morey_correction)
  }

  ### correct the sd, se and ci for the condition where mean=0, and sd=0
  filter_conditions <- data %>%
    dplyr::group_by(dplyr::across(c({{between}}, {{within}}))) %>%
    dplyr::summarize(
      mean = base::mean({{y}}, na.rm = TRUE),
      sd = stats::sd({{y}}, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$mean == 0, .data$sd==0) %>%
    dplyr::select({{between}}, {{within}}) %>%
    dplyr::distinct() %>%
    dplyr::mutate(filter = TRUE)

  if (nrow(filter_conditions) > 0) {
    df <- df %>%
      dplyr::left_join(filter_conditions) %>%
      dplyr::mutate(
        mean = dplyr::if_else(.data$filter, NA, .data$mean),
        se = dplyr::if_else(.data$filter, NA, .data$se),
        ci = dplyr::if_else(.data$filter, NA, .data$ci)
      ) %>%
      dplyr::select(-.data$filter)
  }

  ### return df
  return(df)
}
