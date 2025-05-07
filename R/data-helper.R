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
agg_plot <- function(data, y, between = NULL, within = NULL, group = NULL, ci = 0.95, zero.rm = TRUE) {

  # Input validation
  if (!is.data.frame(data)) stop("`data` must be a data frame.")
  if (ci <= 0 || ci >= 1) stop("`ci` must be a value between 0 and 1.")

  # Possibly function to handle NULL values gracefully
  possibly_null <- purrr::possibly(is.null, otherwise = FALSE)

  # Function to convert tidyverse-style variable vectors to character vectors
  var2string <- function(data,vars) {
    select(data,{{vars}}) %>% names()
  }

  # Convert inputs to symbols (supporting both string and tidyverse-style variables)
  y <- ensym(y)
  between <- if (!possibly_null(between)) var2string(data, {{between}}) else NULL
  within <- if (!possibly_null(within)) var2string(data, {{within}}) else NULL
  group <- if (!possibly_null(group)) var2string(data, {{group}}) else NULL

  # Define grouping variables dynamically
  group_vars <- c(group, between, within)

  # Aggregate data at the participant level
  df <- data %>%
    group_by(across(all_of(group_vars))) %>%
    summarize(sub_y = mean(!!y, na.rm = TRUE), .groups = "drop")

  # Adjust data for within-subject variability
  if (!possibly_null(within)) {
    df <- df %>%
      group_by(across(all_of(c(group, between)))) %>%
      mutate(user_mean = mean(.data$sub_y, na.rm = TRUE)) %>%
      ungroup() %>%
      group_by(across(all_of(between))) %>%
      mutate(grand_mean = mean(.data$sub_y, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(y_adjusted = .data$sub_y - .data$user_mean + .data$grand_mean)
  } else {
    df <- df %>% mutate(y_adjusted = .data$sub_y)
  }

  # Aggregate data at the plot level
  df <- df %>%
    group_by(across(all_of(c(between, within)))) %>%
    summarize(
      mean = mean(.data$y_adjusted, na.rm = TRUE),
      n = n(),
      se = sd(.data$y_adjusted, na.rm = TRUE) / sqrt(n),
      ci = qt(1 - (1 - ci) / 2, n - 1) * .data$se,
      .groups = "drop"
    )

  # Apply Morey correction for within-subject SE
  if (!possibly_null(within)) {
    if (!possibly_null(between)) {
      df <- df %>%
        group_by(across(all_of(between))) %>%
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
    group_by(across(all_of(c(between, within)))) %>%
    summarize(
      mean = mean(!!y, na.rm = TRUE),
      sd = sd(!!y, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(.data$mean == 0, .data$sd == 0) %>%
    select(all_of(c(between, within))) %>%
    distinct() %>%
    mutate(filter = TRUE)

  # Ensure left join works even if some grouping vars are NULL
  join_vars <- intersect(names(df), names(filter_conditions))

  if (nrow(filter_conditions) > 0 & zero.rm) {
    df <- df %>%
      left_join(filter_conditions, by = join_vars) %>%
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




#' Aggregate data using a multinomial distribution
#'
#' @description
#' This function aggregates data for multinomial analysis by counting responses across
#' specified categories. It supports both standard and tidyverse-style variable input
#' through tidy evaluation.
#'
#' @param data A data frame containing the data to be aggregated
#' @param responses Variables to be treated as responses. Can be provided as:
#'   * A character vector of column names
#'   * Unquoted variable names using tidyverse-style selection (e.g., `starts_with("resp")`)
#' @param group Variables to group by. Can be provided as:
#'   * A character vector of column names
#'   * Unquoted variable names (e.g., `c(subject, condition)`)
#'   * NULL for no grouping (default)
#' @param DV_name The name for the dependent variable column in the output. If NULL (default),
#'   the response variables will not be combined into a matrix.
#' @param nDV_name The name for the column containing the total number of responses (default: "nRet")
#' @param numeric Logical. If TRUE (default), rename the matrix columns to sequential numbers.
#'   If FALSE, keep original response variable names.
#'
#' @return A data frame containing the aggregated data with:
#'   * The grouping variables (if any)
#'   * Either individual response counts or a matrix of responses (depending on DV_name)
#'   * A column with the total number of responses
#'
#' @importFrom rlang .data enquo is_character is_null quo_get_expr sym
#' @importFrom dplyr select summarise mutate across all_of group_by ungroup
#' @importFrom tidyr pivot_longer pivot_wider
#'
#' @export
#'
agg_multinomial <- function(
    data,
    responses,
    group = NULL,
    DV_name = NULL,
    nDV_name = "nRet",
    numeric = TRUE) {

  # Capture inputs with tidy evaluation
  responses_enquo <- rlang::enquo(responses)
  group_enquo <- rlang::enquo(group)

  # Initialize response_vars
  response_vars <- NULL

  # Handle different input types for responses
  if (rlang::is_character(rlang::quo_get_expr(responses_enquo))) {
    # Character vector input
    response_vars <- rlang::quo_get_expr(responses_enquo)
  } else {
    # For tidyverse-style input, we need to safely evaluate it
    response_vars <- tryCatch({
      # Try to select the columns using tidyselect
      dplyr::select(data, !!responses_enquo) %>% colnames()
    }, error = function(e) {
      # If that fails, just return the expression and let validation handle it
      as.character(rlang::quo_get_expr(responses_enquo))
    })
  }

  # Validate responses
  if (is.null(response_vars) || length(response_vars) == 0) {
    stop("No response variables found. Check your 'responses' specification.")
  }

  # Initialize group_vars
  group_vars <- NULL

  # Handle different input types for group
  if (rlang::is_null(rlang::quo_get_expr(group_enquo))) {
    group_vars <- NULL
  } else if (rlang::is_character(rlang::quo_get_expr(group_enquo))) {
    # Character vector input
    group_vars <- rlang::quo_get_expr(group_enquo)
  } else {
    # For tidyverse-style input, we need to handle it differently
    group_vars <- tryCatch({
      # Try to select the columns using tidyselect
      dplyr::select(data, !!group_enquo) %>% colnames()
    }, error = function(e) {
      # If that fails, try to extract the variable names from the expression
      expr <- rlang::quo_get_expr(group_enquo)
      if (is.call(expr) && identical(expr[[1]], as.name("c"))) {
        # Handle c(var1, var2) syntax
        var_names <- as.character(expr[-1])
        return(var_names)
      } else {
        # Single variable
        return(as.character(expr))
      }
    })
  }

  # Validate that all specified columns exist in the data
  all_vars <- c(response_vars, group_vars)
  missing_vars <- setdiff(all_vars, colnames(data))
  if (length(missing_vars) > 0) {
    stop("The following variables are not found in the data: ",
         paste(missing_vars, collapse = ", "))
  }

  # Add response column to group for aggregation
  new_group <- c(group_vars, "response")

  # Aggregate responses
  data_sum <- data %>%
    # Select only the columns we need
    dplyr::select(dplyr::all_of(c(response_vars, group_vars))) %>%
    # Convert to long format
    tidyr::pivot_longer(
      cols = dplyr::all_of(response_vars),
      names_to = "response",
      values_to = "value"
    ) %>%
    # Group by the specified variables and response
    dplyr::group_by(dplyr::across(dplyr::all_of(new_group))) %>%
    # Calculate sum of responses for each group
    dplyr::summarise(
      Resp = sum(.data$value, na.rm = TRUE),
      .groups = "drop"
    )

  # Calculate total responses per group
  if (!is.null(group_vars)) {
    data_sum <- data_sum %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
      dplyr::mutate(!!nDV_name := sum(.data$Resp, na.rm = TRUE)) %>%
      dplyr::ungroup()
  } else {
    data_sum <- data_sum %>%
      dplyr::mutate(!!nDV_name := sum(.data$Resp, na.rm = TRUE))
  }

  # Convert back to wide format
  data_sum <- data_sum %>%
    tidyr::pivot_wider(
      names_from = .data$response,
      values_from = .data$Resp
    )

  # Prepare output based on whether DV_name is provided
  if (is.null(DV_name)) {
    # Return individual response columns
    data_output <- data_sum %>%
      dplyr::select(dplyr::all_of(group_vars), dplyr::all_of(response_vars), !!nDV_name)
  } else {
    # Combine responses into a matrix
    resp_matrix <- as.matrix(data_sum[, response_vars])
    resp_matrix[is.na(resp_matrix)] <- 0

    # Rename columns if numeric is TRUE
    if (numeric) {
      colnames(resp_matrix) <- 1:ncol(resp_matrix)
    }

    # Create output with matrix column
    data_output <- data_sum %>%
      dplyr::select(dplyr::all_of(group_vars), !!nDV_name)

    # Add the matrix column separately to ensure proper storage
    # Use a list column to store matrices
    data_output[[DV_name]] <- lapply(seq_len(nrow(resp_matrix)), function(i) {
      resp_matrix[i, , drop = FALSE]
    })

    # Reorder columns to match expected order
    data_output <- data_output %>%
      dplyr::select(dplyr::all_of(group_vars), !!rlang::sym(DV_name), !!nDV_name)
  }

  return(data_output)
}


#' Relabel Factor or Character Vectors
#'
#' This function relabels the values of a factor or character vector based on the provided labels.
#'
#' @param x A factor or character vector to be relabeled.
#' @param labels A named vector or a named list specifying the new labels. Each name in `labels` must correspond to a unique value in `x`.
#' @param factor Logical. If `TRUE` (default), the output is returned as a factor; otherwise, as a character vector.
#' @return A factor or character vector with relabeled values.
#' @examples
#' relabel(c("low", "medium", "high"), list("low" = "L", "medium" = "M", "high" = "H"))
#' relabel(factor(c("low", "medium", "high")), c("low" = "L", "medium" = "M", "high" = "H"))
#' relabel(c("apple", "banana", "cherry"), c("apple" = "A", "banana" = "B", "cherry" = "C"), factor = FALSE)
#' @export
#'
relabel <- function(x, labels, factor = TRUE) {

  # Ensure `x` is a factor, or character vector
  if (!is.factor(x) && !is.character(x)) {
    stop("`x` must be a factor, or character vector.")
  }

  # Ensure `labels` is a vector or a list that can be converted to a vector
  if (!is.vector(labels) && !is.list(labels) || is.vector(labels) && is.null(names(labels))) {
    stop("`labels` must be a vector with names or a list.")
  }


  # If `labels` is a list, ensure each element has only one value
  if (is.list(labels) && any(lengths(labels) != 1)) {
    stop("Each element of `labels` must have exactly one value.")
  }

  # Convert named list to named vector if necessary
  if (is.list(labels)) {
    labels <- unlist(labels, use.names = TRUE)
  }

  # Convert factors to characters for processing
  if (is.factor(x)) {
    x <- as.character(x)
  }

  # Check if all unique non-NA values in `x` are present in `labels`
  if (!all(unique(x) %in% names(labels))) {
    missing_vals <- setdiff(unique(x), names(labels))

    if (any(is.na(missing_vals))) {
      missing_vals <- missing_vals[!is.na(missing_vals)] # remove NA values from `missing_vals`
      warning("There is an NA value in `x` that will not be relabeled.")
    }

    if (length(missing_vals) > 0) {
      warning(paste("The following values in `x` are missing from `labels` and will not be relabeled: ", paste(missing_vals, collapse = ", "), ".", sep = ""))

      # Add missing values to `labels` with the same names
      labels <- c(labels, setNames(as.character(missing_vals), as.character(missing_vals)))
    }

  }


  # If `x` is character, ensure `labels` has names
  if (is.character(x) && is.null(names(labels))) {
    stop("If `x` is a character vector, `labels` must have names corresponding to unique values in `x`.")
  }

  # Ensure `x` is a character vector if `factor = FALSE`
  if (is.factor(x)) {
    x <- as.character(x)
  }

  # Relabel using factors or direct mapping
  if (factor) {
    return(factor(x, levels = names(labels), labels = labels))
  } else {
    return(unname(labels[x]))
  }
}

