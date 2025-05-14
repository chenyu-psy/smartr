#' Resolve Column Names from Various Input Types
#'
#' @description
#' Helper function to robustly resolve column names from user input (character, symbol, quosure, or tidyselect expression).
#'
#' @param data A data frame.
#' @param vars A character vector, symbol, quosure, or tidyselect expression.
#'
#' @importFrom rlang is_symbol is_quosure as_string as_name get_expr
#' @importFrom dplyr select
#'
#' @return A character vector of column names.
#' @keywords internal
get_colnames <- function(data, vars) {
  # Accept NULL
  if (is.null(vars)) return(NULL)

  # Accept character vector directly
  if (is.character(vars)) {
    missing <- setdiff(vars, names(data))
    if (length(missing) > 0)
      stop("Column(s) not found in data: ", paste(missing, collapse = ", "))
    return(vars)
  }

  # Accept quosure or symbol or call (e.g., c(x, y))
  # Use tidyselect to resolve
  # Support for tidyselect expressions, symbols, and calls
  out <- tryCatch(
    tidyselect::eval_select(rlang::enquo(vars), data = data),
    error = function(e) {
      # Try as quosure (if vars is already a quosure)
      tryCatch(
        tidyselect::eval_select(vars, data = data),
        error = function(e2) {
          stop("Could not resolve columns for input: ", deparse(substitute(vars)), "\n", e2$message)
        }
      )
    }
  )
  names(data)[out]
}

#' Aggregate Data for Plotting (Between/Within-Subject Design)
#'
#' @description
#' Aggregates data for plotting by computing the mean, standard error, and confidence interval.
#' Supports both between-subject and within-subject factors, and applies Morey correction for within-subject designs.
#'
#' @param data A data frame containing the data to be aggregated.
#' @param y The variable to be aggregated (string or unquoted variable name).
#' @param between Between-subject factor(s) (string, unquoted variable name, or tidyselect expression). Default is `NULL`.
#' @param within Within-subject factor(s) (string, unquoted variable name, or tidyselect expression). Default is `NULL`.
#' @param group Subject/grouping variable(s) (string, unquoted variable name, or tidyselect expression). Default is `NULL`.
#' @param ci Confidence interval level (between 0 and 1). Default is `0.95`.
#' @param zero.rm Logical; if `TRUE`, removes cases where mean and sd are both zero. Default is `TRUE`.
#'
#' @return A data frame with columns: mean, n, se, ci, and grouping variables.
#' @keywords internal
#' @importFrom dplyr across group_by summarise ungroup mutate n select distinct left_join filter all_of
#' @importFrom rlang enquo as_name is_symbol is_quosure as_string get_expr
#' @importFrom stats sd qt
#'
#' @export
agg_plot <- function(data, y, between = NULL, within = NULL, group = NULL, ci = 0.95, zero.rm = TRUE) {
  # Capture arguments as quosures for flexible input
  y_quo       <- enquo(y)
  between_quo <- enquo(between)
  within_quo  <- enquo(within)
  group_quo   <- enquo(group)

  # Input validation
  if (!is.data.frame(data)) stop("`data` must be a data frame.")
  if (!is.numeric(ci) || length(ci) != 1 || ci <= 0 || ci >= 1) stop("`ci` must be a single value between 0 and 1.")

  y_name       <- tryCatch(as_name(y_quo), error = function(e) NULL)
  if (is.null(y_name) || !(y_name %in% names(data))) stop("`y` variable not found in data.")

  between_vars <- get_colnames(data, between_quo)
  within_vars  <- get_colnames(data, within_quo)
  group_vars   <- get_colnames(data, group_quo)

  # Check for overlap between grouping variables
  all_groupings <- c(between_vars, within_vars, group_vars)
  if (any(duplicated(all_groupings))) {
    stop("Grouping variables (between, within, group) must not overlap. Overlapping variable(s): ",
         paste(unique(all_groupings[duplicated(all_groupings)]), collapse = ", "))
  }

  # Define grouping variables for participant-level aggregation
  participant_group_vars <- c(group_vars, between_vars, within_vars)
  if (length(participant_group_vars) == 0) stop("At least one grouping variable (group, between, or within) must be specified.")

  # Check if each participant × between × within cell has only one row
  n_rows_per_cell <- data %>%
    group_by(across(all_of(participant_group_vars))) %>%
    summarise(n = n(), .groups = "drop") %>%
    pull(n)

  single_measure_per_cell <- all(n_rows_per_cell == 1)
  plot_group_vars <- c(between_vars, within_vars)

  if (single_measure_per_cell) {
    # SIMPLE AGGREGATION, SKIP MOREY CORRECTION
    df_participant <- data %>%
      group_by(across(all_of(participant_group_vars))) %>%
      summarise(mean_value = mean(.data[[y_name]], na.rm = TRUE), .groups = "drop")

    df_plot <- df_participant %>%
      group_by(across(all_of(plot_group_vars))) %>%
      summarise(
        mean = mean(mean_value, na.rm = TRUE),
        n    = n(),
        se   = sd(mean_value, na.rm = TRUE) / sqrt(n),
        ci   = qt(1 - (1 - ci) / 2, n - 1) * se,
        .groups = "drop"
      )

  } else {
    # ORIGINAL PATH: WITHIN-SUBJECTS (MOREY)
    df_participant <- data %>%
      group_by(across(all_of(participant_group_vars))) %>%
      summarise(sub_y = mean(.data[[y_name]], na.rm = TRUE), .groups = "drop")

    # Adjust for within-subject variability (Morey correction)
    if (length(within_vars)!=0) {
      df_participant <- df_participant %>%
        group_by(across(all_of(c(group_vars, between_vars)))) %>%
        mutate(user_mean = mean(sub_y, na.rm = TRUE)) %>%
        ungroup() %>%
        group_by(across(all_of(between_vars))) %>%
        mutate(grand_mean = mean(sub_y, na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(y_adjusted = sub_y - user_mean + grand_mean)
    } else {
      df_participant <- df_participant %>%
        mutate(y_adjusted = sub_y)
    }

    # Aggregate at plot level
    if (length(group_vars) > 0) {
      df_plot <- df_participant %>%
        group_by(across(all_of(plot_group_vars))) %>%
        summarise(
          mean = mean(y_adjusted, na.rm = TRUE),
          n    = n(),
          se   = sd(y_adjusted, na.rm = TRUE) / sqrt(n),
          ci   = qt(1 - (1 - ci) / 2, n - 1) * se,
          .groups = "drop"
        )
    } else if (length(plot_group_vars) == 0) {
      df_plot <- df_participant
    }

    # Morey correction for within-subject SE
    if (length(within_vars)!=0) {
      if (length(between_vars)!=0) {
        df_plot <- df_plot %>%
          group_by(across(all_of(between_vars))) %>%
          mutate(
            morey_correction = sqrt(n() / (n() - 1)),
            se = se * morey_correction
          ) %>%
          ungroup() %>%
          select(-morey_correction)
      } else {
        df_plot <- df_plot %>%
          mutate(
            morey_correction = sqrt(n / (n - 1)),
            se = se * morey_correction
          ) %>%
          select(-morey_correction)
      }
    }
  }

  # Remove cases where mean and sd are both zero, if requested
  if (zero.rm) {
    filter_group_vars <- plot_group_vars
    df_zero <- data %>%
      group_by(across(all_of(filter_group_vars))) %>%
      summarise(
        mean = mean(.data[[y_name]], na.rm = TRUE),
        sd = sd(.data[[y_name]], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(mean == 0, sd == 0) %>%
      select(all_of(filter_group_vars)) %>%
      mutate(filter = TRUE)

    join_vars <- intersect(names(df_plot), names(df_zero))
    if (nrow(df_zero) > 0) {
      df_plot <- df_plot %>%
        left_join(df_zero, by = join_vars) %>%
        mutate(
          filter = ifelse(is.na(filter), FALSE, filter),
          mean = ifelse(filter, NA, mean),
          se = ifelse(filter, NA, se),
          ci = ifelse(filter, NA, ci)
        ) %>%
        select(-filter)
    }
  }

  return(df_plot)
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
  responses_enquo <- enquo(responses)
  group_enquo <- enquo(group)

  # Initialize response_vars
  response_vars <- NULL

  # Handle different input types for responses
  if (is_character(quo_get_expr(responses_enquo))) {
    # Character vector input
    response_vars <- quo_get_expr(responses_enquo)
  } else {
    # For tidyverse-style input, we need to safely evaluate it
    response_vars <- tryCatch({
      # Try to select the columns using tidyselect
      dplyr::select(data, !!responses_enquo) %>% colnames()
    }, error = function(e) {
      # If that fails, just return the expression and let validation handle it
      as.character(quo_get_expr(responses_enquo))
    })
  }

  # Validate responses
  if (is.null(response_vars) || length(response_vars) == 0) {
    stop("No response variables found. Check your 'responses' specification.")
  }

  # Initialize group_vars
  group_vars <- NULL

  # Handle different input types for group
  if (is_null(quo_get_expr(group_enquo))) {
    group_vars <- NULL
  } else if (is_character(quo_get_expr(group_enquo))) {
    # Character vector input
    group_vars <- quo_get_expr(group_enquo)
  } else {
    # For tidyverse-style input, we need to handle it differently
    group_vars <- tryCatch({
      # Try to select the columns using tidyselect
      dplyr::select(data, !!group_enquo) %>% colnames()
    }, error = function(e) {
      # If that fails, try to extract the variable names from the expression
      expr <- quo_get_expr(group_enquo)
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

