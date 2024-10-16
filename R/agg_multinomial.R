
#' Aggregate data for the model using a multinomial distribution
#'
#' @description This function aggregates data for the model using a multinomial distribution.
#'
#' @param data A data frame containing the data to be aggregated
#' @param responses A character vector containing the response variables
#' @param group The grouping variable
#' @param DV_name The name of the dependent variable, if NULL, the response variables will not be combined.
#' @param nDV_name The name of the number of retrieval
#' @param numeric A logical value indicating whether to rename the columns of the dependent variable
#'
#' @return A data frame containing the aggregated data
#'
#' @importFrom rlang .data
#'
#' @export
#'
agg_multinomial <- function(
    data,
    responses,
    group = NULL,
    DV_name = "y",
    nDV_name = "nRet",
    numeric = TRUE) {

  # Check if responses is a character vector
  if (!base::is.character(responses)) {
    base::stop("responses must be a character vector")
  }

  # check the number of responses
  if (base::length(responses) < 2) {
    base::stop("responses must have at least two elements")
  }

  # Add response column to group
  new_group = c(group, "response")

  # Aggregate responses
  data_sum <- data %>%
    dplyr::select(dplyr::all_of(responses), dplyr::all_of(group)) %>%
    tidyr::pivot_longer(cols = dplyr::all_of(responses), names_to = "response", values_to = "value") %>%
    dplyr::summarise(
      Resp = sum(.data$value, na.rm = TRUE),
      .by = dplyr::all_of(new_group)) %>%
    dplyr::mutate(
      {{nDV_name}} := base::sum(.data$Resp, na.rm = TRUE),
      .by = dplyr::all_of(group)) %>%
    tidyr::pivot_wider(names_from = .data$response, values_from = .data$Resp)

  if(is.null(DV_name)) {
    data_output <- data_sum %>%
      dplyr::select(dplyr::all_of(group), dplyr::all_of(responses), {{nDV_name}})
  } else {

    resp_matrix <- base::as.matrix(data_sum[, responses])
    resp_matrix[base::is.na(resp_matrix)] <- 0

    # Rename columns if numeric is TRUE
    if (numeric) base::colnames(resp_matrix) <- 1:base::ncol(resp_matrix)
    data_output <- data_sum %>%
      dplyr::mutate({{DV_name}} := resp_matrix) %>%
      dplyr::select(dplyr::all_of(group), {{DV_name}}, {{nDV_name}})

  }

  return(data_output)

}
