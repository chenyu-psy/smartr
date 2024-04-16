#' add new columns to data frame using formulas
#'
#' @description
#' This function is used to add new columns to a data frame using formulas.
#'
#'
#'@param .data A data frame or data frame extension (e.g. a tibble).
#'
#'@param formulas A formula or a vector of formulas to be used for calculating new columns.
#'
#'@return A data frame with new columns added.
#'
#'@export
mutate_form <- function(.data, formulas) {

  # convert formulas to list
  formulas = c(formulas)

  # calculate the formula one by one
  for (i in 1:length(formulas)) {

    # get the formula
    form = formulas[[i]]

    # extract dependent and independent variables
    DV = all.vars(form)[1]
    IV = all.vars(form)[-1]

    # check if all the independent variables are in the data frame
    if (!all(IV %in% names(.data))) {
      stop("Some independent variables are not in the data frame.")
    }
    # extract right-side formula
    right_from = as.character(form[-2]) %>% stringr::str_remove("~")
    # add the dependent variable to the data frame
    .data[, DV] = with(.data, eval(parse(text = right_from)))
  }

  return(.data)

}
