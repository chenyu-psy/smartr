#' Read json data and transform it into a data frame
#'
#' @description This function reads a list of json files and combines them into a single data frame.
#'
#' @param files A vector of json files to read
#'
#' @return a data frame combined all the data
#'
#' @export
#'
read_json_data <- function(files) {

  # Create an empty string to store the data
  combined_text = ""

  # Iterate over each file in the list of files to read
  combined_text <- purrr::map_vec(files, .f = brio::read_file, .progress = TRUE)
  single_text <- stringr::str_c(combined_text, collapse = "")
  modified_text <- gsub("\\}\\]\\s{0,}\\[\\{", "\\},\\{", single_text, perl = TRUE)
  data <- jsonlite::fromJSON(modified_text)

  # Return the combined data frame
  return(data)

}
