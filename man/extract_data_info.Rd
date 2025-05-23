% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jatos-helper.R
\name{extract_data_info}
\alias{extract_data_info}
\title{Extract Information from Multiple Data Files}
\usage{
extract_data_info(metaData, info, warn = FALSE)
}
\arguments{
\item{metaData}{A data frame or tibble containing at least a column named 'file' with paths to data files.}

\item{info}{A character vector of keys to extract from each data file.}

\item{warn}{Logical. Whether to show warnings during extraction. Default is FALSE.}
}
\value{
A tibble containing the original metadata plus columns for each extracted key.
}
\description{
This function reads specified keys from multiple data files and returns them as a tibble with
the extracted information added as columns.
}
\examples{
\dontrun{
  # Assuming metaData is a tibble with a 'file' column containing file paths
  result <- extract_data_info(metaData, c("author", "date", "version"))
}

}
