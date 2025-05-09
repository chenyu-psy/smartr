% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jatos-helper.R
\name{get_JATOS_data}
\alias{get_JATOS_data}
\title{Download data from JATOS}
\usage{
get_JATOS_data(
  token,
  url = "https://coglab.xyz/jatos/api/v1/results",
  batchId,
  dataPath = NULL,
  attachments = FALSE
)
}
\arguments{
\item{token}{A character string containing the API token for authentication. You can create a new token
on JATOS. See: https://www.jatos.org/JATOS-API.html#personal-access-tokens}

\item{url}{A character string specifying the server address. The default value is the address of the lab server.}

\item{batchId}{A character vector or numeric vector containing one or more unique codes for batch sessions in the study.}

\item{dataPath}{A character string specifying the path used to save data. If NULL, data will be saved
in a "JATOS_DATA" folder in the working directory.}

\item{attachments}{A logical value indicating whether to download attachments. Default is FALSE.}
}
\value{
A data frame containing metadata about the downloaded JATOS results.
}
\description{
This function downloads result data from a JATOS (Just Another Tool for Online Studies) server
using an API token and specified batch ID. It then unzips and extracts the relevant data,
including metadata, file names, and file contents. The function can download all data for specified
batch IDs or only download missing data based on comparison with local files.
}
\examples{
\dontrun{
# Download data for specified batches
data <- get_JATOS_data(
  token = "your_api_token",
  batchId = c("batch_id_1", "batch_id_2"),
  dataPath = "./data/",
  attachments = FALSE
)
}

}
