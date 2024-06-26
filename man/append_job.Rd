% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/append_job.R
\name{append_job}
\alias{append_job}
\title{Append a new job to the job log}
\usage{
append_job(
  name = NULL,
  cores = 1,
  untilFinished = FALSE,
  priority = 0,
  path = NULL
)
}
\arguments{
\item{name}{The name of the job.}

\item{cores}{The number of cores used by the job.}

\item{untilFinished}{Boolean value indicating whether the job should not run
until previous jobs are finished.}

\item{priority}{The priority of the job.}

\item{path}{The path to the job log.}
}
\description{
This function will append a new job to the job log.
}
