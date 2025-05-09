% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bayesfactor-helper.R
\name{compare_best}
\alias{compare_best}
\title{Run the model comparison sequentially and calculate Bayes Factor}
\usage{
compare_best(pars, sample_path, sample_prefix, favorBF = 3, shuffle = FALSE)
}
\arguments{
\item{pars}{A list of parameters to compare. Each parameter is a list of values.}

\item{sample_path}{The path to store the sample.}

\item{sample_prefix}{The prefix of the sample.}

\item{favorBF}{The favor of the Bayes Factor. The default is \code{3}.}

\item{shuffle}{Logical. If \code{TRUE}, the function will shuffle the order of the model comparison. The default is \code{FALSE}.}
}
\value{
A table of the model comparison.
}
\description{
This function calculates the Bayes Factor between the best model and competing models. The favored model will replace the best model and compare with the next model.
}
