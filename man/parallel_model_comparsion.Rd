% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parallel_model_comparsion.R
\name{parallel_model_comparsion}
\alias{parallel_model_comparsion}
\title{Do all the model comparisons.}
\usage{
parallel_model_comparsion(
  fun = brms::brm,
  pars,
  form_fun = NULL,
  model_fun = NULL,
  prior_fun = NULL,
  args,
  favorBF = 3,
  model_name = "Model",
  model_path,
  sample_path,
  bf_path,
  maxCore = NULL,
  sample_check = TRUE
)
}
\arguments{
\item{fun}{The function to run the model. The default is `brms::brm`.}

\item{pars}{A list of parameters to compare. Each parameter is a list of values.}

\item{form_fun}{The function to generate the formula. The default is `NULL`.}

\item{model_fun}{The function to generate the model. This parameter only supports the `bmm` model.}

\item{prior_fun}{The function to generate the prior. The default is `NULL`.}

\item{args}{The arguments of the function to run in parallel.}

\item{favorBF}{The favor of the Bayes Factor. The default is `3`.}

\item{model_name}{The name of the model. The default is `Model`.}

\item{model_path}{The path to store the model.}

\item{sample_path}{The path to store the sample.}

\item{bf_path}{The path to store the Bayes Factor.}

\item{maxCore}{The maximum number of cores that can be used to run the function.}

\item{sample_check}{Logical. If `TRUE`, the function will check whether the sample exists. The default is `TRUE`.}
}
\value{
A table of the model comparison.
}
\description{
This function is used to compare the models. The function will run the model in parallel and calculate the Bayes Factor.
}
