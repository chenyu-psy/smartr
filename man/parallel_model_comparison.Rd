% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parallel_model_comparsion.R
\name{parallel_model_comparison}
\alias{parallel_model_comparison}
\title{Perform Parallel Model Comparisons}
\usage{
parallel_model_comparison(
  fun,
  pars,
  form_fun = NULL,
  model_fun = NULL,
  prior_fun = NULL,
  args,
  sampler_args = list(cores = 1, repetition = 10, maxiter = 1000),
  model_name = "Model",
  model_path,
  sample_path,
  maxCore = NULL,
  sample_check = TRUE
)
}
\arguments{
\item{fun}{Function. The function used to fit models (supports `brms` and `bmm` models).}

\item{pars}{List. A named list of parameter values to compare.}

\item{form_fun}{Function. A function to generate model formulas (default is `NULL`).}

\item{model_fun}{Function. A function to generate models (supports `bmm` models only, default is `NULL`).}

\item{prior_fun}{Function. A function to generate prior distributions (default is `NULL`).}

\item{args}{List. Additional arguments passed to the model-fitting function.}

\item{sampler_args}{List. Arguments for `bridge_sampler` (default: `{cores = 1, repetition = 10, maxiter = 1000}`).}

\item{model_name}{Character. The base name for models (default: `"Model"`).}

\item{model_path}{Character. The directory to store models.}

\item{sample_path}{Character. The directory to store bridge sampling results.}

\item{maxCore}{Integer. The maximum number of cores to use (default is system limit).}

\item{sample_check}{Logical. If `TRUE`, checks whether samples exist before running (default: `TRUE`).}
}
\value{
A data frame containing the model comparison table.
}
\description{
This function compares multiple models using parallel execution. It fits models, runs bridge sampling,
and calculates Bayes Factors.
}
