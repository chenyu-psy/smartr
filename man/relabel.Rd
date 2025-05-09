% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data-helper.R
\name{relabel}
\alias{relabel}
\title{Relabel Factor or Character Vectors}
\usage{
relabel(x, labels, factor = TRUE)
}
\arguments{
\item{x}{A factor or character vector to be relabeled.}

\item{labels}{A named vector or a named list specifying the new labels. Each name in \code{labels} must correspond to a unique value in \code{x}.}

\item{factor}{Logical. If \code{TRUE} (default), the output is returned as a factor; otherwise, as a character vector.}
}
\value{
A factor or character vector with relabeled values.
}
\description{
This function relabels the values of a factor or character vector based on the provided labels.
}
\examples{
relabel(c("low", "medium", "high"), list("low" = "L", "medium" = "M", "high" = "H"))
relabel(factor(c("low", "medium", "high")), c("low" = "L", "medium" = "M", "high" = "H"))
relabel(c("apple", "banana", "cherry"), c("apple" = "A", "banana" = "B", "cherry" = "C"), factor = FALSE)
}
