% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/constructors.R
\name{cattonum_df}
\alias{cattonum_df}
\title{Constructor for class cattonum_df}
\usage{
cattonum_df(x = NULL)
}
\arguments{
\item{x}{\code{NULL} (the default), or a tibble or data.frame.}
}
\value{
Either an empty data frame (if \code{x} is \code{NULL}), or \code{x}.
In both cases, the class is \code{c("cattonum_df", "data.frame")}.
}
\description{
Constructor for class cattonum_df
}
\examples{
cattonum_df(iris)
cattonum_df()
}
