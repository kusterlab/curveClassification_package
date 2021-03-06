% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generateFunList.R
\name{generateFunctionList}
\alias{generateFunctionList}
\title{generateFunctionList}
\usage{
generateFunctionList(functions = vector(), pattern = vector())
}
\arguments{
\item{functions:}{a character vector that contains names of functions that exist in the global environment}

\item{pattern:}{a character vector that represent patterns of the dataset to whom the functions should be applied. If pattern is of length one the same pattern is applied to all functions. Otherwise pattern has to be of the same length as functions.}
}
\description{
Returns a list, that contains all functions specified in functions with the corresponding pattern.
}
