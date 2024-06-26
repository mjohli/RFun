% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/HSGcolors.R
\name{HSGcolors}
\alias{HSGcolors}
\title{HSG colors}
\usage{
HSGcolors(...)
}
\arguments{
\item{...}{Name(s) of HSG colors to retrieve, if empty: returns all names and values, if "all": returns only values to use}
}
\value{
A vector containing values of the specified colors
}
\description{
A vector containing the HSG corporate identity colors.
}
\examples{
HSGcolors()
HSGcolors("HSGgreen", "HSGblue")
HSGcolors("all")
}
