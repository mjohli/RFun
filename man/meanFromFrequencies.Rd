% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/meanFromFrequencies.R
\name{meanFromFrequencies}
\alias{meanFromFrequencies}
\title{Calculates mean from a frequency table}
\usage{
meanFromFrequencies(x, weights)
}
\arguments{
\item{x}{A frequency vector}

\item{weights}{A vector containing weights for all given frequencies}
}
\description{
This calculates the mean if given a vector with frequencies and a vector with weights for each frequency
}
\examples{
meanFromFrequencies(3:5, c(2,1,3))

}
