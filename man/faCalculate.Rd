% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/faCalculate.R
\name{faCalculate}
\alias{faCalculate}
\title{Automatically calculates parallel and factor analyses}
\usage{
faCalculate(
  faDat,
  cutoff,
  nFactors = NULL,
  nRound = 3,
  method = c("pa", "minres"),
  rotate = "oblimin"
)
}
\arguments{
\item{faDat}{Data frame to run analysis on}

\item{cutoff}{Cutoff value for loadings to appear on the tidy table.}

\item{nFactors}{Number of factors to extract, can also be a vector. If \code{NULL}, takes the recommended number of the parallel analysis}

\item{nRound}{Number of decimals to round to}

\item{method}{Method for factor analysis, can be a vector}

\item{rotate}{Rotation method if needed}
}
\description{
Dataframe needs to consist only of the items as columns and participants as rows
}
