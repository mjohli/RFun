% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bfNam.R
\name{bfNam}
\alias{bfNam}
\title{A function that gives names of big five dimensions}
\usage{
bfNam(letter = FALSE, lowercase = TRUE)
}
\arguments{
\item{letter}{Boolean indicating whether a letter or the short form is needed}

\item{lowercase}{Boolean indicating whether dimension names should be all lowercase or title case}
}
\description{
Gives names of interest, maybe extended
}
\examples{
bfNam()
bfNam(TRUE)
bfNam(TRUE, FALSE)
bfNam(lowercase = FALSE)

}
