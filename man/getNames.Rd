% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getNames.R
\name{getNames}
\alias{getNames}
\title{A function that connects series and character names}
\usage{
getNames(name, lowerCase = "None")
}
\arguments{
\item{name}{The name of either a series character or series title}

\item{lowerCase}{Indicates whether the given and the returned names should be all lowercase. Default is 'None' making no lower case, 'Series' and 'Characters' give and take series title and characters respectively in lower case, 'Both' gives and takes everything in lower case.}
}
\description{
This gives the character names appearing in a given series and vice versa
}
\examples{
getNames(name = "HIMYM")
getNames("Ted")
getNames("ted", lowerCase = "both")

}
