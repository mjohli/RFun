#' Calculates mean from a frequency table
#'
#' This calculates the mean if given a vector with frequencies and a vector with weights for each frequency
#' @param x A frequency vector
#' @param weights A vector containing weights for all given frequencies
#' @export
#' @examples
#' meanFromFrequencies(3:5, c(2,1,3))
#'

meanFromFrequencies <- function(x, weights){
    sum(x * weights)/sum(x)
    }
