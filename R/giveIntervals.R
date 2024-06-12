#' Calculates a list with interval information based on lower bounds
#'
#' This gives out a list containing the lower bounds, weights (i.e., mid points of the intervals), and widths of the intervals. Note that the output is one shorter than the input since the last lower bound has no corresponding upper bound.
#' @param lBounds A vector containing all lower bounds and the upper bound of the last interval
#' @export
#' @examples
#' giveIntervals(c(1, 2, 5, 7))
#'

giveIntervals <- function(lBounds){
    return(list(lowBounds = lBounds[-length(lBounds)],
                weights = c((lBounds[-1] + lBounds[-length(lBounds)])/2),
                widths = c(lBounds[-1] - lBounds[-length(lBounds)])))
}
