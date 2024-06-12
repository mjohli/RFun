#' Interpolates median for ranges based on frequencies
#'
#' This interpolates the median if given frequencies, lower bounds of ranges, and widths of ranges
#' @param x A vector containing the frequencies
#' @param lBounds A vector containing the lower bounds of the ranges
#' @param widths A vector containing the widths of the ranges
#' @export
#'

interpMedian <- function(x, lBounds, widths){
    mInt <- median(rep(1:length(x), x))
    lBounds[mInt] + ((sum(x) / 2 - sum(x[1:(mInt - 1)]))/x[mInt]) * widths[mInt]
}
