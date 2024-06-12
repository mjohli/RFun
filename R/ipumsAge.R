#' A function that calculates median and mean age for IPUMS data
#'
#' Calculates mean and interpolated median age for IPUMS data sets based on frequency tables.
#' @param df The data frame to calculate for
#' @param code The three letter code for age
#' @export
#'

ipumsAge <- function(df, code){
    intervals <- giveIntervals(c(0, 5, 10, 15, 18, 20, 21, 22, 25 + 5*(0:7), 62, 65, 67, 70 + 5*(0:4)))
    df <- df[,ipumsVars(code, 3:25)] + df[,ipumsVars(code, 27:49)]
    means <- apply(test, 1, function(x) meanFromFrequencies(x, intervals[["weights"]]))
    medians <- apply(df, 1, function(x) interpMedian(x, intervals[["lowBounds"]], intervals[["widths"]]))
    return(data.frame(means = means, medians = medians))
}
