#' A function that calculates share female, median and mean age for IPUMS data
#'
#' Calculates share female, mean and interpolated median age for IPUMS data sets based on frequency tables.
#' @param df The data frame to calculate for
#' @param code The three letter code for age
#' @param year Year or another data frame identifier to be added to the column names
#' @export
#'

ipumsGenderAge <- function(df, code, year = NULL){
    cNams <- c("meanAge", "medianAge", "shareF")
    if(!is.null(year)){
        cNams <- paste0(cNams, "_", year)
    }
    intervals <- giveIntervals(c(0, 5, 10, 15, 18, 20, 21, 22, 25 + 5*(0:7), 62, 65, 67, 70 + 5*(0:4)))
    shareF <- df[,ipumsVars(code, 2)] / df[,ipumsVars(code, 1)]
    df <- df[,ipumsVars(code, 3:25)] + df[,ipumsVars(code, 27:49)]
    means <- apply(df, 1, function(x) meanFromFrequencies(x, intervals[["weights"]]))
    medians <- apply(df, 1, function(x) interpMedian(x, intervals[["lowBounds"]], intervals[["widths"]]))
    df <- data.frame(means, medians, shareF)
    colnames(df) <- cNams
    return(df)
}
