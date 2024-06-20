#' Generates a tidy looking table from factor analysis results
#'
#' @param fa Results from factor analysis
#' @param cutoff Value below which results should be left out of the reduced table
#' @param nRound number of digits for rounding
#' @export
#'

mFaTable <- function(fa, cutoff, nRound = 3){
    faFTab <- data.frame(matrix(round(fa$loadings, digits = nRound), ncol = ncol(fa$loadings)))
    faFTab <- faFTab[,as.numeric(gsub("[A-z]*", "", colnames(faFTab)))]
    farFTab <- as.data.frame(apply(faFTab, 2, function(x) ifelse(abs(x) < cutoff, "", x)))
    farFTab$Items <- row.names(farFTab)
    faFTab$Items <- row.names(x = faFTab)
    row.names(faFTab) <- NULL
    row.names(farFTab) <- NULL
    return(list("compTab" = faFTab, "redTab" = farFTab))
}
