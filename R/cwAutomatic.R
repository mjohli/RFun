#' Crosswalk function
#'
#' A function that crosswalks and names the outcoming variable
#' @param df A data frame with one column with the same name as one column in the crosswalk
#' @param cw A crosswalk as data.frame with two variables, output of giveCw should work
#' @param varMerge String giving the name of the variable that should be merged
#' @param varName String giving the desired name for the output variable
#' @export
#'

cwAutomatic <- function(df, cw, varMerge, varName){
    if(length(cw) != 2){
        stop("Crosswalk not suitable, use giveCw on geocorr crosswalk first.")
    }
    colnames(cw)[colnames(cw) != varMerge] <- varName
    df <- merge(df, cw, by = varMerge, all.x = TRUE)
}
