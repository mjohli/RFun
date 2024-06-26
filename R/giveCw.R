#' Crosswalk prepare function
#'
#' A function that prepares a crosswalk containing no missings and no duplicates based on a given variable
#' @param cw A crosswalk as data.frame
#' @param from String indicating variable from which the crosswalk should happen
#' @param to String indicating variable to which crosswalk should happen
#' @param by String indicating variable for selecting which case in case of duplicates
#' @param sameLevel Boolean indicating whether duplicates for the to-variable should be avoided
#' @param missings Values that should be removed
#' @param ftb Variables can also be given as vector
#' @export
#'

giveCw <- function(cw, from = NULL, to = NULL, by = NULL, sameLevel = TRUE, missings = c(" ", "99999", NA), ftb = c(from, to, by)){
    ordKick <- function(dat, v1, v2 = by){
        dat <- dat[order(dat[[v1]], -dat[[v2]]),]
        dat <- subset(dat, !duplicated(dat[[v1]]))
    }
    cw <- subset(cw, !(cw[[from]] %in% missings | cw[[to]] %in% missings), ftb)
    cw <- ordKick(cw, from)
    if(sameLevel){
        cw <- ordKick(cw, to)
    }
    cw <- subset(cw, select = !colnames(cw) == by)
}
