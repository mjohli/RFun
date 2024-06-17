#' Crosswalk prepare function
#'
#' A function that prepares a crosswalk containing no missings and no duplicates based on a given variable
#' @param cw A crosswalk as data.frame
#' @param from Variable from which the crosswalk should happen
#' @param to Variable to which crosswalk should happen
#' @param by Variable for selecting which case in case of duplicates
#' @param ftb Variables can also be given as vector
#' @export
#'

giveCw <- function(cw, from = NULL, to = NULL, by = NULL, ftb = c(from, to, by)){
    ordKick <- function(dat, v1, v2 = by){
        dat <- dat[order(dat[[v1]], -dat[[v2]]),]
        dat <- subset(dat, !duplicated(dat[[v1]]))
    }
    cw <- subset(cw, !(cw[[from]] %in% missings | cw[[to]] %in% missings), ftb)
    cw <- ordKick(cw, from)
    cw <- ordKick(cw, to)
    cw <- subset(cw, select = !colnames(cw) == by)
}
