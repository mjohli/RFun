#' Reads a crosswalk file
#'
#' Convenience function for crosswalks from geocorr
#' @param path Path of the crosswalk
#' @export
#'

read.crosswalk <- function(path){
    return(read.csv(file = path)[-1,])
}
