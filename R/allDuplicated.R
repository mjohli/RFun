#' Gets all duplicated values
#'
#' Returns every appearance of a duplicated value
#' @param x An object on which the `duplicated` function could be applied
#' @export
#' @examples
#' allDuplicated(c(1, 2, 3, 1, 1))
#'

allDuplicated <- function(x){
    return(duplicated(x) | duplicated(x, fromLast = TRUE))
}
