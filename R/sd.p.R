#' Calculate population standard deviation
#'
#' @param x Vector of numbers to calculate population standard deviation for
#' @param na.rm Whether `NA`s should be removed
#' @export
#' @examples
#' sd.p(1:10)
#'

sd.p <- function(x, na.rm = T){
    if(na.rm){
        l <- length(x = x[!is.na(x)])
    } else{
        l <- length(x)
    }
    return(sd(x = x, na.rm = na.rm)*sqrt(x = (l-1)/l))
}
