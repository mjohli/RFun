#' Floor or ceiling with number decimals option
#'
#' Floor or ceiling of a number to the number of decimals given and the option to leave out the zero before the decimal point.
#' @param x Vector of numbers to round
#' @param n Number of numbers after decimal point
#' @param FUN Function to use on numbers, for ceiling use `ceiling`
#' @param cutLZero Whether zero before the decimal point should be left out.
#' @export
#' @examples
#' floorCeil(0.4592)
#'

floorCeil <- function(x, n = 2, FUN = floor, cutLZero = TRUE){
    n <- 10^n
    n <- FUN(x = n*x)/n
    if(cutLZero){
        n <- ifelse(test = abs(x = n) == 1,
                    yes = n,
                    no = ifelse(n > 0,
                                substring(text = n, first = 2),
                                ifelse(n < 0, paste0("-", substring(n, 3)), 0)))
    }
    return(n)
}
