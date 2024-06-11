#' A mean LaTex generator
#'
#' This generates the mean formula in LaTex code for given values.
#' @param x A vector
#' @param letter The letter to use inside the formula, defaults to x
#' @export
#' @examples
#' mTex(c(1,2,3))
#' mTex(c(2,3,4), letter = "y")
#'

mTex <- function(x, letter = "x"){
    text <- paste0("$\\bar ", letter, "=\\frac1n\\sum_{k=1}^n", letter, "_k = \\frac1{", length(x), "}(")
    for(i in x){
        text <- paste0(text, i, "+")
    }
    text <- substr(text, start = 1, stop = nchar(text) -1)
    return(paste0(text, ")=", mean(x), "$"))
}
