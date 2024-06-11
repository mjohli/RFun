#' A variance LaTex generator
#'
#' This generates the variance formula in LaTex code for given values.
#' @param x A vector
#' @param letter The letter to use inside the formula, defaults to x
#' @export
#' @examples
#' varTex(c(1,2,3))
#' varTex(c(2,3,4), letter = "y")
#'
varTex <- function(x, letter = "x"){
    text <- paste0("$s_", letter, "^2=\\frac1n\\sum_{k=1}^n(", letter,
                   "_k-\\bar ", letter, ")^2 = \\frac1{", length(x), "}(")
    for(i in x){
        text <- paste0(text, "(", i, "-", mean(x), ")+")
    }
    text <- substr(text, start = 1, stop = nchar(text) - 1)
    text <- paste0(text, ")=", round(mean((x - mean(x))^2), digits = 2), "$")
}
