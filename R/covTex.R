#' A covariance LaTex generator
#'
#' This generates the covariance formula in LaTex code for given values. Note that both vectors have to be the same length
#' @param x The first vector
#' @param y The second vector
#' @export
#' @examples
#' covTex(c(1,2,3), c(2,3,4))
#'
covTex <- function(x, y){
    text <- paste0("$s_{xy} = \\frac1n\\sum_{k=1}^n(x_k-\\bar x)(y_k-\\bar y) = \\frac1{", length(x), "}(")
    for(i in length(x)){
        text <- paste0("(", x[i], "-", mean(x), ")(", y[i], "-", mean(y), ")+")
    }
    text <- substr(text, 1, nchar(text) -1)
    text <- paste0(text, ")=", round(mean((x-mean(x))*(y-mean(y))), 2), "$")
}
