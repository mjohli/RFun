#' A linear prediction LaTex generator
#'
#' This generates the linear prediction formula in LaTex code for given values. Note that both vectors have to be the same length
#' @param x The first vector
#' @param y The second vector
#' @param px The value of x to predict y by
#' @export
#' @examples
#' predTex(c(1,2,3), c(2,3,4), 1)
#'
predTex <- function(x, y, px){
    mx <- round(mean(x), 2)
    my <- round(mean(y), 2)
    sxy <- round(mean((x-mean(x))*(y-mean(y))), 2)
    sx <- round(mean((x-mean(x))^2), 2)
    pred <- round((sxy/sx)*(px-mx)+my, 2)
    text <- paste0("$y = \\frac{s_{xy}}{s_x^2}(x-\\bar x)+\\bar y = \\frac{", sxy, "}{", sx, "}(", px, "-", mx, ")+", my, "=", round((sxy/sx)*(px-mx)+my, 2), "$")
    return(list(text = text, pred = pred))
}
