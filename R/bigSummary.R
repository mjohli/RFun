#' Generates an extensive summary of a numerical variable
#'
#' This prints the name of the variable, a summary and a density plot
#' @param x The column of the data frame to investigate
#' @export
#'

bigSummary <- function(x){
    nam <- deparse(expr = substitute(expr = x))
    print(nam)
    print(summary(object = x))
    ggplot2::ggplot() +
        ggplot2::aes(x) +
        ggplot2::geom_density() +
        ggplot2::xlab(label = nam) +
        moTheme
}
