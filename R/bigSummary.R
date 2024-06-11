#' Generates an extensive summary of a numerical variable
#'
#' This prints the name of the variable, a summary and a density plot
#' @param dvar The column of the data frame to investigate
#' @export
#'

bigSummary <- function(dvar){
    print(deparse(substitute(dvar)))
    print(summary(dvar))
    ggplot() +
        aes(dvar) +
        geom_density() +
        xlab(deparse(substitute(dvar))) +
        moTheme
}
