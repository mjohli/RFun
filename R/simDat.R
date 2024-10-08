#' Function to simulate simple data
#'
#' Generates a dataframe with one row for each id, normally distributed
#' variables for 'vars', and uniform dichotomous variables for 'dichVars'
#' @param ids Vector of ids to identify each row
#' @param vars Vector of variable names for continuous normally distributed
#' variables
#' @param dichVars Vector of variable names for dichotomous uniformly
#' distributed variables
#' @export
#'
simDat <- function(ids, vars, dichVars = NULL){
    dat <- data.frame(id = as.factor(x = ids))
    n <- length(x = ids)
    dat[vars] <- matrix(data = rnorm(n = n * length(vars)), ncol = length(vars))
    dat[dichVars] <- matrix(sample(x = c(TRUE, FALSE),
                                   size = n * length(dichVars), replace = TRUE),
                            ncol = length(dichVars))
    return(as.data.frame(x = dat))
}
