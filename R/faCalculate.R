#' Automatically calculates parallel and factor analyses
#'
#' Dataframe needs to consist only of the items as columns and participants as rows
#' @param faDat Data frame to run analysis on
#' @param cutoff Cutoff value for loadings to appear on the tidy table.
#' @param nFactors Number of factors to extract, can also be a vector. If `NULL`, takes the recommended number of the parallel analysis
#' @param nRound Number of decimals to round to
#' @param method Method for factor analysis, can be a vector
#' @param rotate Rotation method if needed
#' @export
#' @examples
#' floorCeil(0.4592)
#'

faCalculate <- function(faDat, cutoff, nFactors = NULL, nRound = 3, method = c("pa", "minres"), rotate = "oblimin"){
    hPA <- fa.parallel(x = faDat)
    faRes <- list()
    for(meth in method){
        if(is.null(nFactors)){
            if(meth == "pa"){
                nFactors <- hPa$ncomp
            } else if(meth == "minres"){
                nFactors <- hPa$nfact
            } else{
                stop("Method not yet fully implemented, read some theory;)")
            }
        }
        for(i in nFactors){
            faRes[[meth]][[as.character(i)]][["result"]] <-
                fa(r = faDat, nfactors = i, rotate = rotate, fm = meth)
            faRes[[meth]][[as.character(i)]][["tables"]] <-
                mFaTable(fa = faRes[[meth]][[as.character(i)]][["result"]],
                         cutoff = cutoff, n = n, nRound = nRound)
        }
    }
    return(list(pa = hPA, fa = faRes))
}
