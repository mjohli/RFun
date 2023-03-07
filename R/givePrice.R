#' A function that computes the better buying option for cookie clicker buildings
#'
#' This gives the better option of two buildings based on current price and cookie production
#' @param p1 Price of the first building, doesn't need to be the actual price, can also be a share of the price as long as it's consistent to the second building.
#' @param cps1 Cookie production of the first building, doesn't need to be the actual production, can also be a share of the production as long as it's consistent to the second building.
#' @param p2 Price of the second building, doesn't need to be the actual price, can also be a share of the price as long as it's consistent to the first building.
#' @param cps2 Cookie production of the second building, doesn't need to be the actual production, can also be a share of the production as long as it's consistent to the first building.
#' @export
#' @examples
#' givePrice(100, 100, 30, 100)
#'
givePrice <- function(p1, cps1, p2, cps2){
    cpsRel <- cps1 / cps2
    pRel <- p1 / p2
    if(cpsRel > pRel){
        nBuil <- 1
        p <- round(p2 * cpsRel)
    } else{
        nBuil <- 2
        p <- round(p1 / cpsRel)
    }
    cat(paste("Building ", nBuil, "\nnew cost: ", p))
}
