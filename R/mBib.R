#' Adds citations for packages to existing .bib file
#'
#' @param path String containing the path to the .bib file
#' @param pckgs Vector of Strings giving the used packages
#' @export
#'

mBib <- function(path, pckgs){
    bib <- read.table(file = path, quote = "", fill = TRUE, comment.char = "")
    bib <- apply(X = bib,
                 MARGIN = 1,
                 FUN = function(x){paste(x, collapse = " ")})
    bib <- as.data.frame(x = bib)
    colnames(x = bib) <- "x"
    for(pck in pckgs){
        if(pck == "R"){
            cit <- citation()
        } else{
            cit <- citation(package = pck)
        }
        cit <- toBibtex(object = cit)
        cit <- as.character(x = cit)
        cit <- as.data.frame(cit)
        colnames(cit) <- "x"
        cit[1,] <- gsub(pattern = "[{]",
                        replacement = paste0("{", pck),
                        x = cit[1,])
        bib <- rbind(bib, cit)
    }
    write.table(x = bib,
                file = path,
                quote = FALSE,
                row.names = FALSE,
                col.names = FALSE)
}
