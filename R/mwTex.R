mwTex <- function(x, letter = "x"){
    text <- paste0("$\\bar ", letter, "=\\frac1n\\sum_{k=1}^n", letter, "_k = \\frac1{", length(x), "}(")
    for(i in x){
        text <- paste0(text, i, "+")
    }
    text <- substr(text, 1, nchar(text) -1)
    text <- paste0(text, ")=", mean(x), "$")
}
