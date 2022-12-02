covTex <- function(x, y){
    text <- paste0("$s_{xy} = \\frac1n\\sum_{k=1}^n(x_k-\\bar x)(y_k-\\bar y) = \\frac1{", length(x), "}(")
    for(i in length(x)){
        paste0("(", x[i], "-", mean(x), ")(", y[i], "-", mean(y), ")+")
    }
    text <- substr(text, 1, nchar(text) -1)
    text <- paste0(text, ")=", round(mean((x-mean(x))*(y-mean(y))), 2), "$")
}
