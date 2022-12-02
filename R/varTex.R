sdTex <- function(x, letter = "x"){
    text <- paste0("$s_", letter, "^2=\\frac1n\\sum_{k=1}^n(", letter, "_k-\\bar ", letter, ")^2 = \\frac1{", length(x), "}(")
    for(i in x){
        text <- paste0(text, "(", i, "-", mean(x), ")+")
    }
    text <- substr(text, 1, nchar(text) -1)
    text <- paste0(text, ")=", round(mean((x-mean(x))^2), 2), "$")
}
