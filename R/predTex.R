predTex <- function(x, y, px){
    mx <- round(mean(x), 2)
    my <- round(mean(y), 2)
    sxy <- round(mean((x-mean(x))*(y-mean(y))), 2)
    sx <- round(mean((x-mean(x))^2), 2)
    pred <- round((sxy/sx)*(px-mx)+my, 2)
    text <- paste0("$y = \\frac{s_{xy}}{s_x^2}(x-\\bar x)+\\bar y = \\frac{", sxy, "}{", sx, "}(", px, "-", mx, ")+", my, "=", round((sxy/sx)*(px-mx)+my, 2), "$")
    return(list(text = text, pred = pred))
}
