#' My Theme
#'
#' A very clean theme for ggplot2 with no axes and no background
#'
moTheme <- ggplot2::theme(panel.background =
                              ggplot2::element_rect(fill = "white",
                                                    colour = "white",
                                                    linewidth = .5,
                                                    linetype = "solid"),
                          panel.border = ggplot2::element_blank(),
                          panel.grid = ggplot2::element_blank())
