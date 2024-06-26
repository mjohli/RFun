#' My Theme
#'
#' A very clean theme for ggplot2 with no axes and no background with bigger
#' text for slides
moSlides <- ggplot2::theme(panel.background =
                               ggplot2::element_rect(fill = "white",
                                                     colour = "white",
                                                     linewidth = .5,
                                                     linetype = "solid"),
                           panel.border = ggplot2::element_blank(),
                           panel.grid = ggplot2::element_blank(),
                           text = ggplot2::element_text(size = 20))
