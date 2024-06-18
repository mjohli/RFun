#' My Theme
#'
#' A very clean theme for ggplot2 with no axes and no background with bigger
#' text for slides
moSlides <- ggplot2::theme(text = ggplot2::element_text(size = 20),
                          panel.grid = ggplot2::element_blank(),
                          panel.border = ggplot2::element_blank(),
                          panel.background = ggplot2::element_rect(fill = "white",
                                                                   colour = "white",
                                                                   size = 0.5,
                                                                   linetype = "solid"))
