
#' ggplot2-Theme TSD
#'
#' @param base_size Normal-Schriftgrösse
#'
#' @export
#'
theme_tsd <- function(base_size = 11) {

    bg_hex      <- "#F7F7F7"
    caption_hex <- "#555555"
    grid_hex    <- "#cccccc"

    font_family <- "Roboto"

    font_size_axis_text  <- 0.9
    font_size_axis_title <- 0.8
    lineheight_default   <- 1.1

  bg_color <- bg_hex
  bg_rect  <- element_rect(fill = bg_color, color = bg_color)

  theme_bw(base_size) +
    theme(
      text = element_text(family = font_family),
      plot.title    = element_text(size = rel(0.85),
                                   lineheight = lineheight_default),
      plot.subtitle = element_text(size = rel(0.7),
                                   lineheight = lineheight_default),
      plot.caption  = element_text(size = rel(0.6),
                                   margin = unit(c(1, 0, 0, 0), "lines"),
                                   lineheight = lineheight_default,
                                   color = caption_hex),
      legend.title  = element_text(size = rel(0.85),
                                   lineheight = lineheight_default,
                                   color = caption_hex),
      plot.background = bg_rect,
      axis.ticks   = element_blank(),
      axis.text.x  = element_text(size = rel(font_size_axis_text)),
      axis.title.x = element_text(size = rel(font_size_axis_title),
                                  margin = margin(1, 0, 0, 0, unit = "lines")),
      axis.text.y  = element_text(size = rel(font_size_axis_text)),
      axis.title.y = element_text(size = rel(font_size_axis_title)),
      panel.background  = bg_rect,
      panel.border      = element_blank(),
      panel.grid.major  = element_line(color = grid_hex, size = 0.25),
      panel.grid.minor  = element_line(color = grid_hex, size = 0.25),
      panel.spacing     = unit(1.25, "lines") ,
      legend.background = bg_rect,
      legend.key.width  = unit(1.5, "line") ,
      legend.text       = element_text(size = rel(0.7)),
      legend.key        = element_blank(),
      strip.background  = element_blank()
    )
}

