#' Apply IOM theme to a plot
#'
#' Adds thematic presets adhering to the IOM visual branding to a ggplot object.
#'
#' The `basesize` parameter is used for the font size of axis labels and titles
#' and plot annotations.
#'
#' @param type What type of plot is being themed? Accepts "line",
#'    "bar-horizontal", "bar-vertical", "map", and "void".
#' @param basesize Font size in points. Default is 10.
#' @param font Font family. Default is Open Sans.
#' @param facets Is this a faceted plot?
#'
#' @export
apply_theme <- function(type,
                        basesize = 8,
                        font = "Open Sans",
                        facets = FALSE) {
  
  # Size parameters
  size <- list(
    text = basesize,
    title = basesize + 2,
    stext = basesize - 1,
    footnote = basesize - 2
  )
  
  # Scaler
  k <- function(factor = 1) factor * size$text / .pt
  
  panel_grid_color <- pal("blues", 5)
  panel_grid_width <- k(.1)
  bar_key_size <- grid::unit(1.25 * size$text, "points")
  line_key_height <- grid::unit(size$text, "points")
  line_key_width <- grid::unit(2 * size$text, "points")
  steps_key_height <- grid::unit(.75 * size$text, "points")
  steps_key_width <- grid::unit(2 * size$text, "points")
  
  base <- list(
    theme(
      text = element_text(family = font, color = pal("blues")),
      
      axis.title = element_blank(),
      axis.text = element_text(size = size$text, color = pal("blues")),
      axis.ticks = element_blank(),
      
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.key.spacing.y = grid::unit(0 * size$text, "points"),
      legend.text = element_text(
        size = size$text,
        margin = margin(r = k(.5), l = k())
      ),
      legend.title = element_blank(),
      legend.position = "bottom",
      legend.box.margin = margin(t = k(-3), b = k(-3)),
      
      panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid.minor = element_blank(),
      
      plot.background = element_blank(),
      plot.title = element_text(
        face = "bold",
        size = size$title,
        hjust = .5,
        margin = margin(b = k(3.5))
      ),
      plot.title.position = "plot",
      plot.caption = element_text(
        face = "italic",
        color = pal("blues", 3),
        size = size$text,
        hjust = 0,
        lineheight = 1.2,
        margin = margin(t = k(3.5), r = k(1), l = k(1))
      ),
      plot.caption.position = "plot",
      plot.margin = margin(k(4), k(4), k(4), k(4))
    ))
  
  if (type == "void") {
    
    theme_void <- list(
      theme(
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(
          fill = NA,
          color = pal("blues", 5),
          linewidth = k(.2)
        ),
        panel.grid.major = element_blank()
      ))
    theme <- c(base, theme_void)
  }
  
  if (type == "bar-horizontal") {
    
    theme_barh <- list(
      theme(
        legend.key.size = bar_key_size,
        panel.grid.major.x = element_line(
          linewidth = panel_grid_width,
          color = panel_grid_color
        ),
        panel.grid.major.y = element_blank(),
      ))
    theme <- c(base, theme_barh)
  }
  
  if (type == "bar-vertical") {
    
    theme_barv <- list(
      theme(
        legend.key.size = bar_key_size,
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(
          linewidth = panel_grid_width,
          color = panel_grid_color
        ),
      ))
    theme <- c(base, theme_barv)
  }
  
  if (type == "line") {
    
    theme_line <- list(
      theme(
        legend.key.height = line_key_height,
        legend.key.width = line_key_width,
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(
          linewidth = panel_grid_width,
          color = panel_grid_color
        ),
      ))
    theme <- c(base, theme_line)
  }
  
  if (type == "scatter") {
    
    theme_scat <- list(
      theme(
        legend.key.size = bar_key_size,
        panel.grid.major = element_line(
          linewidth = panel_grid_width,
          color = panel_grid_color
        ),
      ))
    theme <- c(base, theme_scat)
  }
  
  if (type == "map") {
    
    theme_map <- list(
      theme(
        axis.text = element_blank(),
        legend.key.height = steps_key_height,
        legend.key.width = steps_key_width,
        legend.text = element_text(
          size = size$stext,
          margin = margin(t = k(1.5))
        ),
        panel.grid.major = element_blank()
      ))
    theme <- c(base, theme_map)
  }
  
  if (facets) {
    
    theme_facets <- list(
      theme(
        strip.background = element_rect(fill = NA),
        strip.text = element_text(
          size = size$title,
          color = pal("blues"),
          margin = margin(b = k(3.5))
        )
      ))
    theme <- c(theme, theme_facets)
    
  }
  
  return(theme)
}
