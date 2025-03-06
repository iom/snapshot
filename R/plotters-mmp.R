
# Dead or missing migrants ------------------------------------------------

plot_mmp <- function(hero,
                     basesize,
                     font,
                     title = paste0(
                       "Dead or missing international migrants, ",
                       plot_data("mmp", hero)$range |>
                         paste(collapse = "\u2013")
                     )) {
  
  k <- function(factor = 1) factor * basesize / .pt
  source <- "Source: IOM Missing Migrants Project."
  t0 <- plot_data("mmp", hero)$range[1]
  t1 <- plot_data("mmp", hero)$range[2]
  
  causes <- c(
    "Drowning"          = pal("blues", 2),
    "Transport hazards" = pal("blues", 4),
    "Harsh conditions"  = pal("greens"),
    "Accidental"        = pal("greens", 3),
    "Sickness"          = pal("yellows"),
    "Violence"          = pal("reds", 2),
    "Mixed or unknown"  = pal("grays", 3)
  )
  
  data <- plot_data("mmp", hero)$data
  
  plot_elements <- list(
    geom_bar(
      aes(
        x = factor(.data$t),
        y = .data$n,
        fill = factor(.data$cause, levels = names(causes))
      ),
      stat = "identity",
      position = "stack",
      width = .7
    ),
    labs(title = title, caption = source),
    scale_fill_manual(name = "Cause", values = causes),
    apply_theme("bar-vertical", basesize = basesize, font = font),
    theme(
      legend.key.size = unit(basesize, "points"),
      legend.text = element_text(
        size = basesize - 1,
        margin = margin(r = 0, l = k(.75))
      ),
      legend.box.margin = margin(t = k(-3), b = 0),
      plot.margin = margin(k(2), k(2), k(2), k(2))
    )
  )
  
  if (nrow(data) > 0) {
    
    df_agg <- summarise(data, n = sum(.data$n), .by = .data$t) |>
      filter(.data$n > 0)
    df <- complete(data, t = t0:t1, cause = names(causes))
    
    plot <- ggplot(df) +
      plot_elements +
      geom_text(
        aes(x = factor(.data$t), y = .data$n, label = prettylabel(.data$n)),
        df_agg,
        color = pal("blues"),
        family = font,
        fontface = "bold",
        size = k(.8),
        vjust = 0,
        nudge_y = max(df$n, na.rm = TRUE) / 40
      ) +
      scale_y_continuous(name = "Persons") +
      theme(
        axis.title.y = element_text(
          size = basesize,
          margin = margin(r = k(2))
        ),
        axis.text.x = element_text(margin = margin(t = -k(.5))),
      )
    
  } else {
    
    df <- expand.grid(t = t0:t1, cause = names(causes), n = 0)
    
    plot <- ggplot(df) +
      plot_elements +
      theme(
        axis.text.y = element_blank(),
        panel.background = element_rect(color = NA, fill = pal("unblues", 6)),
        panel.grid.major.y = element_blank(),
      )
    
    plot <- ggdraw(plot) +
      draw_label(
        "None recorded",
        y = .6,
        fontfamily = font,
        color = pal("blues", 3),
        size = k(3)
      )
  }
  
  return(plot)
}
