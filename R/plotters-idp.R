

# IDPs --------------------------------------------------------------------

plot_idp <- function(hero,
                     basesize,
                     font,
                     title = paste0(
                       "New internal displacements, ",
                       plot_data("idp", hero)$range |>
                         paste(collapse = "\u2013")
                     )) {
  
  k <- function(factor = 1) factor * basesize / .pt
  source <- "Source: IDMC."
  if (hero %in% idmc_iom) {
    source <- "Source: IDMC; IOM Displacement Tracking Matrix."
  }
  t0 <- plot_data("idp", hero)$range[1]
  t1 <- plot_data("idp", hero)$range[2]
  
  causes <- c(
    "Environmental impacts" = pal("blues", 2),
    "Conflict and violence" = pal("reds", 2)
  )
  
  data <- plot_data("idp", hero)$data
  
  plot_elements <- list(
    geom_bar(
      aes(
        x = factor(.data$t), 
        y = .data$n, 
        fill = forcats::fct_rev(.data$cause)
      ),
      stat = "identity",
      position = "stack",
      width = .7
    ),
    labs(title = title, caption = source),
    scale_fill_manual(values = causes),
    apply_theme("bar-vertical", basesize = basesize, font = font),
    theme(plot.margin = margin(k(2), k(2), k(2), k(2)))
  )
  
  if (nrow(data) > 0) {
    
    df_agg <- summarise(data, n = sum(.data$n), .by = .data$t) |>
      filter(.data$n > 0)
    axis <- set_axis(df_agg$n, "Displacements")
    
    df <- complete(data, t = t0:t1, cause = names(causes))
    
    plot <- ggplot(df) +
      plot_elements +
      geom_text(
        aes(x = factor(.data$t), y = .data$n, label = prettylabel(.data$n)),
        df_agg,
        color = pal("blues"),
        family = font,
        fontface = "bold",
        size = k(.9),
        vjust = 0,
        nudge_y = max(df$n, na.rm = TRUE) / 40
      ) +
      scale_y_continuous(
        name = axis$title,
        breaks = axis$breaks,
        labels = axis$labels,
        expand = expansion(mult = c(.02, .05))
      ) +
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
        "No data",
        y = .55,
        fontfamily = font,
        color = pal("blues", 3),
        size = k(3)
      )
  }
  
  return(plot)
}


# IDPs by cause -----------------------------------------------------------

plot_idcause <- function(hero,
                          basesize,
                          font,
                          title = paste0(
                            "Causes of new internal displacements\ndue to environmental impacts, ",
                            plot_data("idcause")$range |>
                              paste(collapse = "\u2013")
                          )) {
  
  k <- function(factor = 1) factor * basesize / .pt
  source <- "Source: IDMC."
  if (hero %in% idmc_iom) {
    source <- "Source: IDMC; IOM Displacement Tracking Matrix."
  }
  
  data <- plot_data("idcause", hero)$data
  
  # data <- filter(gdidata::idmc_flows, t >= max(t) - 9 & cause != "conflict")
  # data_iso <- filter(data, geo == hero)
  
  # t0 <- min(data$t)
  # t1 <- max(data$t)
  
  if (nrow(data) > 0) {
    # 
    # df <- data_iso |> 
    #   summarise(n = sum(n), .by = c(geo, cause)) |> 
    #   arrange(desc(n)) |> 
    #   mutate(
    #     rank = 1:n(),
    #     cause = case_when(
    #       rank >= 4 ~ "Others",
    #       .default = paste0(
    #         toupper(substr(cause, 1, 1)), 
    #         substr(cause, 2, nchar(cause))
    #       ))
    #   ) |> 
    #   summarise(n = sum(n), .by = c(geo, cause)) |> 
    #   mutate(
    #     v = n / sum(n),
    #     ymax = cumsum(v),
    #     ymin = c(0, ymax[-n()]),
    #     pos = (ymax + ymin) / 2,
    #     label = case_when(
    #       v < 1 & v >= .995 ~ ">99%", 
    #       v >= .02 ~ prettylabel(100 * v, pct = TRUE), 
    #       .default = NA
    #     )
    #   )
    
    df <- data |> 
      mutate(
        ymax = cumsum(v),
        ymin = c(0, ymax[-n()]),
        pos = (ymax + ymin) / 2,
        label = case_when(
          v < 1 & v >= .995 ~ ">99%", 
          v >= .02 ~ prettylabel(100 * v, pct = TRUE), 
          .default = NA
        )
      )
    
    plot <- ggplot(
      df, 
      aes(
        ymax = ymax, ymin = ymin, xmax = 4, xmin = 3,
        fill = forcats::fct_reorder(cause, -v)
      )
    ) +
      geom_rect() + 
      geom_text(
        aes(y = pos, label = label),
        size = k(), color = "white", family = font, 
        fontface = "bold", x = 3.5, hjust = .5, vjust = .5
      ) +
      
      # Scales
      scale_x_continuous(limits = c(2, 4)) + 
      scale_y_continuous(expand = waiver()) +
      scale_fill_manual(values = c(
        pal("blues", 2),
        pal("greens"),
        pal("reds", 2),
        pal("grays", 4)
      )) +
      coord_polar(theta = "y") +
      
      # Aesthetics
      apply_theme("bar-vertical", basesize = basesize, font = font) +
      theme(
        axis.text = element_blank(),
        legend.text = element_text(
          size = basesize,
          margin = margin(r = k(), l = k())
        ),
        panel.grid.major.y = element_blank()
      )
    
    plot_title <- ggplot() + ggtitle(title) +
      apply_theme("map", basesize = basesize, font = font) + 
      theme(plot.margin = margin(0, 0, 0, 0))
    
    plot_caption <- ggplot() + labs(caption = source) +
      apply_theme("map", basesize = basesize, font = font) + 
      theme(plot.margin = margin(0, 0, 0, 0))
    
    plot <- plot_grid(
      plot_title, plot, plot_caption, 
      nrow = 3, rel_heights = c(.15, 1, .1)
    ) +
      theme(plot.margin = margin(k(), k(2), k(), k(2)))
    
  } else {
    
    plot <- ggplot(
      data.frame(ymax = 1, ymin = 0), 
      aes(xmax = 4, xmin = 3, ymax = ymax, ymin = ymin)
    ) +
      geom_rect(fill = pal("grays", 5)) +
      scale_x_continuous(limits = c(2, 4)) + 
      scale_y_continuous(expand = waiver()) +
      coord_polar(theta = "y") +
      apply_theme("bar-vertical", basesize = basesize, font = font) +
      theme(
        axis.text = element_blank(),
        panel.grid.major.y = element_blank()
      )
    
    plot_title <- ggplot() + ggtitle(title) +
      apply_theme("map", basesize = basesize, font = font) + 
      theme(plot.margin = margin(0, 0, 0, 0))
    
    plot_caption <- ggplot() + labs(caption = source) +
      apply_theme("map", basesize = basesize, font = font) + 
      theme(plot.margin = margin(0, 0, 0, 0))
    
    plot <- plot_grid(
      plot_title, plot, plot_caption, 
      nrow = 3, rel_heights = c(.15, 1, .1)
    ) +
      theme(plot.margin = margin(k(), k(2), k(), k(2)))
    
    plot <- ggdraw(plot) + 
      draw_label(
        "No data", 
        y = .5,
        fontfamily = font,
        color = pal("blues", 3), 
        size = k(3)
      )
  }
  
  return(plot)
}

