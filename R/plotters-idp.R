
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
      aes(x = factor(.data$t), y = .data$n, fill = forcats::fct_rev(.data$cause)),
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

plot_idpcause <- function(hero) {
  
  k <- function(factor = 1) factor * basesize / .pt
  p_title <- "Causes of new internal displacements\ndue to environmental impacts"
  p_source <- "Source: IDMC."
  if (hero %in% idmc_iom) {
    p_source <- "Source: IDMC; IOM Displacement Tracking Matrix."
  }
  
  data <- filter(gdidata::idmc_flows, t >= max(t) - 9 & cause != "conflict")
  data_iso <- filter(data, geo == hero)
  
  t0 <- min(data$t)
  t1 <- max(data$t)
  
  if (nrow(data_iso) > 0) {
    
    # check <- data |> 
    #   summarise(n = sum(n), .by = c(geo, cause)) |> 
    #   arrange(desc(n)) |> 
    #   mutate(
    #     rank = 1:n(),
    #     cause = case_when(
    #       rank >= 4 ~ "Others",
    #       .default = paste0(
    #         toupper(substr(cause, 1, 1)), 
    #         substr(cause, 2, nchar(cause))
    #       )),
    #     .by = geo
    #   ) |> 
    #   summarise(n = sum(n), .by = c(geo, cause)) |> 
    #   pivot_wider(names_from = cause, values_from = n)
    
    df <- data_iso |> 
      summarise(n = sum(n), .by = c(geo, cause)) |> 
      arrange(desc(n)) |> 
      mutate(
        rank = 1:n(),
        cause = case_when(
          rank >= 4 ~ "Others",
          .default = paste0(
            toupper(substr(cause, 1, 1)), 
            substr(cause, 2, nchar(cause))
          ))
      ) |> 
      summarise(n = sum(n), .by = c(geo, cause)) |> 
      mutate(
        v = n / sum(n),
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
      scale_fill_manual(values = c(
        pal("blues", 2),
        pal("greens"),
        pal("reds", 2),
        pal("grays", 4)
      )) +
      coord_polar(theta = "y") +
      
      # Annotation
      geom_text(
        aes(y = pos, label = label),
        size = k(), color = "white", family = font, 
        fontface = "bold", x = 3.5, hjust = .5, vjust = .5
      ) +
      
      # Aesthetics
      apply_theme(type = "bar-vertical", basesize = basesize, font = font) +
      scale_x_continuous(limits = c(2, 4)) + 
      scale_y_continuous(expand = waiver()) +
      theme(
        axis.text = element_blank(),
        legend.text = element_text(
          size = basesize,
          margin = margin(r = k(), l = k())
        ),
        panel.grid.major.y = element_blank()
      )
    
    plot_title <- ggplot() + ggtitle(str_glue("{p_title}, {t0}–{t1}")) +
      apply_theme(type = "map", basesize = basesize, font = font) + 
      theme(plot.margin = margin(0, 0, 0, 0))
    
    plot_caption <- ggplot() + labs(caption = p_source) +
      apply_theme(type = "map", basesize = basesize, font = font) + 
      theme(plot.margin = margin(0, 0, 0, 0))
    
    plot <- plot_grid(
      plot_title, plot, plot_caption, 
      nrow = 3, rel_heights = c(.05, 1, .05)
    ) +
      theme(plot.margin = margin(k(5), k(3), k(2), k(3)))
    
  } else {
    
    plot <- ggplot(
      tibble(ymax = 1, ymin = 0), 
      aes(xmax = 4, xmin = 3, ymax = ymax, ymin = ymin)
    ) +
      geom_rect(fill = pal("grays", 5)) +
      coord_polar(theta = "y") +
      apply_theme(type = "bar-vertical", basesize = basesize, font = font) +
      scale_x_continuous(limits = c(2, 4)) + 
      scale_y_continuous(expand = waiver()) +
      theme(
        axis.text = element_blank(),
        panel.grid.major.y = element_blank()
      )
    
    plot_title <- ggplot() + ggtitle(p_title) +
      apply_theme(type = "map", basesize = basesize, font = font) + 
      theme(plot.margin = margin(0, 0, 0, 0))
    
    plot_caption <- ggplot() + labs(caption = p_source) +
      apply_theme(type = "map", basesize = basesize, font = font) + 
      theme(plot.margin = margin(0, 0, 0, 0))
    
    plot <- plot_grid(
      plot_title, plot, plot_caption, 
      nrow = 3, rel_heights = c(.05, 1, .05)
    ) +
      theme(plot.margin = margin(k(5), k(3), k(2), k(3)))
    
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

