

# Migrant stocks ----------------------------------------------------------

plot_stocks <- function(hero,
                        basesize,
                        font,
                        title = paste0(
                          "Migrant populations, ",
                          plot_data("stocks", hero, use_2020 = use_2020)$range |>
                            paste(collapse = "\u2013")
                        ),
                        use_2020 = TRUE) {
  
  k <- function(factor = 1) factor * basesize / .pt
  source <- "Source: UN DESA."
  if (use_2020) source <- "Source: UN DESA (2020 release)."
  name <- namer(hero)
  
  emig_disp <- paste0("Emigrants from ", name)
  immig_disp <- paste0("Immigrants in ", name)
  
  data <- plot_data("stocks", hero, use_2020 = use_2020)$data
  timespan <- unique(data$t)
  
  data <- data |>
    mutate(panel = ifelse(.data$panel == "emig", emig_disp, immig_disp))
  
  regions <- c(
    "Within region"  = pal("blues", 2),
    "Outside region" = pal("greens"),
    "Others"         = pal("grays", 4),
    "Unknown"        = pal("grays", 3)
  )
  
  plot_elements <- list(
    facet_wrap(~.data$panel),
    geom_bar(
      aes(fill = .data$region),
      stat = "identity", position = "stack", width = .8
    ),
    labs(title = title, caption = source),
    scale_x_discrete(expand = expansion(mult = .075)),
    scale_fill_manual(values = regions),
    apply_theme("bar-vertical", basesize = basesize, font = font, facets = TRUE),
    theme(
      legend.position = "bottom",
      panel.spacing.x = unit(k(.6), "lines"),
      plot.margin = margin(k(2), 0, k(2), k(2)),
      strip.text = element_text(
        size = basesize,
        margin = margin(t = 0, b = k())
      )
    )
  )
  
  if (nrow(data) > 0) {
    
    df_tot <- data |>
      summarise(
        n = sum(.data$n, na.rm = TRUE), 
        .by = c(.data$t, .data$panel)
      ) |>
      mutate(lab = prettylabel(.data$n, signif = 1))
    
    axis <- set_axis(df_tot$n, "Persons")
    
    df <- data |>
      complete(
        region = names(regions),
        t = timespan,
        .data$panel,
        fill = list(n = 0)
      ) |>
      mutate(region = factor(region, levels = names(regions)))
    
    plot <- ggplot(
      df,
      aes(x = factor(.data$t), y = .data$n)
    ) +
      plot_elements +
      geom_text(
        aes(x = factor(.data$t), y = .data$n, label = prettylabel(.data$n)),
        df_tot,
        color = pal("blues"),
        family = font,
        fontface = "bold",
        size = k(.8),
        vjust = 0,
        nudge_y = max(df$n, na.rm = TRUE) / 40
      ) +
      scale_y_continuous(
        name = axis$title,
        breaks = axis$breaks,
        labels = axis$labels,
        expand = expansion(mult = c(0, .05))
      ) +
      theme(axis.title.y = element_text(
        size = basesize,
        margin = margin(r = k(2))
      ))
    
  } else {
    
    df <- expand.grid(
      region = names(regions),
      t = timespan,
      panel = c(emig_disp, immig_disp),
      n = 0
    )
    
    plot <- ggplot(
      df,
      aes(x = factor(.data$t), y = .data$n)
    ) +
      plot_elements +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        panel.background = element_rect(color = NA, fill = pal("unblues", 6)),
        panel.grid.major.y = element_blank(),
      )
    
    plot <- ggdraw(plot) +
      draw_label(
        "No data",
        x = .2240,
        y = .5,
        fontfamily = font,
        color = pal("blues", 3),
        size = k(3)
      ) +
      draw_label(
        "No data",
        x = .6575,
        y = .5,
        fontfamily = font,
        color = pal("blues", 3),
        size = k(3)
      )
  }
  
  return(plot)
}


# Nationalities -----------------------------------------------------------

plot_nats <- function(hero,
                      basesize,
                      font,
                      title = paste0(
                        "Destinations and origins of migrants, ",
                        plot_data("nats", hero, use_2020 = use_2020)$range
                      ),
                      use_2020 = TRUE) {
  
  k <- function(factor = 1) factor * basesize / .pt
  name <- namer(hero)
  source <- "Source: UN DESA."
  if (use_2020) source <- "Source: UN DESA (2020 release)."
  
  data <- plot_data("nats", hero, use_2020 = use_2020)$data
  
  if (nrow(data) > 0) {
    
    regions <- c(
      "Within region"  = pal("blues", 2),
      "Outside region" = pal("greens"),
      "Others"         = pal("grays", 4),
      "Unknown"        = pal("grays", 3)
    )
    
    df <- mutate(data, country = break_lines(.data$country))
    
    df_destin <- filter(df, str_detect(.data$panel, "Destinations"))
    df_origin <- filter(df, str_detect(.data$panel, "Origins"))
    df_destin$country <- factor(df_destin$country, levels = df_destin$country)
    df_origin$country <- factor(df_origin$country, levels = df_origin$country)
    
    plot_elements <- list(
      geom_bar(stat = "identity", width = .7, show.legend = FALSE),
      geom_text(
        aes(label = prettylabel(.data$share, pct = TRUE), color = .data$region),
        size = k(),
        family = font,
        fontface = "bold",
        hjust = 0, vjust = .5,
        nudge_x = max(df$share) / 30,
        show.legend = FALSE
      ),
      scale_x_continuous(
        labels = function(x) prettylabel(x, pct = TRUE),
        expand = expansion(mult = c(.02, .15))
      ),
      scale_y_discrete(expand = expansion(mult = .15)),
      scale_fill_manual(values = regions),
      scale_color_manual(values = c(
        "Within region"  = pal("blues"),
        "Outside region" = pal("greens"),
        "Others"         = pal("grays", 2),
        "Unknown"        = pal("grays", 2)
      )),
      apply_theme("bar-horizontal", basesize = basesize, font = font),
      theme(
        axis.text.y = element_text(size = basesize - 1, lineheight = k(.35)),
        plot.title = element_text(
          size = basesize,
          face = "plain",
          margin = margin(t = 0, b = k(2))
        )
      )
    )
    
    p_destin <- ggplot(
      df_destin,
      aes(x = .data$share, y = .data$country, fill = .data$region)
    ) +
      ggtitle(paste0("Destinations of emigrants from ", name)) +
      plot_elements +
      theme(plot.margin = margin(0, k(2), 0, 0))
    
    p_origin <- ggplot(
      df_origin,
      aes(x = .data$share, y = .data$country, fill = .data$region)
    ) +
      ggtitle(paste0("Origins of immigrants from ", name)) +
      plot_elements +
      theme(plot.margin = margin(0, 0, 0, k(2)))
    
    plot <- plot_grid(p_destin, p_origin, nrow = 1)
    
    plot_title <- ggplot() +
      ggtitle(title) +
      apply_theme("map", basesize = basesize, font = font) +
      theme(plot.margin = margin(0, 0, 0, 0))
    
    plot_caption <- ggplot() +
      labs(caption = source) +
      apply_theme("map", basesize = basesize, font = font) +
      theme(plot.margin = margin(0, 0, 0, 0))
    
    plot <- plot_grid(
      plot_title, plot, plot_caption,
      nrow = 3,
      rel_heights = c(.1, 1, .1)
    ) +
      theme(plot.margin = margin(k(2), k(2), k(2), k(2)))
    
  } else {
    
    panel_empty_bar <- function(title) {
      
      plot_base <- ggplot() +
        ggtitle(title) +
        apply_theme(type = "void", basesize = basesize, font = font) +
        theme(
          panel.background = element_rect(color = NA, fill = pal("unblues", 6)),
          plot.title = element_text(
            size = basesize,
            face = "plain",
            margin = margin(t = 0, b = k(3))
          )
        )
      
      plot <- ggdraw(plot_base) +
        draw_label(
          "No data",
          y = .4,
          fontfamily = font,
          color = pal("blues", 3),
          size = k(3)
        )
      
      return(plot)
    }
    
    plot <- plot_grid(
      panel_empty_bar("Emigrant destinations"),
      panel_empty_bar("Immigrant origins"),
      nrow = 1
    )
    
    plot_title <- ggplot() +
      ggtitle(title) +
      apply_theme("map", basesize = basesize, font = font) +
      theme(plot.margin = margin(0, 0, 0, 0))
    
    plot_caption <- ggplot() +
      labs(caption = source) +
      apply_theme("map", basesize = basesize, font = font) +
      theme(plot.margin = margin(0, 0, 0, 0))
    
    plot <- plot_grid(
      plot_title, plot, plot_caption,
      nrow = 3,
      rel_heights = c(.075, 1, .05)
    ) +
      theme(plot.margin = margin(k(2), k(2), k(2), k(2)))
  }
  
  return(plot)
}
