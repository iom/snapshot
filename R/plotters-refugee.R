
# Refugees ----------------------------------------------------------------

plot_refug <- function(hero,
                       basesize,
                       font,
                       title = paste0(
                         "Refugee populations, ",
                         plot_data("refug", hero)$range |>
                           paste(collapse = "\u2013")
                       )) {
  
  k <- function(factor = 1) factor * basesize / .pt
  source <- "Source: UNHCR."
  name <- namer(hero)
  
  orig_disp <- paste0("Refugees from ", name, "\nand where they are hosted")
  host_disp <- paste0("Refugees hosted in ", name, "\nand where they come from")
  
  data <- plot_data("refug", hero)$data |>
    mutate(panel = ifelse(.data$panel == "orig", orig_disp, host_disp))
  nats <- levels(data$nat_name)[
    !(levels(data$nat_name) %in% c("Unknown", "Others"))
  ]
  
  colors <- c(
    pal("blues", 2),
    pal("oranges", 2),
    pal("reds", 2),
    pal("unblues", 2),
    pal("greens"),
    pal("yellows", 2)
  )
  
  nat_fill <- colors[1:length(nats)]
  names(nat_fill) <- nats
  nat_fill <- c(
    nat_fill, 
    "Unknown" = pal("grays", 3),
    "Others" = pal("grays", 4)
  )
  
  plot_elements <- list(
    facet_wrap(~forcats::fct_relevel(.data$panel, host_disp, after = Inf)),
    labs(title = title, caption = source),
    scale_x_discrete(expand = expansion(mult = .05)),
    scale_fill_manual(values = nat_fill),
    guides(fill = guide_legend(nrow = 2)),
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
    
    data_tot <- data |>
      summarise(n = sum(.data$n), .by = c(.data$t, .data$panel)) |>
      mutate(lab = prettylabel(.data$n))
    axis <- set_axis(data_tot$n, "Persons")
    
    plot <- ggplot(data, aes(x = factor(.data$t), y = .data$n)) +
      geom_bar(
        aes(fill = .data$nat_name),
        stat = "identity", position = "stack", width = .8
      ) +
      plot_elements +
      geom_text(
        aes(x = factor(.data$t), y = .data$n, label = prettylabel(.data$n)),
        data_tot,
        color = pal("blues"),
        family = font,
        fontface = "bold",
        size = k(.8),
        vjust = 0,
        nudge_y = max(data_tot$n, na.rm = TRUE) / 40
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
      t = c(
        seq(min(gdidata::unhcr$t), max(gdidata::unhcr$t), 5),
        max(gdidata::unhcr$t)
      ),
      panel = c(orig_disp, host_disp),
      n = 0
    )
    
    plot <- ggplot(df, aes(x = factor(.data$t), y = .data$n)) +
      geom_bar(stat = "identity") +
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

