
# Refugees ----------------------------------------------------------------

plot_refug <- function(hero,
                       basesize,
                       font,
                       title = NULL,
                       lang = "en") {
  
  k <- function(factor = 1) factor * basesize / .pt
  name <- namer(hero, lang = lang)
  
  if (is.null(title)) {
    plot_title <- paste0(
      "Refugee populations, ",
      snap_data("refug", hero)$range |> paste(collapse = "\u2013")
    )
  }
  source <- "Source: UNHCR."
  
  orig_disp <- paste0("Refugees from ", name, "\nand where they are hosted")
  host_disp <- paste0("Refugees hosted in ", name, "\nand where they come from")
  
  others <- "Others"
  unknown <- "Unknown"
  total <- "Total"
  persons <- "Persons"
  
  if (lang == "pt") {
    
    if (is.null(title)) {
      plot_title <- paste0(
        "Populações de refugiados, ",
        snap_data("refug", hero)$range |> paste(collapse = "\u2013")
      )
    }
    source <- "Fonte: ACNUR."
    
    orig_disp <- paste0("Refugiados da ", name, "\ne onde estão acolhidos")
    host_disp <- paste0("Refugiados da ", name, "\ne de onde veem")
    
    others <- "Outros"
    unknown <- "Desconhecido"
    total <- "Total"
    persons <- "Pessoas"
  }
  
  timespan <- unique(snap_data("refug")$data$t) |> sort()
  
  data <- snap_data("refug", hero, lang = lang)$data
  
  plot_elements <- list(
    facet_wrap(~forcats::fct_relevel(panel, host_disp, after = Inf)),
    labs(title = plot_title, caption = source),
    scale_x_discrete(expand = expansion(mult = .05)),
    guides(fill = guide_legend(nrow = 2)),
    
    apply_theme("bar-vertical", basesize = basesize, font = font, facets = TRUE),
    theme(
      legend.position = "bottom",
      panel.spacing.x = unit(k(.6), "lines"),
      strip.text = element_text(
        size = basesize,
        margin = margin(t = 0, b = k())
      )
    )
  )
  
  if (nrow(data) > 0) {
    
    df <- data |>
      mutate(panel = ifelse(panel == "orig", orig_disp, host_disp)) |> 
      complete(
        t = timespan, 
        panel = c(orig_disp, host_disp),
        fill = list(nat_name = unknown, n = 0)
      )
    
    nats <- levels(df$nat_name)[
      !(levels(df$nat_name) %in% c(unknown, others))
    ]
    nats_n <- length(nats)
    
    colors <- c(
      pal("blues", 2),
      pal("oranges", 2),
      pal("reds", 2),
      pal("unblues", 2),
      pal("greens"),
      pal("yellows", 2)
    )
    
    nat_fill <- colors[1:nats_n]
    names(nat_fill) <- nats
    nat_fill <- c(
      nat_fill, 
      pal("grays", 3),
      pal("grays", 4)
    )
    names(nat_fill)[(nats_n + 1):(nats_n + 2)] <- c(unknown, others)
    
    df_tot <- df |>
      summarise(n = sum(n), .by = c(t, panel)) |>
      mutate(lab = ifelse(n == 0, "", prettylabel(n)))
    
    axis <- set_axis(df_tot$n, persons, lang = "pt")
    
    plot <- ggplot(df, aes(x = factor(t), y = n)) +
      geom_bar(
        aes(fill = nat_name),
        stat = "identity", position = "stack", width = .8
      ) +
      geom_hline(
        yintercept = 0,
        color = pal("blues"),
        linewidth = k(.1)
      ) +
      plot_elements +
      geom_text(
        aes(x = factor(t), y = n, label = lab), df_tot,
        color = pal("blues"),
        family = font,
        fontface = "bold",
        size = k(.8),
        vjust = 0,
        nudge_y = max(df_tot$n, na.rm = TRUE) / 40
      ) +
      scale_y_continuous(
        name = axis$title,
        breaks = axis$breaks,
        labels = axis$labels,
        expand = expansion(mult = c(0, .05))
      ) +
      scale_fill_manual(values = nat_fill) +
      theme(axis.title.y = element_text(
        size = basesize,
        margin = margin(r = k(2))
      ))
    
  } else {
    
    df <- expand.grid(
      t = c(
        seq(min(gdidata::unhcr$t), max(gdidata::unhcr$t), 5),
        max(gdidata::unhcr$t)
      ),
      panel = c(orig_disp, host_disp),
      n = 0
    )
    
    plot <- ggplot(df, aes(x = factor(t), y = n)) +
      geom_bar(stat = "identity") +
      plot_elements +
      theme(
        axis.text = element_blank(),
        axis.ticks.x = element_blank(),
        panel.background = element_rect(color = NA, fill = pal("unblues", 6)),
        panel.grid.major.y = element_blank(),
      )
    
    plot <- ggdraw(plot) +
      draw_label(
        "Nenhum dado",
        x = .250,
        y = .5,
        fontfamily = font,
        color = pal("blues", 3),
        size = k(3)
      ) +
      draw_label(
        "Nenhum dado",
        x = .750,
        y = .5,
        fontfamily = font,
        color = pal("blues", 3),
        size = k(3)
      )
  }
  
  return(plot)
}

