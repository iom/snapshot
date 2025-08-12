
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
  
  data <- plot_data("stocks", hero, use_2020 = use_2020)$data |> 
    summarise(n = sum(.data$n), .by = c(.data$t, .data$panel))
  timespan <- unique( plot_data("stocks", use_2020 = use_2020)$data$t)
  
  data <- data |>
    mutate(panel = ifelse(.data$panel == "emig", emig_disp, immig_disp))
  
  plot_elements <- list(
    facet_wrap(~.data$panel),
    geom_bar(
      stat = "identity", position = "stack", width = .8, fill = pal("blues", 2)
    ),
    labs(title = title, caption = source),
    scale_x_discrete(expand = expansion(mult = .075)),
    apply_theme("bar-vertical", basesize = basesize, font = font, facets = TRUE),
    theme(
      panel.spacing.x = unit(k(.6), "lines"),
      strip.text = element_text(
        size = basesize,
        margin = margin(t = 0, b = k())
      )
    )
  )
  
  if (nrow(data) > 0) {
    
    axis <- set_axis(data$n, "Persons")
    
    df <- data |>
      mutate(lab = ifelse(
        .data$n == 0, 
        "", 
        prettylabel(.data$n)
      )) |> 
      complete(
        t = timespan,
        .data$panel,
        fill = list(n = 0)
      )
    
    plot <- ggplot(
      df,
      aes(x = factor(.data$t), y = .data$n)
    ) +
      plot_elements +
      geom_hline(
        yintercept = 0,
        color = pal("blues"),
        linewidth = k(.1)
      ) +
      geom_text(
        aes(label = .data$lab),
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
      t = timespan,
      panel = c(emig_disp, immig_disp),
      n = 0
    )
    
    plot <- ggplot(df, aes(x = factor(.data$t), y = .data$n)) +
      plot_elements +
      theme(
        axis.text = element_blank(),
        axis.ticks.x = element_blank(),
        panel.background = element_rect(color = NA, fill = pal("unblues", 6)),
        panel.grid.major.y = element_blank(),
      )
    
    plot <- ggdraw(plot) +
      draw_label(
        "No data",
        x = .25,
        y = .5,
        fontfamily = font,
        color = pal("blues", 3),
        size = k(3)
      ) +
      draw_label(
        "No data",
        x = .75,
        y = .5,
        fontfamily = font,
        color = pal("blues", 3),
        size = k(3)
      )
  }
  
  return(plot)
}

plot_stocks_pt <- function(hero,
                           basesize,
                           font,
                           title = paste0(
                             "Populações migrantes, ",
                             plot_data("stocks", hero, use_2020 = use_2020)$range |>
                               paste(collapse = "\u2013")
                           ),
                           use_2020 = TRUE) {
  
  k <- function(factor = 1) factor * basesize / .pt
  source <- "Fonte: DAESNU."
  if (use_2020) source <- "Fonte: DAESNU (versão 2020)."
  name <- namer(hero, lang = "pt")
  
  emig_disp <- paste0("Emigrantes da ", name)
  immig_disp <- paste0("Imigrantes na ", name)
  
  data <- plot_data("stocks", hero, use_2020 = use_2020)$data |> 
    summarise(n = sum(.data$n), .by = c(.data$t, .data$panel))
  timespan <- unique( plot_data("stocks", use_2020 = use_2020)$data$t)
  
  data <- data |>
    mutate(panel = ifelse(.data$panel == "emig", emig_disp, immig_disp))
  
  plot_elements <- list(
    facet_wrap(~.data$panel),
    geom_bar(
      stat = "identity", position = "stack", width = .8, fill = pal("blues", 2)
    ),
    labs(title = title, caption = source),
    scale_x_discrete(expand = expansion(mult = .075)),
    apply_theme("bar-vertical", basesize = basesize, font = font, facets = TRUE),
    theme(
      panel.spacing.x = unit(k(.6), "lines"),
      strip.text = element_text(
        size = basesize,
        margin = margin(t = 0, b = k())
      )
    )
  )
  
  if (nrow(data) > 0) {
    
    axis <- set_axis(data$n, "Pessoas", lang = "pt")
    
    df <- data |>
      mutate(lab = ifelse(
        .data$n == 0, 
        "", 
        prettylabel(.data$n)
      )) |> 
      complete(
        t = timespan,
        .data$panel,
        fill = list(n = 0)
      )
    
    plot <- ggplot(
      df,
      aes(x = factor(.data$t), y = .data$n)
    ) +
      plot_elements +
      geom_hline(
        yintercept = 0,
        color = pal("blues"),
        linewidth = k(.1)
      ) +
      geom_text(
        aes(label = .data$lab),
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
      t = timespan,
      panel = c(emig_disp, immig_disp),
      n = 0
    )
    
    plot <- ggplot(df, aes(x = factor(.data$t), y = .data$n)) +
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
        x = .25,
        y = .5,
        fontfamily = font,
        color = pal("blues", 3),
        size = k(3)
      ) +
      draw_label(
        "Nenhum dado",
        x = .75,
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
  
  if (!is.null(data)) {
    
    df <- data |> 
      mutate(
        group = ifelse(.data$region == "Others", "B", "A"),
        country = break_lines(.data$country)
      )
    
    df_destin <- filter(df, str_detect(.data$panel, "Destinations"))
    df_origin <- filter(df, str_detect(.data$panel, "Origins"))
    df_destin$country <- factor(df_destin$country, levels = df_destin$country)
    df_origin$country <- factor(df_origin$country, levels = df_origin$country)
    
    plot_elements <- list(
      geom_bar(stat = "identity", width = .7, show.legend = FALSE),
      
      scale_x_continuous(
        labels = function(x) paste0(x, "%"),
        expand = expansion(mult = c(.02, .15))
      ),
      scale_y_discrete(expand = expansion(mult = .15)),
      scale_fill_manual(values = c(
        "A" = pal("blues", 2), 
        "B" = pal("unblues", 3)
      )),
      scale_color_manual(values = c(
        "A" = pal("blues"), 
        "B" = pal("unblues")
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
      aes(x = .data$share, y = .data$country, fill = .data$group)
    ) +
      ggtitle(paste0("Destinations of emigrants from ", name)) +
      plot_elements +
      theme(plot.margin = margin(0, k(2), 0, 0))
    
    p_destin <- p_destin + 
      geom_text(
        aes(label = prettylabel(.data$share, pct = TRUE), color = .data$group),
        size = k(),
        family = font,
        fontface = "bold",
        hjust = 0, vjust = .5,
        nudge_x = layer_scales(p_destin)$x$range$range[2] / 40,
        show.legend = FALSE
      )
    
    p_origin <- ggplot(
      df_origin,
      aes(x = .data$share, y = .data$country, fill = .data$group)
    ) +
      ggtitle(paste0("Origins of immigrants to ", name)) +
      plot_elements +
      theme(plot.margin = margin(0, 0, 0, k(2)))
    
    p_origin <- p_origin + 
      geom_text(
        aes(label = prettylabel(.data$share, pct = TRUE), color = .data$group),
        size = k(),
        family = font,
        fontface = "bold",
        hjust = 0, vjust = .5,
        nudge_x = layer_scales(p_origin)$x$range$range[2] / 40,
        show.legend = FALSE
      )
    
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
      theme(plot.margin = margin(k(), k(2), k(), k(2)))
    
  } else {
    
    panel_empty_bar <- function(title) {
      
      plot_base <- ggplot() +
        ggtitle(title) +
        apply_theme("bar-horizontal", basesize = basesize, font = font) +
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
          y = .5,
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
      theme(plot.margin = margin(k(), k(2), k(), k(2)))
  }
  
  return(plot)
}

plot_nats_pt <- function(hero,
                         basesize,
                         font,
                         title = paste0(
                           "Destinos e origens de migrantes, ",
                           plot_data("nats", hero, use_2020 = use_2020)$range
                         ),
                         use_2020 = TRUE) {
  
  k <- function(factor = 1) factor * basesize / .pt
  source <- "Fonte: DAESNU."
  if (use_2020) source <- "Fonte: DAESNU (versão 2020)."
  name <- namer(hero, lang = "pt")
  
  data <- plot_data("nats", hero, use_2020 = use_2020)$data |> 
    mutate(country = case_when(
      country == "Others" ~ "Outros",
      .default = gdidata::countryname(
        country, 
        from = "name_text", 
        to = "name_pt"
      )
    ))
  
  if (!is.null(data)) {
    
    df <- data |> 
      mutate(
        group = ifelse(.data$region == "Others", "B", "A"),
        country = break_lines(.data$country)
      )
    
    df_destin <- filter(df, str_detect(.data$panel, "Destinations"))
    df_origin <- filter(df, str_detect(.data$panel, "Origins"))
    df_destin$country <- factor(df_destin$country, levels = df_destin$country)
    df_origin$country <- factor(df_origin$country, levels = df_origin$country)
    
    plot_elements <- list(
      geom_bar(stat = "identity", width = .7, show.legend = FALSE),
      
      scale_x_continuous(
        labels = function(x) paste0(x, "%"),
        expand = expansion(mult = c(.02, .15))
      ),
      scale_y_discrete(expand = expansion(mult = .15)),
      scale_fill_manual(values = c(
        "A" = pal("blues", 2), 
        "B" = pal("unblues", 3)
      )),
      scale_color_manual(values = c(
        "A" = pal("blues"), 
        "B" = pal("unblues")
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
      aes(x = .data$share, y = .data$country, fill = .data$group)
    ) +
      ggtitle(paste0("Destinos de emigrantes da ", name)) +
      plot_elements +
      theme(plot.margin = margin(0, k(2), 0, 0))
    
    p_destin <- p_destin + 
      geom_text(
        aes(label = prettylabel(.data$share, pct = TRUE), color = .data$group),
        size = k(),
        family = font,
        fontface = "bold",
        hjust = 0, vjust = .5,
        nudge_x = layer_scales(p_destin)$x$range$range[2] / 40,
        show.legend = FALSE
      )
    
    p_origin <- ggplot(
      df_origin,
      aes(x = .data$share, y = .data$country, fill = .data$group)
    ) +
      ggtitle(paste0("Origens de imigrantes na ", name)) +
      plot_elements +
      theme(plot.margin = margin(0, 0, 0, k(2)))
    
    p_origin <- p_origin + 
      geom_text(
        aes(label = prettylabel(.data$share, pct = TRUE), color = .data$group),
        size = k(),
        family = font,
        fontface = "bold",
        hjust = 0, vjust = .5,
        nudge_x = layer_scales(p_origin)$x$range$range[2] / 40,
        show.legend = FALSE
      )
    
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
      theme(plot.margin = margin(k(), k(2), k(), k(2)))
    
  } else {
    
    panel_empty_bar <- function(title) {
      
      plot_base <- ggplot() +
        ggtitle(title) +
        apply_theme("bar-horizontal", basesize = basesize, font = font) +
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
          "Nenhum dado",
          y = .5,
          fontfamily = font,
          color = pal("blues", 3),
          size = k(3)
        )
      
      return(plot)
    }
    
    plot <- plot_grid(
      panel_empty_bar("Destinos de emigrantes"),
      panel_empty_bar("Origens de imigrantes"),
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
      theme(plot.margin = margin(k(), k(2), k(), k(2)))
  }
  
  return(plot)
}


# Immigrant origins -------------------------------------------------------

plot_immorig <- function(hero,
                         basesize,
                         font,
                         title = paste0(
                           "Origins of migrants, ",
                           snap_data("immorig", hero, use_2020 = use_2020)$range
                         ),
                         use_2020 = TRUE) {
  
  k <- function(factor = 1) factor * basesize / .pt
  name <- namer(hero)
  source <- "Source: UN DESA."
  if (use_2020) source <- "Source: UN DESA (2020 release)."
  
  data <- snap_data("immorig", hero, use_2020 = use_2020)$data
  
  if (nrow(data) > 1) {
    
    df <- data |> 
      mutate(
        label = break_lines(label),
        group = case_when(
          from == "Others" ~ "B",
          from == "OOO" ~ "C",
          .default = "A"
        ),
        v = n / sum(n),
        annot = prettylabel(100 * v)
      )
    
    fills <- c(
      "A" = pal("blues", 2),
      "B" = pal("unblues", 2),
      "C" = pal("grays", 4)
    )
    
    plot <- ggplot(df, aes(x = from, y = v, fill = group)) +
      labs(title = title, caption = source) +
      geom_bar(stat = "identity", width = .7, show.legend = FALSE) +
      
      scale_x_discrete(
        labels = df$label,
        expand = expansion(mult = .05),
      ) +
      scale_y_continuous(
        name = "Per cent",
        label = function(x) 100 * x,
        expand = expansion(mult = c(.02, .1)),
      ) +
      scale_fill_manual(values = fills) + 
      
      apply_theme("bar-vertical", basesize = basesize, font = font) +
      theme(
        axis.title.y = element_text(
          size = basesize,
          margin = margin(r = k(2))
        ),
        axis.text.x = element_text(size = basesize - 1, lineheight = k(.35)),
      )
      
    plot <- plot +
      geom_text(
        aes(label = annot),
        color = pal("blues"),
        size = k(.9),
        vjust = 0,
        nudge_y = layer_scales(plot)$y$range$range[2] / 40,
      )
      
  } else {
    
    plot_base <- ggplot() +
      ggtitle(title) +
      apply_theme("bar-vertical", basesize = basesize, font = font) +
      theme(
        panel.background = element_rect(color = NA, fill = pal("unblues", 6)),
      )
    
    plot <- ggdraw(plot_base) +
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

