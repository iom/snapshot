
# Migrant stocks ----------------------------------------------------------

plot_stocks <- function(hero,
                        basesize,
                        font,
                        title = NULL,
                        use_2020 = TRUE,
                        lang = "en") {
  
  k <- function(factor = 1) factor * basesize / .pt
  name <- namer(hero, lang = lang)
  
  if (lang == "en") {
    if (is.null(title)) title <- "Migrant populations"
    source <- "Source: UN DESA."
    if (use_2020) source <- "Source: UN DESA (2020 release)."
    emig_disp <- paste0("Emigrants from ", name)
    immig_disp <- paste0("Immigrants in ", name)
    persons <- "Persons"
    nodata <- "No data"
  }
  
  if (lang == "pt") {
    if (is.null(title)) title <- "Populações migrantes"
    source <- "Fonte: DAESNU."
    if (use_2020) source <- "Fonte: DAESNU (versão 2020)."
    emig_disp <- paste0("Emigrantes da ", name)
    immig_disp <- paste0("Imigrantes na ", name)
    persons <- "Pessoas"
    nodata <- "Nenhum dado"
  }
  
  data <- snap_data("stocks", hero, use_2020 = use_2020)$data |> 
    summarise(n = sum(n), .by = c(t, panel))
  timespan <- unique(snap_data("stocks", use_2020 = use_2020)$data$t)
  
  data <- data |>
    mutate(panel = ifelse(panel == "emig", emig_disp, immig_disp))
  
  plot_elements <- list(
    facet_wrap(~panel),
    geom_bar(
      stat = "identity", 
      position = "stack", 
      width = .8, 
      fill = pal("blues", 2)
    ),
    scale_x_discrete(expand = expansion(mult = .075)),
    apply_theme(
      "bar-vertical", 
      basesize = basesize, 
      font = font, 
      facets = TRUE
    ),
    theme(
      panel.spacing.x = unit(k(.6), "lines"),
      strip.text = element_text(
        size = basesize,
        margin = margin(t = 0, b = k())
      )
    )
  )
  
  if (nrow(data) > 0) {
    
    plot_title <- paste0(
      title, ", ",
      snap_data("stocks", hero, use_2020 = use_2020)$range |>
        paste(collapse = "\u2013")
    )
    
    axis <- set_axis(data$n, persons, lang = lang)
    
    df <- data |>
      mutate(lab = ifelse(
        n == 0, 
        "", 
        prettylabel(n)
      )) |> 
      complete(
        t = timespan,
        panel,
        fill = list(n = 0)
      )
    
    plot <- ggplot(df, aes(x = factor(t), y = n)) +
      plot_elements +
      geom_hline(
        yintercept = 0,
        color = pal("blues"),
        linewidth = k(.1)
      ) +
      geom_text(
        aes(label = lab),
        color = pal("blues"),
        family = font,
        fontface = "bold",
        size = k(.8),
        vjust = 0,
        nudge_y = max(df$n, na.rm = TRUE) / 40
      ) +
      labs(title = plot_title, caption = source) +
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
    
    plot <- ggplot(df, aes(x = factor(t), y = n)) +
      plot_elements +
      labs(title = title, caption = source) +
      theme(
        axis.text = element_blank(),
        axis.ticks.x = element_blank(),
        panel.background = element_rect(color = NA, fill = pal("unblues", 6)),
        panel.grid.major.y = element_blank(),
      )
    
    plot <- ggdraw(plot) +
      draw_label(
        nodata,
        x = .25,
        y = .5,
        fontfamily = font,
        color = pal("blues", 3),
        size = k(3)
      ) +
      draw_label(
        nodata,
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
                      title = NULL,
                      use_2020 = TRUE,
                      lang = "en") {
  
  k <- function(factor = 1) factor * basesize / .pt
  name <- namer(hero, lang = lang)
  
  if (lang == "en") {
    if (is.null(title)) title <- "Destinations and origins of migrants"
    source <- "Source: UN DESA."
    if (use_2020) source <- "Source: UN DESA (2020 release)."
    others <- "Others"
    panel_destin <- "Destinations of emigrants from "
    panel_origin <- "Origins of immigrants to "
    nodata <- "No data"
  }
  
  if (lang == "pt") {
    if (is.null(title)) title <- "Destinos e origens de migrantes"
    source <- "Fonte: DAESNU."
    if (use_2020) source <- "Fonte: DAESNU (versão 2020)."
    others <- "Outros"
    panel_destin <- "Destinos de emigrantes"
    panel_origin <- "Origens de imigrantes"
    nodata <- "Nenhum dado"
  }
  
  data <- snap_data("nats", hero, lang = lang, use_2020 = use_2020)$data
  
  if (!is.null(data)) {
    
    plot_title <- paste0(
      title, ", ",
      snap_data("nats", hero, use_2020 = use_2020)$range
    )
    
    df <- data |> 
      mutate(
        group = ifelse(region == others, "B", "A"),
        country = break_lines(country)
      )
    
    df_destin <- filter(df, str_detect(panel, "Destinations"))
    df_origin <- filter(df, str_detect(panel, "Origins"))
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
      aes(x = share, y = country, fill = group)
    ) +
      ggtitle(paste0(panel_destin, " da ", name)) +
      plot_elements +
      theme(plot.margin = margin(0, k(2), 0, 0))
    
    p_destin <- p_destin + 
      geom_text(
        aes(label = prettylabel(share, pct = TRUE), color = group),
        size = k(),
        family = font,
        fontface = "bold",
        hjust = 0, vjust = .5,
        nudge_x = layer_scales(p_destin)$x$range$range[2] / 40,
        show.legend = FALSE
      )
    
    p_origin <- ggplot(
      df_origin,
      aes(x = share, y = country, fill = group)
    ) +
      ggtitle(paste0(panel_origin, " na ", name)) +
      plot_elements +
      theme(plot.margin = margin(0, 0, 0, k(2)))
    
    p_origin <- p_origin + 
      geom_text(
        aes(label = prettylabel(share, pct = TRUE), color = group),
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
          nodata,
          y = .5,
          fontfamily = font,
          color = pal("blues", 3),
          size = k(3)
        )
      
      return(plot)
    }
    
    plot <- plot_grid(
      panel_empty_bar(panel_destin),
      panel_empty_bar(panel_origin),
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


# Pyramid -----------------------------------------------------------------

plot_migpyr <- function(hero,
                        basesize,
                        font,
                        title = NULL,
                        lang = "en") {
  
  k <- function(factor = 1) factor * basesize / .pt
  name <- namer(hero, lang = lang)
  
  if (lang == "en") {
    if (is.null(title)) title <- "Age and sex structure of migrants"
    source <- "Source: IOM Global Data Institute; UN DESA."
    legend_popgen <- "General population:"
    legend_popmig <- "Migrant population:"
    legend_labels <- c("Male", "Female")
    nodata <- "No data"
  }
  
  if (lang == "pt") {
    if (is.null(title)) title <- "Estrutura etária e de sexo dos migrantes"
    source <- "Fontes: Instituto Global de Dados da OIM; DAESNU."
    legend_popgen <- "População em geral:"
    legend_popmig <- "População migrante:"
    legend_labels <- c("Masculino", "Feminino")
    nodata <- "Nenhum dado"
  }
  
  data <- snap_data("migpyr", hero)$data
  
  plot_elements <- list(
    facet_wrap(~panel, scales = "free_y"),
    annotate(
      "rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 3.5,
      fill = pal("unblues", 5), alpha = .5
    ),
    annotate(
      "rect", xmin = -Inf, xmax = Inf, ymin = 13.5, ymax = Inf,
      fill = pal("unblues", 5), alpha = .5
    ),
    geom_vline(
      xintercept = 0, 
      color = pal("unblues", 4), 
      linewidth = k(.075)
    ),
    apply_theme(
      "bar-horizontal", 
      basesize = basesize, 
      font = font, 
      facets = TRUE
    ),
    scale_y_discrete(expand = expansion(mult = .05)),
    theme(
      axis.title.x = element_text(size = basesize - 2, color = pal("blues", 5)),
      axis.text.y = element_text(size = basesize - 2),
      panel.spacing.x = unit(k(), "lines"),
      plot.caption = element_text(margin = margin(t = 0, r = k(), l = k())),
      plot.margin = margin(k(4), k(4), k(.25), k(4)),
      strip.text = element_text(
        size = basesize,
        margin = margin(t = 0, b = k(2))
      )
    )
  )
  
  if (nrow(drop_na(data)) > 0) {
    
    plot_title = paste0(title, ", ", snap_data("migpyr", hero)$range)
    
    wpp <- snap_data("pyr", hero)$data
    
    extent <- max(abs(c(drop_na(data, v)$v, wpp$v)))
    
    set_breaks <- c(-4, -2, 0, 2, 4)
    set_labels <- c("4%", "2%", 0, "2%", "4%")
    if (extent >= 4.5 & extent < 6.5) {
      set_breaks <- c(-5, -2.5, 0, 2.5, 5)
      set_labels <- c("5%", "2.5%", 0, "2.5%", "5%")
    }
    if (extent >= 6.5 & extent < 9) {
      set_breaks <- c(-8, -4, 0, 4, 8)
      set_labels <- c("8%", "4%", 0, "4%", "8%")
    }
    if (extent >= 9 & extent < 17) {
      set_breaks <- c(-16, -8, 0, 8, 16)
      set_labels <- c("16%", "8%", 0, "8%", "16%")
    }
    if (extent >= 17) {
      set_breaks <- c(-20, -10, 0, 10, 20)
      set_labels <- c("20%", "10%", 0, "10%", "20%")
    }
    
    df <- data |> 
      mutate(
        lab = prettylabel(v, signif = 1, pct = TRUE),
        v = case_when(sex == "male" ~ -v, .default = v),
      )
    
    plot <- ggplot(
      df, 
      aes(x = v, y = forcats::fct_relevel(age, "5-9", after = 1))
    ) +
      plot_elements +
      geom_bar(aes(fill = sex), stat = "identity", width = .7) + 
      geom_path(aes(group = sex, color = sex), wpp) +
      labs(title = plot_title, caption = source) +
      
      scale_x_continuous(
        name = "",
        limits = c(-extent, extent),
        breaks = set_breaks,
        labels = set_labels,
        expand = expansion(mult = .05)
      ) +
      scale_fill_manual(
        name = legend_popmig,
        labels = legend_labels,
        values = c(pal("blues", 3), pal("greens", 3))
      ) + 
      scale_color_manual(
        name = legend_popgen,
        labels = legend_labels,
        values = c(pal("blues"), pal("greens"))
      ) +
      
      theme(
        legend.spacing.y = unit(10, "lines"),
        legend.title = element_text(
          size = basesize, margin = margin(r = -k(2))),
        legend.text = element_text(margin = margin(r = -k(), l = k())),
        legend.position = "bottom",
        legend.justification.bottom = "right",
        legend.box.spacing = unit(0, "lines"),
      )
    
  } else {
    
    plot <- plot_empty(title, source, basesize, font, msg = nodata)
  }
  
  return(plot)
}

plot_immpyr <- function(hero,
                        basesize,
                        font,
                        title = paste0(
                          "Age and sex structure of immigrants, ",
                          snap_data("migpyr", hero)$range
                        )) {
  
  k <- function(factor = 1) factor * basesize / .pt
  source <- "Source: UN DESA."
  name <- namer(hero)
  
  data <- snap_data("migpyr", hero)$data |> 
    filter(panel == "Immigrants")
  
  plot_elements <- list(
    annotate(
      "rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 3.5,
      fill = pal("unblues", 5), alpha = .5
    ),
    annotate(
      "rect", xmin = -Inf, xmax = Inf, ymin = 13.5, ymax = Inf,
      fill = pal("unblues", 5), alpha = .5
    ),
    geom_vline(
      xintercept = 0, 
      color = pal("unblues", 4), 
      linewidth = k(.075)
    ),
    labs(title = title, caption = source),
    apply_theme(
      "bar-horizontal", 
      basesize = basesize, 
      font = font, 
      facets = TRUE
    ),
    scale_y_discrete(expand = expansion(mult = .05)),
    theme(
      axis.title.x = element_text(size = basesize - 2, color = pal("blues", 5)),
      axis.text.y = element_text(size = basesize - 2),
      plot.caption = element_text(margin = margin(t = 0, r = k(), l = k())),
      plot.margin = margin(k(4), k(4), k(.25), k(4)),
    )
  )
  
  if (nrow(drop_na(data)) > 0) {
    
    wpp <- snap_data("pyr", hero)$data
    
    extent <- max(abs(c(drop_na(data, v)$v, wpp$v)))
    
    set_breaks <- c(-4, -2, 0, 2, 4)
    set_labels <- c("4%", "2%", 0, "2%", "4%")
    if (extent >= 4.5 & extent < 6.5) {
      set_breaks <- c(-5, -2.5, 0, 2.5, 5)
      set_labels <- c("5%", "2.5%", 0, "2.5%", "5%")
    }
    if (extent >= 6.5 & extent < 9) {
      set_breaks <- c(-8, -4, 0, 4, 8)
      set_labels <- c("8%", "4%", 0, "4%", "8%")
    }
    if (extent >= 9 & extent < 17) {
      set_breaks <- c(-16, -8, 0, 8, 16)
      set_labels <- c("16%", "8%", 0, "8%", "16%")
    }
    if (extent >= 17) {
      set_breaks <- c(-20, -10, 0, 10, 20)
      set_labels <- c("20%", "10%", 0, "10%", "20%")
    }
    
    df <- data |> 
      mutate(
        lab = prettylabel(v, signif = 1, pct = TRUE),
        v = case_when(sex == "male" ~ -v, .default = v),
      )
    
    plot <- ggplot(
      df, 
      aes(x = v, y = forcats::fct_relevel(age, "5-9", after = 1))
    ) +
      plot_elements +
      geom_bar(aes(fill = sex), stat = "identity", width = .7) + 
      geom_path(aes(group = sex, color = sex), wpp) +
      
      scale_x_continuous(
        name = "",
        limits = c(-extent, extent),
        breaks = set_breaks,
        labels = set_labels,
        expand = expansion(mult = .05)
      ) +
      scale_fill_manual(
        name = "Immigrant population",
        labels = c("Male", "Female"),
        values = c(pal("blues", 3), pal("greens", 3))
      ) + 
      scale_color_manual(
        name = "General population",
        labels = c("Male", "Female"),
        values = c(pal("blues"), pal("greens"))
      ) +
      guides(
        fill = guide_legend(nrow = 1),
        color = guide_legend(nrow = 1),
      ) +
      
      theme(
        legend.key.height = unit(basesize - 2.5, "points"),
        legend.title = element_text(
          size = basesize, margin = margin(b = k())
        ),
        legend.position = "bottom",
        legend.direction = "vertical",
        legend.box.spacing = unit(.2, "lines"),
        plot.caption = element_text(
          margin = margin(t = k(4), r = k(1), l = k(1))
        ),
      )
    
  } else {
    
    plot <- plot_empty(title, source, basesize, font)
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

