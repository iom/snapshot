

# IDMC --------------------------------------------------------------------

plot_idp <- function(hero,
                     basesize,
                     font,
                     title = NULL,
                     lang = "en") {
  
  k <- function(factor = 1) factor * basesize / .pt
  
  t0 <- snap_data("idp", hero)$range[1]
  t1 <- snap_data("idp", hero)$range[2]
  
  if (lang == "en") {
    if (is.null(title)) title <- "New internal displacements"
    source <- "Source: IDMC."
    if (hero %in% idmc_iom) {
      source <- "Source: IDMC; IOM Displacement Tracking Matrix."
    }
    causes <- c(
      "Environmental impacts" = pal("blues", 2),
      "Conflict and violence" = pal("reds", 2)
    )
    units <- "Displacements"
    nodata <- "No data"
  }
  
  if (lang == "pt") {
    if (is.null(title)) title <- "Novos deslocamentos internos"
    source <- "Fonte: IDMC."
    if (hero %in% idmc_iom) {
      source <- "Fontes: IDMC; Matriz de Rastreio de Deslocamento da OIM."
    }
    causes <- c(
      "Impactos ambientais" = pal("blues", 2),
      "Conflito e violência" = pal("reds", 2)
    )
    units <- "Deslocamentos"
    nodata <- "Nenhum dado"
  }
  
  data <- snap_data("idp", hero)$data
  
  plot_elements <- list(
    geom_bar(
      aes(x = factor(t), y = n, fill = forcats::fct_rev(cause)),
      stat = "identity",
      position = "stack",
      width = .7
    ),
    scale_fill_manual(values = causes),
    apply_theme("bar-vertical", basesize = basesize, font = font),
    theme(plot.margin = margin(k(2), k(2), k(2), k(2)))
  )
  
  if (nrow(data) > 0) {
    
    plot_title <- paste0(
      title, ", ",
      snap_data("idp", hero)$range |> paste(collapse = "\u2013")
    )
    
    df_agg <- summarise(data, n = sum(n), .by = t) |> filter(n > 0)
    axis <- set_axis(df_agg$n, units, lang = lang)
    
    df <- complete(data, t = t0:t1, cause = names(causes))
    
    plot <- ggplot(df) +
      plot_elements +
      labs(title = plot_title, caption = source) +
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
    
    plot <- plot + 
      geom_text(
        aes(x = factor(t), y = n, label = prettylabel(n)),
        df_agg,
        color = pal("blues"),
        family = font,
        fontface = "bold",
        size = k(.9),
        vjust = 0,
        nudge_y = layer_scales(plot)$y$range$range[2] / 40,
        show.legend = FALSE
      )
    
  } else {
    
    plot <- plot_empty(title, source, basesize, font, msg = nodata)
  }
  
  return(plot)
}

plot_idcause <- function(hero,
                         basesize,
                         font,
                         title = NULL,
                         lang = "en") {
  
  k <- function(factor = 1) factor * basesize / .pt
  
  if (lang == "en") {
    if (is.null(title)) title <- "Causes of new internal displacements\ndue to environmental impacts"
    source <- "Source: IDMC."
    if (hero %in% idmc_iom) {
      source <- "Source: IDMC; IOM Displacement Tracking Matrix."
    }
    nodata <- "No data"
  }
  
  if (lang == "pt") {
    if (is.null(title)) title <- "Causas dos novos deslocamentos internos\ndevido a impactos ambientais"
    source <- "Fonte: IDMC."
    if (hero %in% idmc_iom) {
      source <- "Fontes: IDMC; IOM Displacement Tracking Matrix."
    }
    nodata <- "Nenhum dado"
  }
  
  data <- snap_data("idcause", hero, lang = lang)$data
  
  if (nrow(data) > 0) {
    
    plot_title <- paste0(
      title, ", ",
      snap_data("idcause")$range |> paste(collapse = "\u2013")
    )
    
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
    
    plt_title <- ggplot() + ggtitle(plot_title) +
      apply_theme("map", basesize = basesize, font = font) + 
      theme(plot.margin = margin(0, 0, 0, 0))
    
    plt_caption <- ggplot() + labs(caption = source) +
      apply_theme("map", basesize = basesize, font = font) + 
      theme(plot.margin = margin(0, 0, 0, 0))
    
    plot <- plot_grid(
      plt_title, plot, plt_caption, 
      nrow = 3, rel_heights = c(.15, 1, .1)
    ) +
      theme(plot.margin = margin(k(), k(2), k(), k(2)))
    
  } else {
    
    plot <- plot_empty(title, source, basesize, font, msg = nodata)
  }
  
  return(plot)
}

plot_idpmap <- function(hero,
                        basesize,
                        font,
                        title = NULL,
                        lang = "en") {
  
  k <- function(factor = 1) factor * basesize / .pt
  
  if (lang == "en") {
    if (is.null(title)) title <- "Map of new internal displacements,\n2020\u20132024"
    source <- "Source: IDMC."
    if (hero %in% idmc_iom) {
      source <- "Source: IDMC; IOM Displacement Tracking Matrix."
    }
    units <- "Displacements"
    type <- "Type"
    year <- "Year"
    legend <- c("Environmental impacts", "Conflict and violence")
    nodata <- "No data"
  }
  
  if (lang == "pt") {
    if (is.null(title)) title <- "Mapa de novos deslocamentos internos,\n2020\u20132024"
    source <- "Fonte: IDMC."
    if (hero %in% idmc_iom) {
      source <- "Fontes: IDMC; IOM Displacement Tracking Matrix."
    }
    units <- "Deslocamentos"
    type <- "Tipo"
    year <- "Ano"
    legend <- c("Impactos ambientais", "Conflito e violência")
    nodata <- "Nenhum dado"
  }
  
  map <- map_data(hero)
  t0 <- min(idmc_geo$t) |> as.numeric()
  t1 <- max(idmc_geo$t) |> as.numeric()
  
  data <- idmc_geo |> 
    filter(
      geo == hero, 
      between(lon, map$lims$xlim["xmin"], map$lims$xlim["xmax"]),
      between(lat, map$lims$ylim["ymin"], map$lims$ylim["ymax"])
    )
  
  base_map <- map_base(hero)
  
  if (nrow(data) > 0) {
    
    scale <- map_scale(data$n)
    
    df <- data |> 
      complete(
        t = t0:t1,
        type = unique(idmc_geo$type), 
        fill = list(lon = min(data$lon[1]), lat = min(data$lat[1]), n = 0)
      )
    
    plot <- base_map + 
      geom_point(
        aes(x = lon, y = lat, shape = fct_rev(type), fill = t, size = n),
        df, color = pal("blues"), stroke = k(.05)
      ) +
      
      scale_shape_manual(
        name = type, 
        values = c(21, 23),
        labels = legend
      ) +
      scale_fill_steps2(
        name = year, 
        breaks = t0:t1,
        high = pal("blues", 2),
        mid = pal("greens"),
        low = pal("yellows", 2),
        midpoint = 2022,
      ) +
      scale_size_continuous(
        name = units, 
        limits = c(min(df$n), max(df$n)),
        breaks = scale$breaks,
        labels = scale$labels
      ) + 
      guides(
        shape = guide_legend(
          direction = "vertical", 
          override.aes = list(size = 1.75, shape = c(1, 5)),
          theme = theme(legend.key.spacing.y = unit(.2 * basesize, "points")),
          order = 1
        ),
        fill = guide_legend(
          direction = "vertical", 
          override.aes = list(size = 2, shape = 22, color = NA), 
          theme = theme(legend.text = element_text(margin = margin(l = -k()))),
          reverse = TRUE,
          order = 2
        ),
        size = guide_legend(
          direction = "vertical", 
          override.aes = list(shape = 1), 
          order = 3
        )
      )
    
    if (map$aspect > 1 & hero != "USA") {
      
      plot <- plot + 
        guides(fill = guide_legend(
          direction = "vertical", 
          nrow = 3,
          override.aes = list(size = 2, shape = 22, color = NA), 
          order = 2
        )) +
        theme(
          legend.spacing.y = unit(.5 * basesize, "points"),
          legend.title = element_text(
            size = basesize, 
            margin = margin(b = k(2))
          ),
          legend.position = "right"
        )
    }
    
  } else {
    
    plot <- ggdraw(base_map) + 
      draw_label(
        nodata, 
        fontfamily = font,
        color = pal("blues", 3), 
        size = k(3)
      )
  }
  
  if (hero == "USA") {
    
    alaska <- plot + 
      coord_sf(xlim = c(-172, -129), ylim = c(53, 72), expand = FALSE) + 
      theme(legend.position = "none", plot.margin = margin(0, 0, 0, 0))
    hawaii <- plot + 
      coord_sf(xlim = c(-160, -154), ylim = c(18.5, 22.3), expand = FALSE) + 
      theme(legend.position = "none", plot.margin = margin(0, 0, 0, 0))
    
    plot <- ggdraw(
      plot + coord_sf(xlim = c(-125, -66), ylim = c(23, 50), expand = FALSE)
    ) +
      cowplot::draw_plot(
        alaska, 
        x = 0, y = .3, scale = .25, halign = 0, valign = 0
      ) +
      cowplot::draw_plot(
        hawaii, 
        x = .2, y = .3, scale = .2, halign = 0, valign = 0
      )
  }
  
  plot <- map_final(plot, title, source, basesize, font)
  
  return(plot)
}


# MMP ---------------------------------------------------------------------

plot_mmp <- function(hero,
                     basesize,
                     font,
                     title = NULL,
                     lang = "en") {
  
  k <- function(factor = 1) factor * basesize / .pt
  t0 <- snap_data("mmp", hero)$range[1]
  t1 <- snap_data("mmp", hero)$range[2]
  
  if (lang == "en") {
    if (is.null(title)) title <- "Dead or missing international migrants"
    source <- "Source: IOM Missing Migrants Project."
    causes <- c(
      "Drowning"          = pal("blues", 2),
      "Transport hazards" = pal("blues", 4),
      "Harsh conditions"  = pal("greens"),
      "Accidental"        = pal("greens", 3),
      "Sickness"          = pal("yellows"),
      "Violence"          = pal("reds", 2),
      "Mixed or unknown"  = pal("grays", 3)
    )
    units <- "Persons"
    nodata <- "None recorded"
  }
  
  if (lang == "pt") {
    if (is.null(title)) title <- "Migrantes internacionais mortos ou desaparecidos"
    source <- "Fonte: Projeto Migrantes Desaparecidos da OIM."
    causes <- c(
      "Afogamento"             = pal("blues", 2),
      "Riscos de transporte"   = pal("blues", 4),
      "Condições severas"      = pal("greens"),
      "Acidental"              = pal("greens", 3),
      "Doença"                 = pal("yellows"),
      "Violência"              = pal("reds", 2),
      "Mista ou desconhecida"  = pal("grays", 3)
    )
    units <- "Pessoas"
    nodata <- "Nenhum registado"
  }
  
  data <- snap_data("mmp", hero, lang = lang)$data
  
  plot_elements <- list(
    geom_bar(
      aes(
        x = factor(t),
        y = n,
        fill = factor(cause, levels = names(causes))
      ),
      stat = "identity",
      position = "stack",
      width = .7
    ),
    scale_fill_manual(values = causes),
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
    
    plot_title <- paste0(
      title, ", ",
      snap_data("mmp", hero)$range |> paste(collapse = "\u2013")
    )
    
    df_agg <- summarise(data, n = sum(n), .by = t) |>
      filter(n > 0)
    df <- complete(data, t = t0:t1, cause = names(causes))
    
    plot <- ggplot(df) +
      plot_elements +
      labs(title = plot_title, caption = source) +
      scale_y_continuous(name = units) +
      theme(
        axis.title.y = element_text(
          size = basesize,
          margin = margin(r = k(2))
        ),
        axis.text.x = element_text(margin = margin(t = -k(.5))),
      )
    
    plot <- plot +
      geom_text(
        aes(x = factor(t), y = n, label = prettylabel(n)),
        df_agg,
        color = pal("blues"),
        family = font,
        fontface = "bold",
        size = k(.8),
        vjust = 0,
        nudge_y = layer_scales(plot)$y$range$range[2] / 40,
        show.legend = FALSE
      )
    
  } else {
    
    plot <- plot_empty(title, source, basesize, font, msg = nodata)
  }
  
  return(plot)
}

plot_mmpmap <- function(hero,
                        basesize,
                        font,
                        title = NULL,
                        lang = "en") {
  
  k <- function(factor = 1) factor * basesize / .pt
  
  if (lang == "en") {
    if (is.null(title)) title <- "Map of dead or missing international migrants,\n2014\u20132024"
    source <- "Source: IOM Missing Migrants Project."
    causes <- c(
      "Drowning"          = pal("blues", 2),
      "Transport hazards" = pal("blues", 4),
      "Harsh conditions"  = pal("greens"),
      "Accidental"        = pal("greens", 3),
      "Sickness"          = pal("yellows"),
      "Violence"          = pal("reds", 2),
      "Mixed or unknown"  = pal("grays", 3)
    )
    cause <- "Cause"
    units = "Persons"
    nodata <- "No data"
  }
  
  if (lang == "pt") {
    if (is.null(title)) title <- "Mapa de migrantes internacionais mortos ou\ndesaparecidos, 2014\u20132024"
    source <- "Fonte: Projeto Migrantes Desaparecidos da OIM."
    causes <- c(
      "Afogamento"             = pal("blues", 2),
      "Riscos de transporte"   = pal("blues", 4),
      "Condições severas"      = pal("greens"),
      "Acidental"              = pal("greens", 3),
      "Doença"                 = pal("yellows"),
      "Violência"              = pal("reds", 2),
      "Mista ou desconhecida"  = pal("grays", 3)
    )
    cause <- "Causa"
    units <- "Pessoas"
    nodata <- "Nenhum dado"
  }
  
  map <- map_data(hero)
  base_map <- map_base(hero)
  data <- snap_data("mmpmap", hero, lang = lang)$data
  
  if (nrow(data) > 0) {
    
    scale <- map_scale(data$n)
    
    df <- data |> 
      complete(
        cause = names(causes),
        fill = list(lon = min(data$lon[1]), lat = min(data$lat[1]), n = 0)
      ) |> 
      mutate(cause = factor(cause, levels = names(causes)))
    
    plot <- base_map + 
      geom_point(
        aes(x = lon, y = lat, fill = cause, size = n),
        df, shape = 21, stroke = k(.05)
      ) +
      
      scale_fill_manual(name = cause, values = causes) +
      scale_size_continuous(
        name = units,
        limits = c(min(df$n), max(df$n)),
        breaks = scale$breaks,
        labels = scale$labels
      ) + 
      guides(
        fill = guide_legend(
          direction = "vertical",
          nrow = 4,
          override.aes = list(size = 2.25, shape = 22, color = NA), 
          theme = theme(legend.text = element_text(margin = margin(l = -k()))),
          order = 1
        ),
        size = guide_legend(
          direction = "vertical",
          override.aes = list(shape = 1), 
          order = 2
        )
      )
    
    if (map$aspect > 1 & hero != "USA") {
      
      plot <- plot + 
        guides(fill = guide_legend(
          direction = "vertical", 
          override.aes = list(size = 2, shape = 22, color = NA), 
          order = 2
        )) +
        theme(
          legend.spacing.y = unit(.5 * basesize, "points"),
          legend.title = element_text(
            size = basesize, 
            margin = margin(b = k(2))
          ),
          legend.position = "right"
        )
    }
    
  } else {
    
    plot <- ggdraw(base_map) + 
      draw_label(
        nodata, 
        fontfamily = font,
        color = pal("blues", 3), 
        size = k(3)
      )
  }
  
  if (hero == "USA") {
    
    alaska <- plot + 
      coord_sf(xlim = c(-172, -129), ylim = c(53, 72), expand = FALSE) + 
      theme(legend.position = "none", plot.margin = margin(0, 0, 0, 0))
    
    hawaii <- plot + 
      coord_sf(xlim = c(-160, -154), ylim = c(18.5, 22.3), expand = FALSE) + 
      theme(legend.position = "none", plot.margin = margin(0, 0, 0, 0))
    
    plot <- ggdraw(
      plot + coord_sf(xlim = c(-125, -66), ylim = c(23, 50), expand = FALSE)
    ) +
      cowplot::draw_plot(
        alaska, 
        x = 0, y = .3, scale = .25, halign = 0, valign = 0
      ) +
      cowplot::draw_plot(
        hawaii, 
        x = .2, y = .3, scale = .2, halign = 0, valign = 0
      )
  }
  
  plot <- map_final(plot, title, source, basesize, font)
  
  return(plot)
}


# UNHCR -------------------------------------------------------------------

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





