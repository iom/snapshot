
plot_empty <- function(title, 
                       source, 
                       basesize, 
                       font, 
                       msg = "No data") {
  
  k <- function(factor = 1) factor * basesize / .pt

  plot <- ggplot(data.frame(i = 1:10), aes(x = i)) +
    labs(title = title, caption = source) +
    apply_theme("line", basesize = basesize, font = font) +
    theme(
      axis.text = element_blank(),
      panel.background = element_rect(color = NA, fill = pal("unblues", 6)),
      plot.caption = element_text(
        margin = margin(t = k(), r = k(), l = k())
      ),
    )
  
  plot <- ggdraw(plot) +
    draw_label(
      msg,
      y = .5, 
      fontfamily = font, 
      color = pal("blues", 3), 
      size = k(3)
    )
  
  return(plot)
}


# Net migration -----------------------------------------------------------

plot_nmig <- function(hero,
                      basesize,
                      font,
                      title = NULL,
                      lang = "en") {
  
  k <- function(factor = 1) factor * basesize / .pt
  
  if (lang == "en") {
    if (is.null(title)) title <- "Net migration"
    source <- "Source: World Bank."
    persons <- "Persons"
  }
  
  if (lang == "pt") {
    if (is.null(title)) title <- "Migração líquid"
    source <- "Fonte: Banco Mundial."
    persons <- "Pessoas"
  }
  
  t0 <- snap_data("nmig",)$range[1]
  t1 <- snap_data("nmig")$range[2]
  data <- snap_data("nmig", hero)$data
  
  if (nrow(data) > 0) {
    
    plot_title <- paste0(
      title, ", ",
      snap_data("nmig", hero)$range |> paste(collapse = "\u2013")
    )
    
    axis <- set_axis(data$n, persons, lang = lang)
    
    plot <- ggplot(data, aes(x = t, y = n)) +
      geom_bar(stat = "identity", width = .7, fill = pal("blues", 2)) +
      geom_hline(yintercept = 0, color = pal("blues"), linewidth = k(.1)) +
      labs(title = plot_title, caption = source) +
      
      scale_x_continuous(
        breaks = seq(t0, t1, 10),
        expand = expansion(mult = c(.03, .03)),
        guide = guide_axis(minor.ticks = TRUE)
      ) +
      scale_y_continuous(
        name = axis$title,
        breaks = axis$breaks,
        labels = axis$labels
      ) +
      guides(
        color = guide_legend(nrow = 2),
        linetype =  guide_legend(nrow = 2)
      ) +
      
      apply_theme("bar-vertical", basesize = basesize, font = font) +
      theme(
        axis.title.y = element_text(
          size = basesize,
          margin = margin(r = k(2))
        ),
        axis.ticks.x = element_line(
          color = pal("blues"),
          linewidth = k(.05)
        ),
      )
    
  } else {
    plot <- plot_empty(title, source, basesize, font, msg = nodata)
  }
  
  return(plot)
}

plot_nmigmap <- function(hero,
                         basesize,
                         font,
                         title = NULL,
                         lang = "en") {
  
  k <- function(factor = 1) factor * basesize / .pt
  
  if (lang == "en") {
    if (is.null(title)) title <- "Map of net migration per capita, average 2016\u20132020"
    source <- "Source: Niva et al. (2023)."
    legendtitle <- "Net migrants\nper 1000 people"
  }
  
  if (lang == "pt") {
    if (is.null(title)) title <- "Mapa da migração líquida per capita, média 2016\u20132020"
    source <- "Fonte: Niva et al. (2023)."
    legendtitle <- "Migrantes líquidos\npor 1000 pessoas"
  }
  
  border_sub <- filter(admin1, adm0_a3 == hero) 
  border <- border_sub |> sf::st_union()
  
  bbox <- sf::st_bbox(border)
  border_spat <- sf::as_Spatial(border)
  rast <- terra::crop(terra::mask(nmig_rast, border_spat), border_spat)
  
  lims <- list(
    xlim = c(bbox$xmin, bbox$xmax),
    ylim = c(bbox$ymin, bbox$ymax)
  )
  
  # Manually set boundaries for countries that are smaller than 1 pixel
  small <- c("AIA", "MCO", "NRU", "VAT", "VGB", "TUV")
  if (hero == "AIA") {
    bbox_manual <- raster::extent(-63.42882, -62.92568, 18.16909, 18.60126)
    rast <- terra::crop(nmig_rast, bbox_manual)
  }
  if (hero == "CYM") {
    bbox_manual <- raster::extent(-81.41654, -79.72664, 19.3, 19.6)
    rast <- terra::crop(nmig_rast, bbox_manual)
  }
  if (hero == "MCO") {
    bbox_manual <- raster::extent(7.375000, 7.458333, 43.70833, 43.79166)
    rast <- terra::crop(nmig_rast, bbox_manual)
  }
  if (hero == "SGS") {
    bbox_manual <- raster::extent(-38.05379, -36.0565, -54.9584, -53.95915)
    rast <- terra::crop(nmig_rast, bbox_manual)
  }
  if (hero == "NRU") {
    bbox_manual <- raster::extent(
      166.9583, 166.9583 + 1/12, 
      -0.5416667, -0.5416667 + 1/12
    )
    rast <- terra::crop(nmig_rast, bbox_manual)
  }
  if (hero == "VAT") {
    bbox_manual <- raster::extent(12.37500, 12.45833, 41.87500, 41.95833)
    rast <- terra::crop(nmig_rast, bbox_manual)
  }
  if (hero == "TUV") {
    bbox_manual <- bbox
    rast <- terra::crop(nmig_rast, bbox_manual)
  }
  
  # Manually set plot area
  if (hero == "CHL") {
    lims <- list(
      xlim = c(-76, -55.91850),
      ylim = c(-66.42081, -17.50659)
    )
  }
  
  df <- terra::as.data.frame(rast, xy = TRUE, na.rm = TRUE)
  
  aspect <- (lims$ylim[2] - lims$ylim[1]) / (lims$xlim[2] - lims$xlim[1])
  
  # Background map
  map_themes <- function() {
    list(
      apply_theme("map", basesize = basesize, font = font),
      theme(
        legend.key.width = unit(1.25 * basesize, "points"),
        legend.title = element_text(
          size = basesize, 
          hjust = .5,
          margin = margin(l = k(3.5))
        ),
        legend.title.position = "right",
        legend.box.spacing = unit(k(.25), "lines"),
        plot.margin = margin(0, 0, k(2), 0)
      )
    )
  }
  
  plot <- ggplot() + 
    geom_sf(data = border, fill = pal("grays", 5), color = NA) +
    map_themes()
  
  if (length(unique(df$v)) == 1) {
    
    v <- df$v[1]
    set_labels <- prettylabel(v)
    set_fill <- ifelse(v >= 0, pal("blues", 3), pal("reds", 3))
    
    plot <- plot +
      geom_tile(mapping = aes(x = x, y = y, fill = factor(v)), data = df) + 
      scale_fill_manual(
        name = "Net migrants\nper 1000 people",
        labels = set_labels,
        values = set_fill
      ) + 
      theme(legend.text = element_text(
        size = basesize,
        hjust = .5,
        margin = margin(l = k())
      ))
    
  } else {
    
    threshold <- quantile(abs(df$v), prob = .9)
    set_limits <- c(-threshold, threshold)
    set_labels <- function(x) {
      ifelse(x == 0 | abs(x) == threshold, prettylabel(x), "")
    }
    
    plot <- plot + 
      geom_tile(aes(x = x, y = y, fill = v), df) + 
      scale_fill_steps2(
        n.breaks = 7,
        nice.breaks = FALSE, 
        name = legendtitle,
        labels = set_labels,
        show.limits = TRUE,
        limits = set_limits,
        low = pal("reds"), 
        high = pal("blues"),
        midpoint = 0
      )
  }
  
  # Superimpose borders
  plot <- plot + 
    geom_sf(
      data = border_sub, 
      fill = NA, color = pal("grays", 3), linewidth = k(.025)
    ) + 
    geom_sf(
      data = border, 
      fill = NA, color = pal("grays", 2), linewidth = k(.05)
    ) +
    coord_sf(
      xlim = lims$xlim,
      ylim = lims$ylim,
      expand = FALSE
    )
  
  # If map is long, position legend to the right 
  if ((aspect > 1 & hero != "USA") | hero %in% small) {
    plot <- plot + 
      theme(
        legend.key.height = unit(basesize, "points"),
        legend.key.width = unit(.75 * basesize, "points"),
        legend.text = element_text(margin = margin(l = k())),
        legend.text.position = "right",
        legend.title = element_text(
          size = basesize, 
          hjust = .5,
          margin = margin(b = k(3.5))
        ),
        legend.title.position = "top",
        legend.position = "right"
      )
  }
  
  # Special cases
  
  if (hero == "USA") {
    
    ask <- plot + 
      coord_sf(xlim = c(-172, -129), ylim = c(53, 72), expand = FALSE) + 
      theme(legend.position = "none", plot.margin = margin(0, 0, 0, 0))
    hwi <- plot + 
      coord_sf(xlim = c(-160, -154), ylim = c(18.5, 22.3), expand = FALSE) + 
      theme(legend.position = "none", plot.margin = margin(0, 0, 0, 0))
    
    plot <- ggdraw(
      plot + coord_sf(xlim = c(-125, -66), ylim = c(23, 50), expand = FALSE)
    ) +
      cowplot::draw_plot(ask, x = 0, y = .12, scale = .3, halign = 0, valign = 0) +
      cowplot::draw_plot(hwi, x = .25, y = .15, scale = .2, halign = 0, valign = 0)
  }
  
  if (hero == "RUS") {
    
    border_sub <- filter(admin1, adm0_a3 == hero) 
    border <- border_sub |> sf::st_union()
    
    border_main <- border |> 
      sf::st_crop(c(xmin = 19, ymin = 0, xmax = 179.9999, ymax = 82))
    border_main_sub <- border_sub |> 
      sf::st_crop(c(xmin = 19, ymin = 0, xmax = 179.9999, ymax = 82))
    
    border_cut <- border |> 
      sf::st_crop(c(xmin = -179.9999, ymin = 0, xmax = -170, ymax = 82))
    border_cut_sub <- border_sub |> 
      sf::st_crop(c(xmin = -179.9999, ymin = 0, xmax = -170, ymax = 82))
    
    border_spat_main <- sf::as_Spatial(border_main)
    rast_main <- terra::crop(
      terra::mask(nmig_rast, border_spat_main), 
      border_spat_main
    )
    df_main <- terra::as.data.frame(rast_main, xy = TRUE, na.rm = TRUE)
    
    border_spat_cut <- sf::as_Spatial(border_cut)
    rast_cut <- terra::crop(
      terra::mask(nmig_rast, border_spat_cut), 
      border_spat_cut
    )
    df_cut <- terra::as.data.frame(rast_cut, xy = TRUE, na.rm = TRUE)
    
    # Background maps
    plot_main <- ggplot() + 
      geom_sf(data = border_main, fill = pal("grays", 5), color = NA) +
      map_themes()
    plot_cut <- ggplot() + 
      geom_sf(data = border_cut, fill = pal("grays", 5), color = NA) +
      map_themes()
    
    plot_main <- plot_main + 
      geom_tile(aes(x = x, y = y, fill = v), df_main) + 
      geom_sf(
        data = border_main_sub, 
        fill = NA, color = pal("grays", 3), linewidth = k(.025)
      ) + 
      geom_sf(
        data = border_main, 
        fill = NA, color = pal("grays", 2), linewidth = k(.05)
      ) +
      scale_fill_steps2(
        n.breaks = 7,
        nice.breaks = FALSE, 
        name = legendtitle,
        labels = set_labels,
        show.limits = TRUE,
        limits = set_limits,
        low = pal("reds"), 
        high = pal("blues"),
        midpoint = 0
      )
    
    plot_cut <- plot_cut + 
      geom_tile(aes(x = x, y = y, fill = v), df_cut) + 
      geom_sf(
        data = border_cut_sub, 
        fill = NA, color = pal("grays", 3), linewidth = k(.025)
      ) + 
      geom_sf(
        data = border_cut, 
        fill = NA, color = pal("grays", 2), linewidth = k(.05)
      ) +
      scale_fill_steps2(
        n.breaks = 7,
        nice.breaks = FALSE, 
        name = legendtitle,
        labels = set_labels,
        show.limits = TRUE,
        limits = set_limits,
        low = pal("reds"), 
        high = pal("blues"),
        midpoint = 0
      ) + 
      theme(legend.position = "none")
    
    plot <- ggdraw(plot_main + theme(plot.margin = margin(0, 0, 0, 0))) +
      cowplot::draw_plot(
        plot_cut, 
        x = .88, y = .5675, scale = .19375, 
        halign = 0, valign = 0
      )
  }
  
  plot_title <- ggplot() + 
    ggtitle(title) +
    apply_theme("map", basesize = basesize, font = font) + 
    theme(plot.margin = margin(0, 0, k(2.5), 0))
  
  plot_caption <- ggplot() + 
    labs(caption = source) +
    apply_theme("map", basesize = basesize, font = font) + 
    theme(plot.margin = margin(0, 0, 0, 0))
  
  plot <- plot_grid(
    plot_title, plot, plot_caption, 
    nrow = 3,
    rel_heights = c(.100, 1, .050)
  ) +
    theme(plot.margin = margin(k(2), k(3), k(2), k(3)))
  
  return(plot)
}


# Remittances -------------------------------------------------------------

plot_remt <- function(hero,
                      basesize,
                      font,
                      title = NULL,
                      lang = "en") {
  
  k <- function(factor = 1) factor * basesize / .pt
  
  data <- snap_data("remt", hero)$data
  t0 <- snap_data("remt", hero)$range[1]
  t1 <- snap_data("remt", hero)$range[2]
  
  if (lang == "en") {
    if (is.null(title)) title <- "Remittances"
    source <- "Source: World Bank."
    remin <- "Remittance received"
    remout <- "Remittance paid"
    nodata <- "No data"
  }
  
  if (lang == "pt") {
    if (is.null(title)) title <- "Remessas"
    source <- "Fonte: Banco Mundial."
    remin <- "Remessa recebida"
    remout <- "Remessa paga"
    nodata <- "Nenhum dado"
  }
  
  vars <- c("remin" = pal("blues", 2), "remout" = pal("reds", 2))
  
  plot_elements <- list(
    geom_line(
      aes(x = t, y = n, color = var, group = var),
      linewidth = k(.3),
      na.rm = TRUE
    ),
    scale_x_continuous(
      minor_breaks = seq(t0, t1, 5),
      expand = expansion(mult = c(.025, .150)),
      guide = guide_axis(minor.ticks = TRUE)
    ),
    scale_color_manual(
      label = c(remin, remout),
      values = vars
    ),
    scale_fill_manual(values = vars),
    coord_cartesian(clip = "off"),
    apply_theme("line", basesize = basesize, font = font),
    theme(
      axis.ticks.x = element_line(color = pal("blues"), linewidth = k(.05)),
    )
  )
  
  if (nrow(data) > 0) {
    
    plot_title <- paste0(
      title, " ,",
      snap_data("remt", hero)$range |> paste(collapse = "\u2013")
    )
    
    axis <- set_axis(data$n, "USD")
    
    endpts <- data |>
      filter(t %in% c(max(t), max(t) - 1), .by = var) |>
      drop_na() |>
      mutate(pt = 1:n(), .by = var) |>
      filter(max(pt) == 1, .by = var)
    
    df <- complete(data, t = t0:t1, var = c("remin", "remout"))
    
    lab <- df |>
      drop_na(n) |>
      filter(t == max(t), .by = var) |>
      mutate(lab_n = prettylabel(n, signif = 3, currency = "$"))
    
    plot <- ggplot(df) +
      geom_point(
        aes(x = t, y = n, color = var), endpts,
        size = .5,
        shape = 15,
        show.legend = FALSE
      ) +
      geom_segment(
        aes(x = t, y = n), lab,
        xend = max(t1) + 1,
        color = "black",
        linetype = "11",
        linewidth = k(.1)
      ) +
      labs(title = plot_title, caption = source) +
      geom_label(
        aes(y = n, fill = var, label = lab_n), lab,
        x = max(t1) + 1,
        color = "white",
        size = k(.9),
        fontface = "bold",
        family = font,
        hjust = 0,
        vjust = .5,
        label.r = unit(.05, "lines"),
        label.size = .1,
        show.legend = FALSE,
      ) +
      scale_y_continuous(
        name = axis$title,
        breaks = axis$breaks,
        labels = axis$labels
      ) +
      plot_elements +
      theme(axis.title.y = element_text(
        size = basesize,
        margin = margin(r = k(2))
      ))
    
  } else {
    
    plot <- plot_empty(title, source, basesize, font, msg = nodata)
  }
  
  return(plot)
}


# FDI ---------------------------------------------------------------------

plot_fdi <- function(hero,
                     basesize,
                     font,
                     title = NULL,
                     lang = "en") {
  
  k <- function(factor = 1) factor * basesize / .pt
  
  if (lang == "en") {
    if (is.null(title)) title <- "Foreign direct investment"
    source <- "Source: World Bank."
    fdiin <- "FDI inflow"
    fdiout <- "FDI outflow"
    nodata <- "No data"
  }
  
  if (lang == "pt") {
    if (is.null(title)) title <- "Investimento estrangeiro direto"
    source <- "Fonte: Banco Mundial."
    fdiin <- "Entrada de IED"
    fdiout <- "Saída de IED"
    nodata <- "Nenhum dado"
  }
  
  data <- snap_data("fdi", hero)$data
  t0 <- snap_data("fdi", hero)$range[1]
  t1 <- snap_data("fdi", hero)$range[2]
  
  vars <- c("fdiin" = pal("blues", 2), "fdiout" = pal("reds", 2))
  
  plot_elements <- list(
    geom_line(
      aes(x = t, y = n, color = var, group = var),
      linewidth = k(.3),
      na.rm = TRUE
    ),
    scale_x_continuous(
      minor_breaks = seq(t0, t1, 5),
      expand = expansion(mult = c(.025, .150)),
      guide = guide_axis(minor.ticks = TRUE)
    ),
    scale_color_manual(label = c(fdiin, fdiout), values = vars),
    scale_fill_manual(values = vars),
    coord_cartesian(clip = "off"),
    apply_theme("line", basesize = basesize, font = font),
    theme(
      axis.ticks.x = element_line(color = pal("blues"), linewidth = k(.05)),
    )
  )
  
  if (nrow(data) > 0) {
    
    axis <- set_axis(data$n, "USD")
    
    plot_title <- paste0(
      title, ", ",
      snap_data("fdi", hero)$range |> paste(collapse = "\u2013")
    )
    
    endpts <- data |>
      filter(t %in% c(max(t), max(t) - 1), .by = var) |>
      drop_na() |>
      mutate(pt = 1:n(), .by = var) |>
      filter(max(pt) == 1, .by = var)
    
    df <- complete(data, t = t0:t1, var = c("fdiin", "fdiout"))
    
    lab <- df |>
      drop_na(n) |>
      filter(t == max(t), .by = var) |>
      mutate(lab_n = prettylabel(n, signif = 3, currency = "$"))
    
    plot <- ggplot(df) +
      geom_point(
        aes(x = t, y = n, color = var), endpts,
        size = .5,
        shape = 15,
        show.legend = FALSE
      ) +
      geom_segment(
        aes(x = t, y = n), lab,
        xend = max(t1) + 1,
        color = "black",
        linetype = "11",
        linewidth = k(.1)
      ) +
      geom_label(
        aes(y = n, fill = var, label = lab_n), lab,
        x = max(t1) + 1,
        color = "white",
        size = k(.9),
        fontface = "bold",
        family = font,
        hjust = 0,
        vjust = .5,
        label.r = unit(.05, "lines"),
        label.size = .1,
        show.legend = FALSE,
      ) +
      labs(title = plot_title, caption = source) +
      scale_y_continuous(
        name = axis$title,
        breaks = axis$breaks,
        labels = axis$labels
      ) +
      plot_elements +
      theme(axis.title.y = element_text(
        size = basesize,
        margin = margin(r = k(2))
      ))
    
  } else {
    
    plot <- plot_empty(title, source, basesize, font, msg = nodata)
  }
  
  return(plot)
}


# Population --------------------------------------------------------------

plot_pop <- function(hero,
                     basesize,
                     font,
                     title = NULL,
                     lang = "en") {
  
  k <- function(factor = 1) factor * basesize / .pt
  name <- namer(hero, lang = lang)
  
  data <- snap_data("pop", hero, lang = lang)$data
  t0 <- snap_data("pop", hero)$range[1]
  t1 <- snap_data("pop", hero)$range[2]
  
  if (lang == "en") {
    if (is.null(title)) title <- "Population"
    source <- "Source: UN DESA."
    persons <- "Persons"
    worldmed <- "World median"
    name_text <- "name_text"
    nodata <- "No data"
  }
  
  if (lang == "pt") {
    if (is.null(title)) title <- "População"
    source <- "Fonte: DAESNU."
    persons <- "Pessoas"
    worldmed <- "Mediana mundial"
    name_text <- "name_pt"
    nodata <- "Nenhum dado"
  }
  
  if (nrow(data) > 0) {
    
    axis <- set_axis(data$n, persons, lang = lang)
    
    plot_title <- paste0(
      title, ",\n",
      snap_data("pop", hero)$range |> paste(collapse = "\u2013")
    )
    
    endpts <- data |>
      filter(t %in% c(max(t), max(t) - 1), .by = var) |>
      drop_na() |>
      mutate(pt = 1:n(), .by = var) |>
      filter(max(pt) == 1, .by = var)
    
    df <- complete(
      data, 
      t = t0:t1, 
      var = c(countryname(hero, to = name_text), worldmed)
    )
    
    lab <- df |>
      drop_na(n) |>
      filter(var != worldmed) |>
      filter(t == max(t)) |>
      mutate(lab_n = prettylabel(n))

    plot <- ggplot(
      df, 
      aes(x = t, y = n, color = var, linetype = var, group = var)
    ) + 
      geom_line(linewidth = k(.25), na.rm = TRUE) + 
      geom_segment(
        aes(x = t, y = n), lab,
        xend = t1,
        color = "black",
        linetype = "11",
        linewidth = k(.1)
      ) +
      geom_label(
        aes(y = n, fill = var, label = lab_n), lab, 
        x = t1,
        color = "white", 
        fill = pal("blues", 2),
        size = k(.9), 
        fontface = "bold", 
        family = font,
        hjust = 0, 
        vjust = .5, 
        label.r = unit(.05, "lines"), 
        label.size = .1,
        show.legend = FALSE,
      ) +
      labs(title = plot_title, caption = source) +
      
      scale_x_continuous(
        limits = c(t0, t1),
        breaks = c(1950, 1990, 2020),
        expand = expansion(mult = c(.025, .125)),
      ) +
      scale_y_continuous(
        name = axis$title,
        breaks = axis$breaks,
        labels = axis$labels,
      ) +
      scale_color_manual(
        values = c(pal("blues", 2), "black"),
        guide = guide_legend(nrow = 2)
      ) +
      coord_cartesian(clip = "off") +
      
      apply_theme("line", basesize = basesize, font = font) +
      theme(
        axis.title.y = element_text(
          size = basesize,
          margin = margin(r = k(2))
        ),
        axis.ticks.x = element_line(color = pal("blues"), linewidth = k(.05)),
      )
    
  } else {
    
    plot <- plot_empty(title, source, basesize, font, msg = nodata)
  }
  
  return(plot)
}


# Birth rate --------------------------------------------------------------

plot_birth <- function(hero,
                       basesize,
                       font,
                       title = NULL,
                       lang = "en") {
  
  k <- function(factor = 1) factor * basesize / .pt
  name <- namer(hero, lang = lang)
  
  data <- snap_data("birth", hero, lang = lang)$data
  t0 <- snap_data("birth", hero)$range[1]
  t1 <- snap_data("birth", hero)$range[2]
  
  if (lang == "en") {
    if (is.null(title)) title <- "Birth rate"
    source <- "Source: World Bank."
    worldmed <- "World median"
    name_text <- "name_text"
    yaxis <- "Births per 1000 population"
    nodata <- "No data"
  }
  
  if (lang == "pt") {
    if (is.null(title)) title <- "Taxa de natalidade"
    source <- "Fonte: Banco Mundial."
    worldmed <- "Mediana mundial"
    name_text <- "name_pt"
    yaxis <- "Nascimentos por 1000 habitantes"
    nodata <- "Nenhum dado"
  }
  
  if (nrow(data) > 0) {
    
    plot_title <- paste0(
      title, ",\n",
      snap_data("birth", hero)$range |> paste(collapse = "\u2013")
    )
    
    endpts <- data |>
      filter(t %in% c(max(t), max(t) - 1), .by = var) |>
      drop_na() |>
      mutate(pt = 1:n(), .by = var) |>
      filter(max(pt) == 1, .by = var)
    
    df <- complete(
      data, 
      t = t0:t1, 
      var = c(countryname(hero, to = name_text), worldmed)
    )
    
    lab <- df |>
      drop_na(n) |>
      filter(var != worldmed) |>
      filter(t == max(t)) |>
      mutate(lab_n = prettylabel(n))
    
    plot <- ggplot(
      df, 
      aes(x = t, y = n, color = var, linetype = var, group = var)
    ) + 
      geom_line(linewidth = k(.25), na.rm = TRUE) + 
      geom_segment(
        aes(x = t, y = n), lab,
        xend = t1,
        color = "black",
        linetype = "11",
        linewidth = k(.1)
      ) +
      geom_label(
        aes(y = n, fill = var, label = lab_n), lab, 
        x = t1,
        color = "white", 
        fill = pal("blues", 2),
        size = k(.9), 
        fontface = "bold", 
        family = font,
        hjust = 0, 
        vjust = .5, 
        label.r = unit(.05, "lines"), 
        label.size = .1,
        show.legend = FALSE,
      ) +
      labs(title = plot_title, caption = source) +
      
      scale_x_continuous(
        limits = c(t0, t1),
        breaks = seq(t0, t1, 10),
        minor_breaks = seq(t0, t1, 5),
        expand = expansion(mult = c(.025, .125)),
        guide = guide_axis(minor.ticks = TRUE)
      ) +
      scale_y_continuous(name = yaxis) +
      scale_color_manual(
        values = c(pal("blues", 2), "black"),
        guide = guide_legend(nrow = 2)
      ) +
      coord_cartesian(clip = "off") +
      
      apply_theme("line", basesize = basesize, font = font) +
      theme(
        axis.title.y = element_text(
          size = basesize,
          margin = margin(r = k(2))
        ),
        axis.ticks.x = element_line(color = pal("blues"), linewidth = k(.05)),
      )
    
  } else {
    
    plot <- plot_empty(title, source, basesize, font, msg = nodata)
  }
  
  return(plot)
}


# Dependency ratio --------------------------------------------------------

plot_depend <- function(hero,
                        basesize,
                        font,
                        title = NULL,
                        lang = "en") {
  
  k <- function(factor = 1) factor * basesize / .pt
  name <- namer(hero, lang = lang)
  
  data <- snap_data("depend", hero, lang = lang)$data
  t0 <- snap_data("depend", hero)$range[1]
  t1 <- snap_data("depend", hero)$range[2]
  
  if (lang == "en") {
    if (is.null(title)) title <- "Age dependency ratio"
    source <- "Source: World Bank."
    worldmed <- "World median"
    name_text <- "name_text"
    yaxis <- "Dependents per 100 working age"
    nodata <- "No data"
  }
  
  if (lang == "pt") {
    if (is.null(title)) title <- "Taxa de dependência\ndemográfica"
    source <- "Fonte: Banco Mundial."
    worldmed <- "Mediana mundial"
    name_text <- "name_pt"
    yaxis <- "Dependentes por 100 em idade ativa"
    nodata <- "Nenhum dado"
  }
  
  if (nrow(data) > 0) {
    
    if (lang == "en") title <- "Age dependency ratio,\n"
    if (lang == "pt") title <- "Taxa de dependência\ndemográfica, "
    
    plot_title <- paste0(
      title,
      snap_data("depend", hero)$range |> paste(collapse = "\u2013")
    )
    
    endpts <- data |>
      filter(t %in% c(max(t), max(t) - 1), .by = var) |>
      drop_na() |>
      mutate(pt = 1:n(), .by = var) |>
      filter(max(pt) == 1, .by = var)
    
    df <- complete(
      data, 
      t = t0:t1, 
      var = c(countryname(hero, to = name_text), worldmed)
    )
    
    lab <- df |>
      drop_na(n) |>
      filter(var != worldmed) |>
      filter(t == max(t)) |>
      mutate(lab_n = prettylabel(n))
    
    plot <- ggplot(
      df, 
      aes(x = t, y = n, color = var, linetype = var, group = var)
    ) + 
      geom_line(linewidth = k(.25), na.rm = TRUE) + 
      geom_segment(
        aes(x = t, y = n), lab,
        xend = t1,
        color = "black",
        linetype = "11",
        linewidth = k(.1)
      ) +
      geom_label(
        aes(y = n, fill = var, label = lab_n), lab, 
        x = t1,
        color = "white", 
        fill = pal("blues", 2),
        size = k(.9), 
        fontface = "bold", 
        family = font,
        hjust = 0, 
        vjust = .5, 
        label.r = unit(.05, "lines"), 
        label.size = .1,
        show.legend = FALSE,
      ) +
      labs(title = plot_title, caption = source) +
      
      scale_x_continuous(
        limits = c(t0, t1),
        breaks = seq(t0, t1, 10),
        minor_breaks = seq(t0, t1, 5),
        expand = expansion(mult = c(.025, .125)),
        guide = guide_axis(minor.ticks = TRUE)
      ) +
      scale_y_continuous(name = yaxis) +
      scale_color_manual(
        values = c(pal("blues", 2), "black"),
        guide = guide_legend(nrow = 2)
      ) +
      coord_cartesian(clip = "off") +
      
      apply_theme("line", basesize = basesize, font = font) +
      theme(
        axis.title.y = element_text(
          size = basesize,
          margin = margin(r = k(2))
        ),
        axis.ticks.x = element_line(color = pal("blues"), linewidth = k(.05)),
      )
    
  } else {
    
    plot <- plot_empty(title, source, basesize, font, msg = nodata)
  }
  
  return(plot)
}


# Income ------------------------------------------------------------------

plot_income <- function(hero,
                        basesize,
                        font,
                        title = NULL,
                        lang = "en") {
  
  k <- function(factor = 1) factor * basesize / .pt
  name <- namer(hero, lang = lang)
  
  data <- snap_data("income", hero, lang = lang)$data
  t0 <- snap_data("income", hero)$range[1]
  t1 <- snap_data("income", hero)$range[2]
  
  if (lang == "en") {
    if (is.null(title)) title <- "GDP per capita"
    source <- "Source: World Bank."
    worldmed <- "World median"
    name_text <- "name_text"
    nodata <- "No data"
  }
  
  if (lang == "pt") {
    if (is.null(title)) title <- "PIB per capita"
    source <- "Fonte: Banco Mundial."
    worldmed <- "Mediana mundial"
    name_text <- "name_pt"
    nodata <- "Nenhum dado"
  }
  
  if (nrow(data) > 0) {
    
    axis <- set_axis(data$n, "$")
    
    plot_title <- paste0(
      title, ",\n",
      snap_data("income", hero)$range |> paste(collapse = "\u2013")
    )
    
    endpts <- data |>
      filter(t %in% c(max(t), max(t) - 1), .by = var) |>
      drop_na() |>
      mutate(pt = 1:n(), .by = var) |>
      filter(max(pt) == 1, .by = var)
    
    df <- complete(
      data, 
      t = t0:t1, 
      var = c(countryname(hero, to = name_text), worldmed)
    )
    
    lab <- df |>
      drop_na(n) |>
      filter(var != worldmed) |>
      filter(t == max(t)) |>
      mutate(lab_n = prettylabel(n, currency = "$"))
    
    plot <- ggplot(
      df, 
      aes(x = t, y = n, color = var, linetype = var, group = var)
    ) + 
      geom_line(linewidth = k(.25), na.rm = TRUE) + 
      geom_segment(
        aes(x = t, y = n), lab,
        xend = t1,
        color = "black",
        linetype = "11",
        linewidth = k(.1)
      ) +
      geom_label(
        aes(y = n, fill = var, label = lab_n), lab, 
        x = t1,
        color = "white", 
        fill = pal("blues", 2),
        size = k(.9), 
        fontface = "bold", 
        family = font,
        hjust = 0, 
        vjust = .5, 
        label.r = unit(.05, "lines"), 
        label.size = .1,
        show.legend = FALSE,
      ) +
      labs(title = plot_title, caption = source) +
      
      scale_x_continuous(
        limits = c(t0, t1),
        breaks = seq(t0, t1, 10),
        minor_breaks = seq(t0, t1, 5),
        expand = expansion(mult = c(.025, .125)),
        guide = guide_axis(minor.ticks = TRUE)
      ) +
      scale_y_continuous(
        name = axis$title,
        breaks = axis$breaks,
        labels = axis$labels,
      ) +
      scale_color_manual(
        values = c(pal("blues", 2), "black"),
        guide = guide_legend(nrow = 2)
      ) +
      coord_cartesian(clip = "off") +
      
      apply_theme("line", basesize = basesize, font = font) +
      theme(
        axis.title.y = element_text(
          size = basesize,
          margin = margin(r = k(2))
        ),
        axis.ticks.x = element_line(color = pal("blues"), linewidth = k(.05)),
      )
    
  } else {
    
    plot <- plot_empty(title, source, basesize, font, msg = nodata)
  }
  
  return(plot)
}


# Inflation ---------------------------------------------------------------

plot_inf <- function(hero,
                     basesize,
                     font,
                     title = NULL,
                     lang = "en") {
  
  k <- function(factor = 1) factor * basesize / .pt
  name <- namer(hero, lang = lang)
  
  data <- snap_data("inf", hero, lang = lang)$data
  t0 <- snap_data("inf", hero)$range[1]
  t1 <- snap_data("inf", hero)$range[2]
  
  if (lang == "en") {
    if (is.null(title)) title <- "Inflation rate"
    source <- "Source: World Bank."
    worldmed <- "World median"
    name_text <- "name_text"
    yaxis <- "Per cent"
    nodata <- "No data"
  }
  
  if (lang == "pt") {
    if (is.null(title)) title <- "Taxa de inflação"
    source <- "Fonte: Banco Mundial."
    worldmed <- "Mediana mundial"
    name_text <- "name_pt"
    yaxis <- "Por cento"
    nodata <- "Nenhum dado"
  }
  
  if (nrow(data) > 0) {
    
    plot_title <- paste0(
      title, ",\n",
      snap_data("inf", hero)$range |> paste(collapse = "\u2013")
    )
    
    endpts <- data |>
      filter(t %in% c(max(t), max(t) - 1), .by = var) |>
      drop_na() |>
      mutate(pt = 1:n(), .by = var) |>
      filter(max(pt) == 1, .by = var)
    
    df <- complete(
      data, 
      t = t0:t1, 
      var = c(countryname(hero, to = name_text), worldmed)
    )
    
    lab <- df |>
      drop_na(n) |>
      filter(var != worldmed) |>
      filter(t == max(t)) |>
      mutate(lab_n = prettylabel(n, pct = TRUE, signif = 1))
    
    plot <- ggplot(
      df, 
      aes(x = t, y = n, color = var, linetype = var, group = var)
    ) + 
      geom_line(linewidth = k(.25), na.rm = TRUE) + 
      geom_segment(
        aes(x = t, y = n), lab,
        xend = t1,
        color = "black",
        linetype = "11",
        linewidth = k(.1)
      ) +
      geom_label(
        aes(y = n, fill = var, label = lab_n), lab, 
        x = t1,
        color = "white", 
        fill = pal("blues", 2),
        size = k(.9), 
        fontface = "bold", 
        family = font,
        hjust = 0, 
        vjust = .5, 
        label.r = unit(.05, "lines"), 
        label.size = .1,
        show.legend = FALSE,
      ) +
      labs(title = plot_title, caption = source) +
      
      scale_x_continuous(
        limits = c(t0, t1),
        breaks = seq(t0, t1, 10),
        minor_breaks = seq(t0, t1, 5),
        expand = expansion(mult = c(.025, .125)),
        guide = guide_axis(minor.ticks = TRUE)
      ) +
      scale_y_continuous(name = yaxis) +
      scale_color_manual(
        values = c(pal("blues", 2), "black"),
        guide = guide_legend(nrow = 2)
      ) +
      coord_cartesian(clip = "off") +
      
      apply_theme("line", basesize = basesize, font = font) +
      theme(
        axis.title.y = element_text(
          size = basesize,
          margin = margin(r = k(2))
        ),
        axis.ticks.x = element_line(color = pal("blues"), linewidth = k(.05)),
      )
    
  } else {
    
    plot <- plot_empty(title, source, basesize, font, msg = nodata)
  }
  
  return(plot)
}


# Unemployment rate -------------------------------------------------------

plot_unem <- function(hero,
                      basesize,
                      font,
                      title = NULL,
                      lang = "en") {
  
  k <- function(factor = 1) factor * basesize / .pt
  name <- namer(hero, lang = lang)
  
  data <- snap_data("unem", hero, lang = lang)$data
  t0 <- snap_data("unem", hero)$range[1]
  t1 <- snap_data("unem", hero)$range[2]
  
  if (lang == "en") {
    if (is.null(title)) title <- "Unemployment rate"
    source <- "Source: ILO."
    worldmed <- "World median"
    name_text <- "name_text"
    yaxis <- "Per cent"
    nodata <- "No data"
  }
  
  if (lang == "pt") {
    if (is.null(title)) title <- "Taxa de desemprego"
    source <- "Fonte: OIT."
    worldmed <- "Mediana mundial"
    name_text <- "name_pt"
    yaxis <- "Por cento"
    nodata <- "Nenhum dado"
  }
  
  if (nrow(data) > 0) {
    
    plot_title <- paste0(
      title, ",\n",
      snap_data("unem", hero)$range |> paste(collapse = "\u2013")
    )
    
    endpts <- data |>
      filter(t %in% c(max(t), max(t) - 1), .by = var) |>
      drop_na() |>
      mutate(pt = 1:n(), .by = var) |>
      filter(max(pt) == 1, .by = var)
    
    df <- complete(
      data, 
      t = t0:t1, 
      var = c(countryname(hero, to = name_text), worldmed)
    )
    
    lab <- df |>
      drop_na(n) |>
      filter(var != worldmed) |>
      filter(t == max(t)) |>
      mutate(lab_n = prettylabel(n, pct = TRUE, signif = 1))
    
    plot <- ggplot(
      df, 
      aes(x = t, y = n, color = var, linetype = var, group = var)
    ) + 
      geom_line(linewidth = k(.25), na.rm = TRUE) + 
      geom_segment(
        aes(x = t, y = n), lab,
        xend = t1,
        color = "black",
        linetype = "11",
        linewidth = k(.1)
      ) +
      geom_label(
        aes(y = n, fill = var, label = lab_n), lab, 
        x = t1,
        color = "white", 
        fill = pal("blues", 2),
        size = k(.9), 
        fontface = "bold", 
        family = font,
        hjust = 0, 
        vjust = .5, 
        label.r = unit(.05, "lines"), 
        label.size = .1,
        show.legend = FALSE,
      ) +
      labs(title = plot_title, caption = source) +
      
      scale_x_continuous(
        limits = c(t0, t1),
        breaks = seq(t0, t1, 10),
        minor_breaks = seq(t0, t1, 5),
        expand = expansion(mult = c(.025, .125)),
        guide = guide_axis(minor.ticks = TRUE)
      ) +
      scale_y_continuous(name = yaxis) +
      scale_color_manual(
        values = c(pal("blues", 2), "black"),
        guide = guide_legend(nrow = 2)
      ) +
      coord_cartesian(clip = "off") +
      
      apply_theme("line", basesize = basesize, font = font) +
      theme(
        axis.title.y = element_text(
          size = basesize,
          margin = margin(r = k(2))
        ),
        axis.ticks.x = element_line(color = pal("blues"), linewidth = k(.05)),
      )
    
  } else {
    
    plot <- plot_empty(title, source, basesize, font, msg = nodata)
  }
  
  return(plot)
}


# Immigrant dependency ratio ----------------------------------------------

plot_immdep <- function(hero,
                        basesize,
                        font,
                        title = "Age dependency ratio") {
  
  k <- function(factor = 1) factor * basesize / .pt
  source <- "Source: UN DESA; World Bank."
  data <- snap_data("immdep", hero)$data
  timespan <- unique(migdemog$t)
  
  if (nrow(data) > 0) {
    
    plot_title <- paste0(
      title, ", ", 
      snap_data("immdep", hero)$range |> paste(collapse = "\u2013")
    )
    
    t0 <- min(timespan)
    t1 <- max(timespan)
    
    endpts <- filter(data, t %in% c(max(t), max(t) - 1), .by = var) |> 
      drop_na() |> 
      mutate(pt = 1:n(), .by = var) |> 
      filter(max(pt) == 1, .by = var)
    
    df <- complete(data, t = timespan, var = c("gen", "immig"))
    
    lab <- data |> 
      filter(t == max(t)) |> 
      mutate(lab = round(v, digits = 0))
    
    plot <- ggplot(df, aes(x = t, y = v, color = var, group = var)) +
      geom_line(linewidth = k(.35), na.rm = TRUE) + 
      geom_point(
        aes(x = t, y = v, color = var), endpts,
        size = .5, shape = 15, show.legend = FALSE
      ) + 
      
      # Annotations
      geom_segment(
        aes(x = t, y = v), lab, 
        xend = t1 + 1,
        color = "black", 
        linetype = "11", 
        linewidth = k(.1)
      ) +
      geom_label(
        aes(y = v, fill = var, label = lab), lab, 
        x = t1 + 1,
        color = "white", 
        size = k(.9), 
        fontface = "bold", 
        family = font,
        hjust = 0, 
        vjust = .5, 
        label.r = unit(.05, "lines"), 
        label.size = .1,
        show.legend = FALSE,
      ) +
      labs(title = plot_title, caption = source) +
      
      scale_x_continuous(
        expand = expansion(mult = c(.03, .05)),
        guide = guide_axis(minor.ticks = TRUE)
      ) +
      scale_y_continuous(name = "Dependents per\n100 working age persons") +
      scale_fill_manual(values = c(
        "gen" = pal("blues", 2), 
        "immig" = pal("greens")
      )) +
      scale_color_manual(
        label = c("General population", "Immigrant population"),
        values = c(
          "gen" = pal("blues", 2), 
          "immig" = pal("greens")
        )
      ) +
      coord_cartesian(clip = "off") +
      
      # Aesthetics
      apply_theme(type = "line", basesize = basesize, font = font) +
      theme(
        axis.title.y = element_text(
          size = basesize,
          margin = margin(r = k(2))
        ),
        axis.ticks.x = element_line(color = pal("blues"), linewidth = k(.05)),
        plot.margin = margin(k(4), k(4), k(.25), k(4))
      )
    
  } else {
    plot <- plot_empty(title, source, basesize, font)
  }
  
  return(plot)
}




