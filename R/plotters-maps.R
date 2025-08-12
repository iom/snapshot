
labeler <- function(x) {
  
  formatter <- function(n, d) {
    format(round(x, digits = d), trim = TRUE, big.mark = ",")
  }
  
  label <- case_when(
    x > 1050 ~ formatter(x, d = -2),
    x > 105 & x <= 1050 ~ formatter(x, d = -1),
    .default = formatter(x, d = 0)
  )
  
  return(label)
}


# Net migration -----------------------------------------------------------

plot_nmigmap <- function(hero,
                         basesize,
                         font,
                         title = "Map of net migration per capita, average 2016\u20132020") {
  
  k <- function(factor = 1) factor * basesize / .pt
  source <- "Source: Niva et al. (2023)."
  
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
        name = "Net migrants\nper 1000 population",
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
        name = "Net migrants\nper 1000 population",
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
        name = "Net migrants\nper 1000 population",
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

plot_nmigmap_pt <- function(hero,
                            basesize,
                            font,
                            title = "Mapa da migração líquida per capita, média 2016\u20132020") {
  
  k <- function(factor = 1) factor * basesize / .pt
  source <- "Fonte: Niva et al. (2023)."
  
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
        name = "Migrantes líquidos\npor 1000 pessoas",
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
        name = "Migrantes líquidos\npor 1000 pessoas",
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
        name = "Migrantes líquidos\npor 1000 pessoas",
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
        name = "Migrantes líquidos\npor 1000 pessoas",
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


