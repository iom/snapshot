

plot_idpmap <- function(hero,
                        basesize,
                        font,
                        title = "Map of new internal displacements,\n2020\u20132024") {
  
  k <- function(factor = 1) factor * basesize / .pt
  
  source <- "Source: IDMC."
  if (hero %in% idmc_iom) {
    source <- "Source: IDMC; IOM Displacement Tracking Matrix."
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
      
      # Scales
      scale_shape_manual(name = "Type", values = c(21, 23)) +
      scale_fill_steps2(
        name = "Year", 
        breaks = t0:t1,
        high = pal("blues", 2),
        mid = pal("greens"),
        low = pal("yellows", 2),
        midpoint = 2022,
      ) +
      scale_size_continuous(
        name = "Displacements", 
        limits = c(min(df$n), max(df$n)),
        breaks = scale$breaks,
        labels = scale$labels
      ) + 
      
      # Guides
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
        "No data", 
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


# Dead and missing migrants -----------------------------------------------

plot_mmpmap <- function(hero,
                        basesize,
                        font,
                        title = "Map of dead or missing international migrants,\n2014\u20132024") {
  
  k <- function(factor = 1) factor * basesize / .pt
  
  source <- "Source: IOM Missing Migrants Project."
  
  map <- map_data(hero)
  t0 <- min(gdidata::iom_mmp$t) |> as.numeric()
  t1 <- max(gdidata::iom_mmp$t) |> as.numeric()

  causes <- c(
    "Drowning"          = pal("blues", 2),
    "Transport hazards" = pal("blues", 4),
    "Harsh conditions"  = pal("greens"),
    "Accidental"        = pal("greens", 3),
    "Sickness"          = pal("yellows"),
    "Violence"          = pal("reds", 2),
    "Mixed or unknown"  = pal("grays", 3)
  )
  
  data <- gdidata::iom_mmp |> 
    filter(
      geo == hero,
      between(lon, map$lims$xlim["xmin"], map$lims$xlim["xmax"]),
      between(lat, map$lims$ylim["ymin"], map$lims$ylim["ymax"])
    ) |> 
    mutate(
      t = year(t),
      cause = case_when(
        str_detect(cause, "Accidental") ~ "Accidental",
        str_detect(cause, "Harsh")      ~ "Harsh conditions",
        str_detect(cause, "Sickness")   ~ "Sickness",
        str_detect(cause, "transport")  ~ "Transport hazards",
        .default = cause
      )
    ) |> 
    summarise(n = sum(dead), .by = c(t, cause, lon, lat)) |> 
    drop_na() |> 
    arrange(desc(n))
  
  base_map <- map_base(hero)
  
  if (nrow(data) > 0) {
    
    scale <- map_scale(data$n)
    
    df <- data |> 
      complete(
        t = t0:t1,
        cause = names(causes),
        fill = list(lon = min(data$lon[1]), lat = min(data$lat[1]), n = 0)
      ) |> 
      mutate(cause = factor(cause, levels = names(causes)))
    
    plot <- base_map + 
      geom_point(
        aes(x = lon, y = lat, fill = cause, size = n),
        df, shape = 21, stroke = k(.05)
      ) +
      
      # Scales
      scale_fill_manual(name = "Cause", values = causes) +
      scale_size_continuous(
        name = "Persons",
        limits = c(min(df$n), max(df$n)),
        breaks = scale$breaks,
        labels = scale$labels
      ) + 
      
      # Guides
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
        "No data", 
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



