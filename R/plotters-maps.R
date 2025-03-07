
# basesize <- 7

size <- list(
  text = basesize,
  title = basesize + 2,
  stext = basesize - 1,
  footnote = basesize - 2
)

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

admin0 <- rnaturalearth::ne_countries(scale = 50)

admin1 <- sf::st_read("data-raw/maps/ne_10m_admin_1_states_provinces.shp") |> 
  mutate(adm0_a3 = case_when(
    woe_name %in% c("Bonaire", "Saba", "St. Eustatius") ~ "BES",
    geonunit == "French Guiana" ~ "GUF",
    geonunit == "Guadeloupe" ~ "GLP",
    geonunit == "Martinique" ~ "MTQ",
    adm0_a3 == "KOS" ~ "XKX", 
    adm0_a3 == "SDS" ~ "SSD", 
    adm0_a3 == "PSX" ~ "PSE", 
    adm0_a3 == "SAH" ~ "ESH", 
    .default = adm0_a3
  ))

nmig_rast <- system.file(
  "rasters", "nmig_2016_2020_avg.tif", package = "gdidata"
) |> 
  raster::raster()
names(nmig_rast) <- "v"

popden_rast <- system.file(
  "rasters", "popden_2020.tif", package = "gdidata"
) |> 
  raster::raster()
names(popden_rast) <- "v"

income_rast <- system.file(
  "rasters", "income_2015.tif", package = "gdidata"
) |> 
  raster::raster()
names(income_rast) <- "v"


# Net migration -----------------------------------------------------------

plot_nmigmap <- function(hero,
                         basesize) {
  
  k <- function(factor = 1) factor * basesize / .pt
  
  p_title <- "Map of net migration per capita"
  p_source <- "Source: Niva et al. (2023)."
  
  border_sub <- filter(admin1, adm0_a3 == hero) 
  
  # Special cases
  if (hero == "BRA") {
    border_sub <- filter(admin0, adm0_a3 == hero)
  }
  if (hero == "FRA") {
    border_sub <- filter(border_sub, type_en != "Overseas department")
  }
  border <- border_sub |> sf::st_union()
  
  bbox <- sf::st_bbox(border)
  border_spat <- sf::as_Spatial(border)
  rast <- terra::crop(terra::mask(nmig_rast, border_spat), border_spat)
  df <- terra::as.data.frame(rast, xy = TRUE, na.rm = TRUE)
  # df <- as.data.frame(nmig_rast, xy = TRUE, na.rm = TRUE) |>
  # filter(x >= -38 & x <= -36 & y >= -55, y <= -53.5)
  # filter(x >= -40 & x <= -30 & y <= -15)
  
  # Manually set boundaries for countries that are smaller than 1 pixel
  small <- c("AIA", "MCO", "NRU", "VAT", "VGB", "TUV")
  if (hero == "AIA") {
    bbox_manual <- raster::extent(-63.42882, -62.92568, 18.16909, 18.60126)
    rast <- terra::crop(nmig_rast, bbox_manual)
    df <- terra::as.data.frame(rast, xy = TRUE, na.rm = TRUE)
  }
  if (hero == "CYM") {
    bbox_manual <- raster::extent(-81.41654, -79.72664, 19.3, 19.6)
    rast <- terra::crop(nmig_rast, bbox_manual)
    df <- terra::as.data.frame(rast, xy = TRUE, na.rm = TRUE)
  }
  if (hero == "MCO") {
    bbox_manual <- raster::extent(7.375000, 7.458333, 43.70833, 43.79166)
    rast <- terra::crop(nmig_rast, bbox_manual)
    df <- terra::as.data.frame(rast, xy = TRUE, na.rm = TRUE)
  }
  if (hero == "SGS") {
    bbox_manual <- raster::extent(-38.05379, -36.0565, -54.9584, -53.95915)
    # bbox_manual <- raster::extent(
    #   -38.05379143792758, -38.05379143792758 + 1/12, 
    #   -53.95915511810113, -53.95915511810113 + 1/12
    # )
    rast <- terra::crop(nmig_rast, bbox_manual)
    df <- terra::as.data.frame(rast, xy = TRUE, na.rm = TRUE)
  }
  if (hero == "NRU") {
    bbox_manual <- raster::extent(
      166.9583, 166.9583 + 1/12, 
      -0.5416667, -0.5416667 + 1/12
    )
    rast <- terra::crop(nmig_rast, bbox_manual)
    df <- terra::as.data.frame(rast, xy = TRUE, na.rm = TRUE)
  }
  if (hero == "VAT") {
    bbox_manual <- raster::extent(12.37500, 12.45833, 41.87500, 41.95833)
    rast <- terra::crop(nmig_rast, bbox_manual)
    df <- terra::as.data.frame(rast, xy = TRUE, na.rm = TRUE)
  }
  if (hero == "TUV") {
    bbox_manual <- bbox
    rast <- terra::crop(nmig_rast, bbox_manual)
    df <- terra::as.data.frame(rast, xy = TRUE, na.rm = TRUE)
  }

  aspect <- (bbox$ymax - bbox$ymin) / (bbox$xmax - bbox$xmin)
  
  plot <- ggplot() + 
    geom_sf(data = border, fill = pal("grays", 5), color = NA) +
    geom_sf(
      data = border, 
      fill = NA, color = pal("grays", 2), linewidth = k(.05)
    ) +
    geom_sf(
      data = border_sub, 
      fill = NA, color = pal("grays", 3), linewidth = k(.025)
    ) +
    coord_sf(expand = FALSE) +
    apply_theme(type = "map", basesize = basesize, font = font) + 
    theme(
      legend.key.width = unit(1.25 * size$text, "points"),
      legend.title = element_text(
        size = size$text, 
        hjust = .5,
        margin = margin(l = k(3.5))
      ),
      legend.title.position = "right",
      legend.box.spacing = unit(k(.25), "lines"),
      plot.margin = margin(0, 0, k(2), 0)
    )
  
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
        size = size$text, 
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
  
  if ((aspect > 1 & hero != "USA") | hero %in% small) {
    plot <- plot + 
      theme(
        legend.key.height = unit(size$text, "points"),
        legend.key.width = unit(.75 * size$text, "points"),
        legend.text = element_text(margin = margin(l = k())),
        legend.text.position = "right",
        legend.title = element_text(
          size = size$text, 
          hjust = .5,
          margin = margin(b = k(3.5))
        ),
        legend.title.position = "top",
        legend.position = "right"
      )
  }
  
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
  
  plot_title <- ggplot() + 
    ggtitle(str_glue("{p_title}, average 2016–2020")) +
    apply_theme(type = "map", basesize = basesize, font = font) + 
    theme(plot.margin = margin(0, 0, k(2.5), 0))
  
  plot_caption <- ggplot() + 
    labs(caption = p_source) +
    apply_theme(type = "map", basesize = basesize, font = font) + 
    theme(plot.margin = margin(0, 0, 0, 0))
  
  plot <- plot_grid(
    plot_title, plot, plot_caption, 
    nrow = 3,
    rel_heights = c(.100, 1, .050)
  ) +
    theme(plot.margin = margin(k(2), k(3), k(2), k(3)))
  
  return(plot)
}


# Internal displacements --------------------------------------------------

plot_idpmap <- function(hero,
                        basesize) {
  
  k <- function(factor = 1) factor * size$text / .pt
  
  p_title <- "Map of new internal displacements"
  p_source <- "Source: IDMC."
  if (hero %in% idmc_iom) {
    p_source <- "Source: IDMC; IOM Displacement Tracking Matrix."
  }
  
  border_sub <- filter(admin1, adm0_a3 == hero)
  
  # Special cases
  if (hero == "BRA") {
    border_sub <- filter(admin0, adm0_a3 == hero)
  }
  if (hero == "FRA") {
    border_sub <- filter(border_sub, type_en != "Overseas department")
  }
  border <- border_sub |> sf::st_union()
  
  bbox <- sf::st_bbox(border)
  aspect <- (bbox$ymax - bbox$ymin) / (bbox$xmax - bbox$xmin)
  
  types = c(
    "Conflict and violence", 
    "Environmental impacts"
  )
  
  data <- readr::read_csv("data-raw/Displacements_geolocated.csv") |> 
    select(
      geo = Country, t = Year, type = Type, category = Category,
      n = Displacements, lat = latitude, lon = longitude
    ) |> 
    filter(type != "Other" & t > max(t) - 5)
  
  timespan <- data |> distinct(t) |> pull(t)
  
  df <- data |> 
    filter(
      geo == hero & 
        between(lon, bbox[1], bbox[3]) & 
        between(lat, bbox[2], bbox[4])
    ) |> 
    mutate(type = case_when(
      type == "Conflict" ~ types[1],
      type == "Disaster" ~ types[2]
    )) |> 
    summarise(n = sum(n), .by = c(t, type, lon, lat))
  
  plot <- ggplot() + 
    geom_sf(
      data = border, 
      fill = pal("grays", 5), color = pal("grays", 4), linewidth = k(.05)
    ) +
    geom_sf(
      data = border_sub, 
      fill = NA, color = pal("grays", 4), linewidth = k(.025)
    ) + 
    coord_sf(
      xlim = bbox[c(1, 3)], 
      ylim = bbox[c(2, 4)], 
      expand = FALSE, 
      clip = "off"
    ) + 
    apply_theme(type = "map", basesize = basesize, font = font) + 
    guides(
      shape = guide_legend(
        direction = "vertical", 
        override.aes = list(size = 1.75, shape = c(1, 5)),
        order = 1
      ),
      fill = guide_legend(
        direction = "vertical", 
        override.aes = list(size = 2, shape = 22, color = NA), 
        theme = theme(legend.text = element_text(
          margin = margin(l = -k())
        )),
        order = 2
      ),
      size = guide_legend(
        direction = "vertical", 
        override.aes = list(shape = 1), 
        order = 3
      )
    ) +
    theme(
      legend.spacing.x = unit(.25 * size$text, "points"),
      legend.key.spacing.y = unit(.2 * size$text, "points"),
      legend.text = element_text(margin = margin(t = 0)),
      legend.title = element_text(size = size$text),
      plot.margin = margin(0, 0, 0, 0)
    )
  
  if (nrow(df) > 0) {
    
    df <- complete(df, t = timespan, type = types, n = min(df$n))
    
    set_labels <- prettylabel
    if (length(unique(df$n)) == 1) {
      set_breaks <- df$n[1]
    }
    if (length(unique(df$n)) == 2) {
      set_breaks <- c(min(df$n), max(df$n))
    }
    if (length(unique(df$n)) >= 3) {
      set_breaks <- c(
        min(df$n), 
        (max(df$n) + min(df$n)) / 4 |> round(), 
        max(df$n)
      )
      set_labels <- c(
        min(df$n), 
        (max(df$n) + min(df$n)) / 4 |> round(), 
        max(df$n)
      ) |> prettylabel()
    }
    
    plot <- plot + 
      geom_point(
        mapping = aes(
          x = lon, y = lat, 
          shape = forcats::fct_relevel(type, types[1], after = 1), 
          fill = factor(t) |> forcats::fct_rev(), 
          size = n
        ),
        data = df,
        color = pal("blues"), stroke = k(.05)
      ) +
      scale_shape_manual(
        name = "Type", 
        labels = rev(types),
        values = c(21, 23)
      ) +
      scale_fill_manual(
        name = "Year", 
        values = pal("blues", 1:5),
      ) +
      scale_size_continuous(
        name = "Displacements", 
        limits = c(min(df$n), max(df$n)),
        breaks = set_breaks,
        labels = set_labels
      )
    title <- str_glue("{ p_title },\n{ min(df$t) }\u2013{ max(df$t) }")
    
    if (aspect > 1 & hero != "USA") {
      plot <- plot + 
        guides(fill = guide_legend(
          direction = "vertical", 
          nrow = 3,
          override.aes = list(size = 2, shape = 22, color = NA), 
          order = 2
        )) +
        theme(
          legend.spacing.y = unit(.5 * size$text, "points"),
          legend.title = element_text(
            size = size$text, 
            margin = margin(b = k(2))
          ),
          legend.position = "right"
        )
    }
    
  } else {
    
    plot <- ggdraw(plot) + 
      draw_label(
        "No data", 
        fontfamily = "Gill Sans Nova", color = pal("blues", 3), size = k(3)
      )
    title <- p_title
  }
  
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
      cowplot::draw_plot(ask, x = 0, y = .3, scale = .25, halign = 0, valign = 0) +
      cowplot::draw_plot(hwi, x = .2, y = .3, scale = .2, halign = 0, valign = 0)
  }
  
  plot_title <- ggplot() + ggtitle(title) +
    apply_theme(type = "map", basesize = basesize, font = font) + 
    theme(plot.margin = margin(0, 0, 0, 0))
  
  plot_caption <- ggplot() + labs(caption = p_source) +
    apply_theme(type = "map", basesize = basesize, font = font) + 
    theme(plot.margin = margin(0, 0, 0, 0))
  
  plot <- plot_grid(
    plot_title, plot, plot_caption, 
    nrow = 3, 
    rel_heights = c(.1, 1, .05)
  ) +
    theme(plot.margin = margin(k(2), k(3), k(2), k(3)))
  
  return(plot)
}


# Missing migrants --------------------------------------------------------

plot_mmpmap <- function(hero,
                        basesize) {
  
  k <- function(factor = 1) factor * basesize / .pt
  
  p_title <- "Map of dead or missing international migrants"
  p_source <- "Source: IOM Missing Migrants Project."
  
  causes <- tibble::tribble(
    ~cause,                 ~fill,
    "Drowning",             pal("blues", 2),
    "Transport hazards",    pal("blues", 4),
    "Harsh conditions",     pal("greens"),
    "Accidental",           pal("greens", 3),
    "Sickness",             pal("yellows"),
    "Violence",             pal("reds", 2),
    "Mixed or unknown",     pal("grays", 3)
  )
  
  border_sub <- filter(admin1, adm0_a3 == hero)
  
  # Special cases
  if (hero == "BRA") {
    border_sub <- filter(admin0, adm0_a3 == hero)
  }
  if (hero == "FRA") {
    border_sub <- filter(border_sub, type_en != "Overseas department")
  }
  border <- border_sub |> sf::st_union()
  
  bbox <- sf::st_bbox(border)
  aspect <- (bbox$ymax - bbox$ymin) / (bbox$xmax - bbox$xmin)
  
  t0 <- min(gdidata::iom_mmp$t, na.rm = TRUE) |> year()
  t1 <- max(gdidata::iom_mmp$t, na.rm = TRUE) |> year()
  
  df <- gdidata::iom_mmp |> 
    filter(
      geo == hero & 
        between(lon, bbox[1], bbox[3]) & 
        between(lat, bbox[2], bbox[4])
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
  
  plot <- ggplot() + 
    geom_sf(
      data = border, 
      fill = pal("grays", 5), color = pal("grays", 4), linewidth = k(.05)
    ) +
    geom_sf(
      data = border_sub, 
      fill = NA, color = pal("grays", 4), linewidth = k(.025)
    ) + 
    coord_sf(
      xlim = bbox[c(1, 3)], 
      ylim = bbox[c(2, 4)], 
      expand = FALSE, 
      clip = "off"
    ) + 
    apply_theme(type = "map", basesize = basesize, font = font) + 
    guides(
      fill = guide_legend(
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
    ) +
    theme(plot.margin = margin(0, 0, 0, 0))
  
  if (nrow(df) > 0) {
    
    df <- add_row(df, cause = causes$cause, n = df$n[1])
    
    plot <- plot +
      geom_point(
        mapping = aes(
          x = lon, y = lat, 
          fill = factor(cause, levels = causes$cause), 
          size = n
        ),
        data = df,
        shape = 21, stroke = k(.05)
      ) +
      scale_fill_manual(name = "Cause", values = causes$fill) +
      scale_size_continuous(
        name = "Persons",
        breaks = c(min(df$n), round((max(df$n) + min(df$n)) / 4), max(df$n)),
      ) + 
      theme(
        legend.spacing.x = unit(.25 * size$text, "points"),
        # legend.key.spacing.y = unit(.2 * size$text, "points"),
        legend.text = element_text(
          size = size$stext,
          margin = margin(r = k(1), l = k(.25))
        ),
        legend.title = element_text(size = size$text),
        legend.title.position = "top",
      )
    
    if (aspect > 1 & hero != "USA") {
      plot <- plot +
        guides(fill = guide_legend(
          direction = "vertical",
          override.aes = list(size = 2.25, shape = 22, color = NA), 
          theme = theme(legend.text = element_text(margin = margin(l = -k()))),
          order = 1
        )) +
        theme(
          # legend.spacing.y = unit(.5 * size$text, "points"),
          legend.title = element_text(
            size = size$text, 
            margin = margin(b = k(2))
          ),
          legend.position = "right"
        )
    }
    
    title <- str_glue("{p_title},\n{t0}–{t1}")
    
  } else {
    
    plot <- ggdraw(plot) + 
      draw_label(
        "None recorded", 
        fontfamily = "Gill Sans Nova", color = pal("blues", 3), size = k(3)
      )
    title <- p_title
  }
  
  if (hero == "USA") {
    
    border_usa <- admin1 |> 
      filter(adm0_a3 == "USA" & !(name %in% c("Alaska", "Hawaii"))) |> 
      sf::st_union()
    border_sub_usa <- admin1 |> 
      filter(adm0_a3 == "USA" & !(name %in% c("Alaska", "Hawaii")))
    
    df_alaska <- filter(df, between(lon, -172, -129) & between(lat, 53, 72))
    df_hawaii <- filter(df, between(lon, -160, -154) & between(lat, 18.5, 22.3))
    
    mainland <- ggplot() + 
      geom_sf(
        data = border_usa, 
        fill = pal("grays", 5), color = pal("grays", 4), linewidth = k(.05)
      ) +
      geom_sf(
        data = border_sub_usa, 
        fill = NA, color = pal("grays", 4), linewidth = k(.025)
      ) + 
      geom_point(
        mapping = aes(
          x = lon, y = lat, 
          fill = factor(cause, levels = causes$cause), size = n
        ),
        data = df, shape = 21, stroke = k(.05)
      ) +
      scale_fill_manual(name = "Cause", values = causes$fill) +
      scale_size_continuous(
        name = "Persons",
        breaks = c(min(df$n), round((max(df$n) + min(df$n)) / 4), max(df$n)),
      ) + 
      coord_sf(
        xlim = c(-125, -66), 
        ylim = c(23, 50), 
        expand = FALSE, 
        clip = "off"
      ) + 
      apply_theme(type = "map", basesize = basesize, font = font) + 
      guides(
        fill = guide_legend(
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
      ) +
      theme(
        legend.spacing.x = unit(.25 * size$text, "points"),
        # legend.key.spacing.y = unit(.2 * size$text, "points"),
        legend.text = element_text(
          size = size$stext,
          margin = margin(r = k(1), l = k(.25))
        ),
        legend.title = element_text(size = size$text),
        legend.title.position = "top",
        plot.margin = margin(0, 0, 0, 0)
      )
    
    alaska <- ggplot() + 
      geom_sf(
        data = filter(border_sub, name == "Alaska"), 
        fill = pal("grays", 5), , color = pal("grays", 4), linewidth = k(.05)
      ) + 
      geom_point(
        mapping = aes(
          x = lon, y = lat, 
          fill = factor(cause, levels = causes$cause), size = n
        ),
        data = df_alaska, shape = 21, stroke = k(.05)
      ) +
      scale_fill_manual(values = causes$fill) +
      coord_sf(
        xlim = c(-172, -129), 
        ylim = c(53, 72), 
        expand = FALSE,
        clip = "off"
      ) + 
      apply_theme(type = "map", basesize = basesize, font = font) + 
      theme(plot.margin = margin(0, 0, 0, 0))
    
    hawaii <- ggplot() + 
      geom_sf(
        data = filter(border_sub, name == "Hawaii"), 
        fill = pal("grays", 5), , color = pal("grays", 4), linewidth = k(.05)
      ) + 
      geom_point(
        mapping = aes(
          x = lon, y = lat, 
          fill = factor(cause, levels = causes$cause), size = n
        ),
        data = df_alaska, shape = 21, stroke = k(.05)
      ) +
      scale_fill_manual(values = causes$fill) +
      coord_sf(
        xlim = c(-160, -154), 
        ylim = c(18.5, 22.3), 
        expand = FALSE,
        clip = "off"
      ) + 
      apply_theme(type = "map", basesize = basesize, font = font) + 
      theme(plot.margin = margin(0, 0, 0, 0))
    
    plot <- ggdraw(mainland) +
      cowplot::draw_plot(alaska, x = 0, y = .3, scale = .25, halign = 0, valign = 0) +
      cowplot::draw_plot(hawaii, x = .2, y = .3, scale = .2, halign = 0, valign = 0)
  }
  
  plot_title <- ggplot() + ggtitle(title) +
    apply_theme(type = "map", basesize = basesize, font = font) + 
    theme(plot.margin = margin(0, 0, 0, 0))
  
  plot_caption <- ggplot() + labs(caption = p_source) +
    apply_theme(type = "map", basesize = basesize, font = font) + 
    theme(plot.margin = margin(0, 0, 0, 0))
  
  plot <- plot_grid(
    plot_title, plot, plot_caption, 
    nrow = 3, rel_heights = c(.125, 1, .05)
  ) +
    theme(plot.margin = margin(k(2), k(3), k(2), k(3)))
  
  return(plot)
}


# Population --------------------------------------------------------------

# plot_popmap <- function(hero) {
#   
#   p_title <- "Map of population density"
#   p_source <- "Source: WorldPop."
#   
#   border_sub <- filter(admin1, adm0_a3 == hero)
#   
#   # Special cases
#   if (hero == "BRA") {
#     border_sub <- filter(admin0, adm0_a3 == hero)
#   }
#   if (hero == "FRA") {
#     border_sub <- filter(border_sub, type_en != "Overseas department")
#   }
#   border <- border_sub |> sf::st_union()
#   
#   bbox <- sf::st_bbox(border)
#   border_spat <- as_Spatial(border)
#   rast <- crop(mask(popden_rast, border_spat), border_spat)
#   df <- as.data.frame(rast, xy = TRUE, na.rm = TRUE) |> 
#     filter(v >= 1)
#   
#   # Manually set boundaries for countries that are smaller than 1 pixel
#   small <- c("AIA", "MCO", "NRU", "VAT", "VGB", "TUV")
#   if (hero == "AIA") {
#     bbox_manual <- raster::extent(-63.42882, -62.92568, 18.16909, 18.60126)
#     rast <- crop(nmig_rast, bbox_manual)
#     df <- as.data.frame(rast, xy = TRUE, na.rm = TRUE)
#   }
#   if (hero == "MCO") {
#     bbox_manual <- raster::extent(7.375000, 7.458333, 43.70833, 43.79166)
#     rast <- crop(popden_rast, bbox_manual)
#     df <- as.data.frame(rast, xy = TRUE, na.rm = TRUE)
#   }
#   if (hero == "NRU") {
#     bbox_manual <- raster::extent(
#       166.9583, 166.9583 + 1/12, 
#       -0.5416667, -0.5416667 + 1/12
#     )
#     rast <- crop(popden_rast, bbox_manual)
#     df <- as.data.frame(rast, xy = TRUE, na.rm = TRUE)
#   }
#   if (hero == "VAT") {
#     bbox_manual <- raster::extent(12.37500, 12.45833, 41.87500, 41.95833)
#     rast <- crop(popden_rast, bbox_manual)
#     df <- as.data.frame(rast, xy = TRUE, na.rm = TRUE)
#   }
#   if (hero == "TUV") {
#     bbox_manual <- bbox
#     rast <- crop(popden_rast, bbox_manual)
#     df <- as.data.frame(rast, xy = TRUE, na.rm = TRUE)
#   }
#   
#   aspect <- (bbox$ymax - bbox$ymin) / (bbox$xmax - bbox$xmin)
#   
#   plot <- ggplot() +
#     geom_sf(data = border, fill = pal("grays", 5), color = NA) +
#     geom_sf(
#       data = border,
#       fill = NA, color = pal("grays", 2), linewidth = k(.05)
#     ) +
#     geom_sf(
#       data = border_sub,
#       fill = NA, color = pal("grays", 3), linewidth = k(.025)
#     ) +
#     coord_sf(xlim = bbox[c(1, 3)], ylim = bbox[c(2, 4)], expand = FALSE) +
#     apply_theme(type = "map", basesize = basesize, font = font) +
#     theme(
#       legend.key.width = unit(1.5 * size$text, "points"),
#       legend.title = element_text(
#         size = size$text,
#         hjust = .5,
#         margin = margin(l = k(6))
#       ),
#       legend.title.position = "right",
#       legend.box.spacing = unit(k(.25), "lines"),
#       plot.margin = margin(0, 0, k(2), 0)
#     )
#   
#   if (length(unique(df$v)) == 1) {
#     
#     v <- df$v[1]
#     set_labels <- format(round(v, digits = -2), trim = TRUE, big.mark = ",")
#     
#     plot <- plot +
#       geom_tile(mapping = aes(x = x, y = y, fill = factor(v)), data = df) +
#       scale_fill_manual(
#         name = "Persons per\nsquare km",
#         labels = set_labels,
#         values = pal("greens")
#       ) + 
#       theme(legend.text = element_text(
#         size = size$text, 
#         hjust = .5,
#         margin = margin(l = k())
#       ))
#   
#   } else {
#     
#     set_limits <- c(min(df$v), quantile(df$v, prob = .95))
#     set_mid <- c(min(df$v), quantile(df$v, prob = .95)) |> mean()
#     set_labels <- function(x) {
#       case_when(
#         abs(x - min(df$v)) < .001 ~ labeler(x),
#         abs(x - set_mid) < .001 ~ labeler(x),
#         abs(x - quantile(df$v, prob = .95)) < .001 ~ labeler(x),
#         .default = ""
#       )
#     }
#     
#     plot <- plot + 
#       geom_tile(mapping = aes(x = x, y = y, fill = v), data = df) +
#       scale_fill_steps2(
#         name = "Persons per\nsquare km",
#         labels = set_labels,
#         n.breaks = 5,
#         nice.breaks = FALSE,
#         limits = set_limits,
#         show.limits = TRUE,
#         low = pal("yellows", 3),
#         mid = pal("greens"),
#         high = pal("blues", 2),
#         midpoint = set_mid
#       )
#   }
#   
#   if ((aspect > 1 & hero != "USA") | hero %in% small) {
#     plot <- plot + 
#       theme(
#         legend.key.height = unit(size$text, "points"),
#         legend.key.width = unit(.75 * size$text, "points"),
#         legend.text = element_text(margin = margin(l = k())),
#         legend.text.position = "right",
#         legend.title = element_text(
#           size = size$text, 
#           hjust = .5,
#           margin = margin(b = k(3.5))
#         ),
#         legend.title.position = "top",
#         legend.position = "right"
#       )
#   }
#   
#   if (hero == "USA") {
#     
#     ask <- plot + 
#       coord_sf(xlim = c(-172, -129), ylim = c(53, 72), expand = FALSE) + 
#       theme(legend.position = "none", plot.margin = margin(0, 0, 0, 0))
#     hwi <- plot + 
#       coord_sf(xlim = c(-160, -154), ylim = c(18.5, 22.3), expand = FALSE) + 
#       theme(legend.position = "none", plot.margin = margin(0, 0, 0, 0))
#     
#     plot <- ggdraw(
#       plot + coord_sf(xlim = c(-125, -66), ylim = c(23, 50), expand = FALSE)
#     ) +
#       cowplot::draw_plot(ask, x = 0, y = .1, scale = .35, halign = 0, valign = 0) +
#       cowplot::draw_plot(hwi, x = .25, y = .15, scale = .2, halign = 0, valign = 0)
#   }
#   
#   plot_title <- ggplot() + 
#     ggtitle(str_glue("{p_title}, 2020")) +
#     apply_theme(type = "map", basesize = basesize, font = font) + 
#     theme(plot.margin = margin(0, 0, k(2.5), 0))
#   
#   plot_caption <- ggplot() + 
#     labs(caption = p_source) +
#     apply_theme(type = "map", basesize = basesize, font = font) + 
#     theme(plot.margin = margin(0, 0, 0, 0))
#   
#   plot <- plot_grid(
#     plot_title, plot, plot_caption, 
#     nrow = 3, rel_heights = c(.100, 1, .050)
#   ) +
#     theme(plot.margin = margin(k(5), k(3), k(2), k(3)))
#   
#   return(plot)
# }
# 
# 
# # Income ------------------------------------------------------------------
# 
# plot_incmap <- function(hero) {
#   
#   p_title <- "Map of GNI per capita"
#   p_source <- "Source: Kummu et al. (2018)."
#   
#   border_sub <- filter(admin1, adm0_a3 == hero)
#   
#   # Special cases
#   if (hero == "BRA") {
#     border_sub <- filter(admin0, adm0_a3 == hero)
#   }
#   if (hero == "FRA") {
#     border_sub <- filter(border_sub, type_en != "Overseas department")
#   }
#   border <- border_sub |> sf::st_union()
#   
#   bbox <- sf::st_bbox(border)
#   boundary_spat <- as_Spatial(border_sub)
#   rast <- crop(mask(income_rast, boundary_spat), boundary_spat)
#   df <- as.data.frame(rast, xy = TRUE, na.rm = TRUE)
#   threshold <- quantile(df$v, prob = .95)
#   # df <- filter(df, v >= threshold)
#   
#   # Manually set boundaries for countries that are smaller than 1 pixel
#   small <- c("AIA", "MCO", "NRU", "VAT", "VGB", "TUV")
#   if (hero == "AIA") {
#     bbox_manual <- raster::extent(-63.42882, -62.92568, 18.16909, 18.60126)
#     rast <- crop(nmig_rast, bbox_manual)
#     df <- as.data.frame(rast, xy = TRUE, na.rm = TRUE)
#   }
#   if (hero == "MCO") {
#     bbox_manual <- raster::extent(7.375000, 7.458333, 43.70833, 43.79166)
#     rast <- crop(income_rast, bbox_manual)
#     df <- as.data.frame(rast, xy = TRUE, na.rm = TRUE)
#   }
#   if (hero == "NRU") {
#     bbox_manual <- raster::extent(
#       166.9583, 166.9583 + 1/12, 
#       -0.5416667, -0.5416667 + 1/12
#     )
#     rast <- crop(income_rast, bbox_manual)
#     df <- as.data.frame(rast, xy = TRUE, na.rm = TRUE)
#   }
#   if (hero == "VAT") {
#     bbox_manual <- raster::extent(12.37500, 12.45833, 41.87500, 41.95833)
#     rast <- crop(income_rast, bbox_manual)
#     df <- as.data.frame(rast, xy = TRUE, na.rm = TRUE)
#   }
#   if (hero == "TUV") {
#     bbox_manual <- bbox
#     rast <- crop(income_rast, bbox_manual)
#     df <- as.data.frame(rast, xy = TRUE, na.rm = TRUE)
#   }
#   
#   aspect <- (bbox$ymax - bbox$ymin) / (bbox$xmax - bbox$xmin)
#   
#   plot <- ggplot() + 
#     geom_sf(data = border, fill = pal("grays", 5), color = NA) +
#     geom_sf(
#       data = border, 
#       fill = NA, color = pal("grays", 2), linewidth = k(.05)
#     ) +
#     geom_sf(
#       data = border_sub, 
#       fill = NA, color = pal("grays", 3), linewidth = k(.025)
#     ) +
#     coord_sf(xlim = bbox[c(1, 3)], ylim = bbox[c(2, 4)], expand = FALSE) + 
#     apply_theme(type = "map", basesize = basesize, font = font) + 
#     theme(
#       legend.key.width = unit(1.5 * size$text, "points"),
#       legend.title = element_text(
#         size = size$text, 
#         hjust = .5,
#         margin = margin(l = k(6))
#       ),
#       legend.title.position = "right",
#       legend.box.spacing = unit(k(.25), "lines"),
#       plot.margin = margin(0, 0, k(2), 0)
#     )
#   
#   if (length(unique(filter(df, v <= threshold)$v)) == 1) {
#     
#     df <- filter(df, v <= threshold)
#     v <- df$v[1]
#     set_labels <- format(round(v, digits = -2), trim = TRUE, big.mark = ",")
#     
#     plot <- plot +
#       geom_tile(mapping = aes(x = x, y = y, fill = factor(v)), data = df) + 
#       scale_fill_manual(
#         name = "Constant\n2017 USD",
#         labels = set_labels,
#         values = pal("greens")
#       ) + 
#       theme(legend.text = element_text(
#         size = size$text, 
#         hjust = .5,
#         margin = margin(l = k())
#       ))
#     
#   } else {
#     
#     # set_limits <- c(min(df$v), quantile(df$v, prob = .95))
#     set_mid <- c(min(df$v), quantile(df$v, prob = .95)) |> mean()
#     set_labels <- function(x) {
#       case_when(
#         abs(x - min(df$v)) < .001 ~ labeler(x),
#         abs(x - set_mid) < .001 ~ labeler(x),
#         abs(x - quantile(df$v, prob = .95)) < .001 ~ labeler(x),
#         .default = ""
#       )
#     }
#     
#     plot <- plot + 
#       geom_tile(mapping = aes(x = x, y = y, fill = v), data = df) + 
#       scale_fill_steps2(
#         name = "Constant\n2017 USD",
#         labels = set_labels,
#         n.breaks = 5,
#         nice.breaks = FALSE,
#         # limits = set_limits,
#         show.limits = TRUE,
#         low = pal("yellows", 3), 
#         mid = pal("greens"), 
#         high = pal("blues", 2),
#         midpoint = set_mid
#       )
#   }
#   
#   if ((aspect > 1 & hero != "USA") | hero %in% small) {
#     plot <- plot + 
#       theme(
#         legend.key.height = unit(size$text, "points"),
#         legend.key.width = unit(.75 * size$text, "points"),
#         legend.text = element_text(margin = margin(l = k())),
#         legend.text.position = "right",
#         legend.title = element_text(
#           size = size$text, 
#           hjust = .5,
#           margin = margin(b = k(3.5))
#         ),
#         legend.title.position = "top",
#         legend.position = "right"
#       )
#   }
#   
#   if (hero == "USA") {
#     
#     ask <- plot + 
#       coord_sf(xlim = c(-172, -129), ylim = c(53, 72), expand = FALSE) + 
#       theme(legend.position = "none", plot.margin = margin(0, 0, 0, 0))
#     hwi <- plot + 
#       coord_sf(xlim = c(-160, -154), ylim = c(18.5, 22.3), expand = FALSE) + 
#       theme(legend.position = "none", plot.margin = margin(0, 0, 0, 0))
#     
#     plot <- ggdraw(
#       plot + coord_sf(xlim = c(-125, -66), ylim = c(23, 50), expand = FALSE)
#     ) +
#       cowplot::draw_plot(ask, x = 0, y = .1, scale = .35, halign = 0, valign = 0) +
#       cowplot::draw_plot(hwi, x = .25, y = .15, scale = .2, halign = 0, valign = 0)
#   }
#   
#   plot_title <- ggplot() + 
#     ggtitle(str_glue("{p_title}, 2015")) +
#     apply_theme(type = "map", basesize = basesize, font = font) + 
#     theme(plot.margin = margin(0, 0, k(2.5), 0))
#   
#   plot_caption <- ggplot() + 
#     labs(caption = p_source) +
#     apply_theme(type = "map", basesize = basesize, font = font) + 
#     theme(plot.margin = margin(0, 0, 0, 0))
#   
#   plot <- plot_grid(
#     plot_title, plot, plot_caption, 
#     nrow = 3, rel_heights = c(.100, 1, .050)
#   ) +
#     theme(plot.margin = margin(k(5), k(3), k(2), k(3)))
#   
#   return(plot)
# }

