
# Parameters --------------------------------------------------------------

basesize <- 7
font <- "Gill Sans Nova"
label_size <- 1
legend_spacing <- .05
legend_nrow <- 3

interval <- "years"
end <- NULL
years <- 7


# Chart helpers -----------------------------------------------------------

idmc_iom <- c(
  "AFG",
  "BEN",
  "BDI",
  "CMR",
  "CAF",
  "TCD",
  "COD",
  "ETH",
  "HTI",
  "IRQ",
  "LBY",
  "MLI",
  "MOZ",
  "NER",
  "NGA",
  "PNG",
  "SOM",
  "SSD",
  "SDN",
  "SYR",
  "UKR",
  "MDG" ,      
  "MWI",
  "ZWE",
  "ZMB"
)

gva_sectors <- c(
  "Agriculture, hunting, forestry, fishing",
  "Mining, Manufacturing, Utilities",
  "Construction",
  "Manufacturing",
  "Wholesale, retail trade, restaurants and hotels",
  "Transport, storage and communication",
  "Other Activities"
)

exports_sectors <- c(
  "Agriculture, Hunting, Forestry and Fishing",
  "Mining and Quarrying",
  "Manufacturing",
  "Electricity, Gas and Water",
  "Community, Social and Personal Services"
)

namer <- function(iso, bold = FALSE, lang = "en") {
  
  if (is.null(iso)) {
    
    return(NULL)
    
  } else {
    
    name_iso <- filter(gdidata::countrynames, iso3 == iso)
    
    if (iso == "XKX") {
      name_iso$with_the <- 0
      name_iso$name_text <- "Kosovo"
    }
    
    if (lang == "en") {
      
      if (bold) {
        
        if (name_iso$with_the == 1) {
          name <- paste0("the ", "#b[", name_iso$name_text, "]")
        }
        else name <- paste0("#b[", name_iso$name_text, "]")
        
      } else {
        
        if (name_iso$with_the == 1) name <- paste0("the ", name_iso$name_text)
        else name <- name_iso$name_text
      }
    }
    
    if (lang == "pt") {
      if (bold) name <- paste0("#b[", name_iso$name_pt, "]")
      else name <- name_iso$name_pt
    }
  }
  
  return(name)
}

break_lines <- function(column) {

  dict <- c(
    "Plurinational State of Bolivia" =
      "Plurinational State\nof Bolivia",
    "Syrian Arab Republic" =
      "Syrian Arab\nRepublic",
    "Russian Federation" =
      "Russian\nFederation",
    "China, Taiwan Province of China" =
      "Taiwan Province\nof China",
    "Democratic People's Republic of Korea" =
      "Democratic People's Republic\nof Korea",
    "Democratic Republic of the Congo" =
      "Democratic Republic\nof the Congo",
    "Lao People's Democratic Republic" =
      "Lao People's Democratic\nRepublic",
    "Federated States of Micronesia" =
      "Federated States\nof Micronesia",
    "Occupied Palestinian Territory" =
      "Occupied Palestinian\nTerritory",
    "Bolivarian Republic of Venezuela" =
      "Bolivarian Republic\nof Venezuela",
    
    "Wholesale and retail trade" = 
      "Wholesale and\nretail trade",
    "Accommodation and food services" = 
      "Accommodation\nand food services",
    "Information and communication" = 
      "Information and\ncommunication",
    "Professional activities" = 
      "Professional\nactivities",
    "Administrative activities" = 
      "Administrative\nactivities",
    "Public administration" = 
      "Public\nadministration",
    "Arts and recreation" = 
      "Arts and\nrecreation",
    "Miscellaneous sectors" = 
      "Miscellaneous\nsectors",
    
    
    "Agriculture, hunting, forestry, fishing" = 
      "Agriculture,\nhunting, forestry,\nfishing",
    "Mining, Manufacturing, Utilities" = 
      "Mining,\nManufacturing,\nUtilities",
    "Wholesale, retail trade, restaurants and hotels" =
      "Wholesale, retail\ntrade, restaurants\nand hotels",
    "Transport, storage and communication" = 
      "Transport,\nstorage and\ncommunication",
    
    "Agriculture, Hunting, Forestry and Fishing" =
      "Agriculture,\nHunting, Forestry\nand Fishing",
    "Mining and Quarrying" =
      "Mining and\nQuarrying",
    "Electricity, Gas and Water" =
      "Electricity, Gas\nand Water",
    "Community, Social and Personal Services" =
      "Community, Social\nand Personal\nServices"
  )

  indices <- which(column %in% names(dict))

  new_col <- replace(
    column,
    indices,
    dict[column[indices]]
  )

  return(new_col)
}

set_axis <- function(values, units = "Person", lang = "en") {

  max_n <- max(values, na.rm = TRUE)

  if (lang == "en") {
    preposition <- "of"
    thousands <- "Thousands"
    millions <- "Millions"
    billions <- "Billions"
    trillions <- "Trillions"
  }
  
  if (lang == "pt") {
    preposition <- "de"
    thousands <- "Milhares"
    millions <- "Milhões"
    billions <- "Bilhões"
    trillions <- "Trilhões"
  }
  
  write_title <- function(scale, units) {
    if (units == "US$") text <- paste0(scale, " ", units)
    else text <- paste(scale, preposition, tolower(units))
    return(text)
  }
  
  output <- list(
    breaks = waiver(),
    labels = function(x) x / 10^6,
    title = write_title(millions, units)
  )

  if (max_n < 12) {
    output$title <- units
    output$breaks <- c(0, 5, 10)
    output$labels <- waiver()
  }
  if (max_n >= 12 & max_n < 1200) {
    output$title <- units
    output$labels <- waiver()
  }
  if (max_n >= 1200 & max_n < 1.20 * 10^6) {
    output$title <- write_title(thousands, units)
    output$labels <- function(x) x / 1000
  }
  if (max_n >= 1.20 * 10^6 & max_n < 1.40 * 10^6) {
    output$breaks <- seq(0, 1.25 * 10^6, .25 * 10^6)
    output$labels <- c("0", "0.25", "0.50", "0.75", "1.00", "1.25")
  }
  if (max_n >= 1.40 * 10^6 & max_n < 1.80 * 10^6) {
    output$breaks <- seq(0, 1.50 * 10^6, .50 * 10^6)
    output$labels <- c("0", "0.5", "1.0", "1.5")
  }

  if (max_n >= 1.20 * 10^9 & max_n < 1.40 * 10^9) {
    output$title <- write_title(billions, units)
    output$breaks <- seq(0, 1.50 * 10^9, .25 * 10^9)
    output$labels <- c("0", "0.25", "0.50", "0.75", "1.00", "1.25", "1.50")
  }
  if (max_n >= 1.40 * 10^9 & max_n < 1.80 * 10^9) {
    output$title <- write_title(billions, units)
    output$breaks <- seq(0, 1.50 * 10^9, .50 * 10^9)
    output$labels <- c("0", "0.5", "1.0", "1.5")
  }
  if (max_n >= 1.80 * 10^9) {
    output$title <- write_title(billions, units)
    output$labels <- function(x) x / 10^9
  }

  return(output)
}

get_legend <- function(plot, legend = NULL) {
  
  gt <- ggplotGrob(plot)
  
  pattern <- "guide-box"
  if (!is.null(legend)) {
    pattern <- paste0(pattern, "-", legend)
  }
  
  indices <- grep(pattern, gt$layout$name)
  
  not_empty <- !vapply(
    gt$grobs[indices], 
    inherits, what = "zeroGrob", 
    FUN.VALUE = logical(1)
  )
  indices <- indices[not_empty]
  
  if (length(indices) > 0) {
    return(gt$grobs[[indices[1]]])
  }
  return(NULL)
}

plot_label <- function(plot, label, span = 2, h = .06) {

  box <- grid::rectGrob(gp = grid::gpar(fill = pal("unblues", 3), col = NA))

  width <- .025 + .015 * nchar(label)
  height <- h
  if (span == 1) width <- width / 2
  if (span == 3) width <- width * 1.5

  cowplot::ggdraw(plot) +
    cowplot::draw_grob(
      box,
      x = 0,
      y = 1,
      vjust = 1,
      width = width,
      height = height
    ) +
    cowplot::draw_label(
      label,
      x = width / 2,
      y = 1 - height / 2,
      hjust = .5,
      vjust = .5,
      size = basesize,
      fontface = "bold",
      fontfamily = font,
      color = "white"
    )
}

get_dims <- function(ticks, inwidth = 0, outwidth = .1) {
  
  biggap <- outwidth / (ticks - 1)
  barwidthfull <- .9 / (ticks * 2)
  barwidth <- barwidthfull * (1 - inwidth)
  smallgap <- barwidthfull - barwidth
  
  xmin <- c()
  for (i in 1:(ticks * 2)) {
    xmin <- c(
      xmin, 
      biggap * ((i - 1) %/% 2) + barwidth * (i - 1) + smallgap * (i %/% 2)
    )
  }
  
  table <- tibble(xmin = xmin) |> 
    mutate(
      xmax = xmin + barwidth,
      mid1 = (xmin + xmax) / 2,
      mid = (mid1 + lag(mid1)) / 2,
      id = 1:n(),
      xtick = ifelse(id %% 2 == 0, TRUE, FALSE)
    ) |> 
    select(-mid1, -id)
  
  return(table)
}


# Map helpers -------------------------------------------------------------

admin0 <- rnaturalearth::ne_countries(scale = 50)

admin1 <- sf::st_read(
  "data-raw/maps/ne_10m_admin_1_states_provinces.shp",
  quiet = TRUE
) |> 
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
  )) |> 
  
  # Remove French overseas departments Réunion and Mayotte
  filter(!(adm0_a3 == "FRA" & type_en == "Overseas department")) |> 
  
  sf::st_make_valid()

# plot_map <- function(data, data_sub, borders = TRUE, fill = TRUE) {
#   
#   if (borders) stroke <- pal("grays", 2)
#   
#   ggplot() + 
#     geom_sf(data = data, fill = pal("grays", 5), color = NA) +
#     geom_sf(
#       data = border, 
#       fill = NA, color = pal("grays", 2), linewidth = k(.05)
#     ) +
#     geom_sf(
#       data = data_sub, 
#       fill = NA, color = pal("grays", 3), linewidth = k(.025)
#     ) +
#     apply_theme("map", basesize = basesize, font = font) + 
#     theme(
#       legend.key.width = unit(1.25 * size$text, "points"),
#       legend.title = element_text(
#         size = size$text, 
#         hjust = .5,
#         margin = margin(l = k(3.5))
#       ),
#       legend.title.position = "right",
#       legend.box.spacing = unit(k(.25), "lines"),
#       plot.margin = margin(0, 0, k(2), 0)
#     )
# }

nmig_rast <- system.file(
  "rasters", "nmig_2016_2020_avg.tif", package = "gdidata"
) |> 
  raster::raster()
names(nmig_rast) <- "v"

map_data <- function(hero) {
  
  border_adm1 <- filter(admin1, adm0_a3 == hero)
  
  if (hero == "RUS") {
    border_adm1 <- border_adm1 |> 
      sf::st_crop(c(xmin = 19, ymin = 0, xmax = 179.9999, ymax = 82))
  }
  
  border <- sf::st_union(border_adm1)
  bbox <- sf::st_bbox(border)
  
  if (hero == "CHL") {
    lims <- list(
      xlim = c("xmin" = -76, "xmax" = -55.91850),
      ylim = c("ymin" = -66.42081, "ymax" = -17.50659)
    )
  } else {
    lims <- list(
      xlim = c(bbox$xmin, bbox$xmax),
      ylim = c(bbox$ymin, bbox$ymax)
    )
  }
  
  output <- list()
  
  output$border_adm1 <- border_adm1
  output$border <- border
  output$lims <- lims
  output$aspect <- (lims$ylim[2] - lims$ylim[1]) / (lims$xlim[2] - lims$xlim[1])
  
  return(output)
}

map_scale <- function(values) {
  
  values <- sort(values)
  
  labels <- prettylabel
  
  if (length(unique(values)) <= 2) {
    breaks <- unique(values)
  }
  
  if (length(unique(values)) >= 3) {
    
    breaks <- c(
      min(values), 
      round((max(values) + min(values)) / 4), 
      max(values)
    )
    
    labels <- c(
      min(values), 
      round((max(values) + min(values)) / 4), 
      max(values)
    ) |> prettylabel()
  }
  
  output <- list()
  output$labels <- labels
  output$breaks <- breaks
  
  return(output)
}

map_base <- function(hero, k) {
  
  map <- map_data(hero)
  
  plot <- ggplot() + 
    geom_sf(
      data = map$border, 
      fill = pal("grays", 5), color = pal("grays", 4), linewidth = .10
    ) +
    geom_sf(
      data = map$border_adm1, 
      fill = NA, color = pal("grays", 4), linewidth = .05
    ) + 
    coord_sf(
      xlim = map$lims$xlim, 
      ylim = map$lims$ylim, 
      expand = FALSE,
      clip = "off"
    ) +
    apply_theme("map", basesize = basesize, font = font) +
    theme(
      legend.spacing.x = unit(.25 * basesize, "points"),
      legend.text = element_text(margin = margin(t = 0)),
      legend.title = element_text(size = basesize),
      plot.margin = margin(0, 0, 0, 0)
    )
  
  return(plot)
}

map_final <- function(base, title, source, basesize, font) {
  
  k <- function(factor = 1) factor * basesize / .pt
  
  plot_title <- ggplot() + ggtitle(title) +
    apply_theme("map", basesize = basesize, font = font) +
    theme(plot.margin = margin(0, 0, 0, 0))
  
  plot_caption <- ggplot() + labs(caption = source) +
    apply_theme("map", basesize = basesize, font = font) +
    theme(plot.margin = margin(0, 0, 0, 0))
  
  plot <- plot_grid(
    plot_title, base, plot_caption,
    nrow = 3,
    rel_heights = c(.15, 1, .05)
  ) +
    theme(plot.margin = margin(k(2), k(3), k(2), k(3)))
  
  return(plot)
}


# Miscellaneous -----------------------------------------------------------

kosovo_disclaimer <- function(hero) {
  
  text <- paste(
    "References to Kosovo shall be understood to be in the context of United",
    "Nations Security Council resolution 1244 (1999). "
  )
  
  if (hero == "XKX") {
    
    return(text)
    
  } else {
    
    return_text <- ""
    return(return_text)
  }
}
