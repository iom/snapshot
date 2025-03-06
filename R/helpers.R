
basesize <- 7
font <- "Gill Sans Nova"

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

add_typst <- function(file, folder = "typ") {
  readLines(paste0(folder, "/", file, ".typ")) |> 
    paste(collapse = "\n")
}

namer <- function(iso, bold = FALSE) {
  
  if (is.null(iso)) {
    
    return(NULL)
    
  } else {
  
    name_iso <- filter(gdidata::countrynames, iso3 == iso)
    
    if (bold) {
      
      if (name_iso$with_the == 1) {
        name <- paste0("the ", "#b[", name_iso$name_text, "]")
      }
      else name <- paste("#b[", name_iso$name_text, "]")
      
    } else {
      
      if (name_iso$with_the == 1) name <- paste0("the ", name_iso$name_text)
      else name <- name_iso$name_text
    }
  }
  
  return(name)
}


break_lines <- function(column) {

  dict <- c(
    "Plurinational State of Bolivia" =
      "Plurinational State\nof Bolivia",
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
      "Bolivarian Republic\nof Venezuela"
  )

  indices <- which(column %in% names(dict))

  new_col <- replace(
    column,
    indices,
    dict[column[indices]]
  )

  return(new_col)
}

set_axis <- function(values, units = "Persons") {

  max_n <- max(values)

  write_title <- function(scale, units) {
    if (units == "USD") text <- paste0(scale, " ", units)
    else text <- paste0(scale, " of ", tolower(units))
    return(text)
  }

  output <- list(
    breaks = waiver(),
    labels = function(x) x / 10^6,
    title = write_title("Millions", units)
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
    output$title <- write_title("Thousands", units)
    output$labels <- function(x) x / 1000
  }
  if (max_n >= 1.20 * 10^6 & max_n < 1.40 * 10^6) {
    output$breaks <- seq(0, 1.25 * 10^6, .25 * 10^6)
    output$labels <- c("0", "0.25", "0.50", "0.75", "1", "1.25")
  }
  if (max_n >= 1.40 * 10^6 & max_n < 1.80 * 10^6) {
    output$breaks <- seq(0, 1.50 * 10^6, .50 * 10^6)
    output$labels <- c("0", "0.5", "1", "1.5")
  }

  if (max_n >= 1.20 * 10^9 & max_n < 1.40 * 10^9) {
    output$title <- write_title("Billions", units)
    output$breaks <- seq(0, 1.25 * 10^9, .25 * 10^9)
    output$labels <- c("0", "0.5", "1", "1.5")
  }
  if (max_n >= 1.40 * 10^9 & max_n < 1.80 * 10^9) {
    output$title <- write_title("Billions", units)
    output$breaks <- seq(0, 1.50 * 10^9, .50 * 10^9)
    output$labels <- c("0", "0.5", "1", "1.5")
  }
  if (max_n >= 1.80 * 10^9) {
    output$title <- write_title("Billions", units)
    output$labels <- function(x) x / 10^9
  }

  return(output)
}


# # Helper functions for country brief generation
# 
# scale_labels <- function(N) {
#   max <- max(N, na.rm = TRUE)
#   if (max < 1200) {
#     n <- pretty(N, d = 1, dotzero = FALSE)
#   }
#   if ((max >= 1200) & (max < 1200000)) {
#     n <- pretty(N / 10^3, d = 1, dotzero = FALSE)
#   }
#   if (max >= 1200000) {
#     n <- pretty(N / 10^6, d = 1, dotzero = FALSE)
#   }
#   return(n)
# }
# 
# scale_title <- function(data) {
#   if (max(data$v) < 1200) {
#     title <- "persons"
#   }
#   if ((max(data$v) >= 1200) & (max(data$v) < 1200000)) {
#     title <- "thousands of persons"
#   }
#   if (max(data$v) >= 1200000) {
#     title <- "millions of persons"
#   }
#   return(title)
# }


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


