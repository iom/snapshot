
library(tidyverse)
devtools::load_all(".")

# Sharepoint drive
save_path <- "GDI Country Data Snapshot/Snapshots"


# Render all --------------------------------------------------------------

regions <- c(
  "Americas and Caribbean", 
  "Asia and the Pacific",
  "East, Horn and Southern Africa",
  "Europe and Central Asia",
  "Middle East and North Africa",
  "West and Central Africa"
)

for (region in regions) {
  
  countries <- gdidata::countrynames |> 
    filter(iom_region == region, snapshot == 1) |> 
    pull(iso3)
  
  for (country in countries) {
    render_snapshot(
      country, 
      target_dir = file.path(save_path, region)
    )
  }
}


# Render by region --------------------------------------------------------

region_select <- regions[4]

countries <- gdidata::countrynames |> 
  dplyr::filter(iom_region == region_select, snapshot == 1) |> 
  pull(iso3)

for (country in countries[20:55]) {
  render_snapshot(
    country, 
    target_dir = file.path(save_path, region_select)
  )
}


# Test select countries ---------------------------------------------------

test_countries <- c("GNQ", "TCD")

for (country in test_countries) {
  render_snapshot(country)
}

render_snapshot("ITA")
