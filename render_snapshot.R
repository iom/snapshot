
library(tidyverse)
devtools::load_all(".")

v <- "2.0.0"


# Test select countries ---------------------------------------------------

test_countries <- c("DEU", "LBY", "VEN")

for (country in test_countries) {
  render_snapshot(country, version = v)
}


# Render all --------------------------------------------------------------

groupings <- gdidata::countrynames |> 
  filter(!(iso3 %in% c(
    "AB9", 
    "IOT", "ATF", "MYT", "REU", "SYC",
    "BES", "BVT", "FLK", "GRL", "MAF", "SXM", "SGS", "BLM", "ANT",
    "HMD", "NFK", "PCN", "TIB", "UMI", "CXR", "CCK", "MHL", "TKL",
    "ALA", "FRO", "GGY", "GIB", "IMN", "JEY", "XCX", "SJM"
  ))) |>
  mutate(grouping = case_when(
    str_detect(subregion, "Europe") | 
      subregion == "Central Asia" |
      name %in% c("Armenia", "Azerbaijan", "Cyprus", "Georgia", "Türkiye") ~ 
      "Europe and Central Asia",
    intregion %in% c("Middle Africa", "Western Africa") ~ 
      "West and Central Africa",
    intregion %in% c("Eastern Africa", "Southern Africa") ~ 
      "East, Horn and Southern Africa",
    region == "Oceania" | 
      subregion %in% c("Eastern Asia", "South-eastern Asia", "Southern Asia") ~ 
      "Asia and the Pacific",
    region == "Americas" ~ "Americas and Caribbean",
    subregion %in% c("Northern Africa", "Western Asia") ~ 
      "Middle East and North Africa",
    .default = NA
  )) |> 
  select(iso3, name_text, grouping) |> 
  filter(!is.na(grouping))

groups <- c(
  "Middle East and North Africa",
  "West and Central Africa",
  "East, Horn and Southern Africa",
  "Americas and Caribbean", 
  "Asia and the Pacific",
  "Europe and Central Asia"
)

countries <- filter(groupings, grouping == groups[6]) |> pull(iso3)

for (country in countries) {
  render_snapshot(country, version = v)
}


