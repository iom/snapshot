
library(dplyr)

types = c("Conflict and violence", "Environmental impacts")

idmc_geo <- readr::read_csv("data-raw/Displacements_geolocated.csv") |> 
  select(
    geo = Country, t = Year, type = Type,
    n = Displacements, lat = latitude, lon = longitude
  ) |> 
  filter(type != "Other", t > max(t) - 5) |> 
  mutate(type = case_when(
    type == "Conflict" ~ types[1],
    type == "Disaster" ~ types[2]
  )) |> 
  summarise(n = sum(n), .by = c(geo, t, type, lon, lat))

usethis::use_data(idmc_geo, overwrite = TRUE)

