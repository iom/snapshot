
library(tidyverse)

migdemog <- bind_rows(
  read_csv("data-raw/Emigrants_AgeSex.csv") |> mutate(var = "emigrants"),
  read_csv("data-raw/Immigrants_AgeSex.csv") |> mutate(var = "immigrants")
) |> 
  select(geo = Country, var, t = Year, sex = Sex, age = Age, n = Value)

usethis::use_data(migdemog, overwrite = TRUE)