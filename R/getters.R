
get_refug <- function(iso = NULL, lang = "en", ...) {
  
  output <- list(data = NULL, range = NULL)
  
  years <- c(seq(1990, 2020, 5), 2021:max(gdidata::unhcr$t))
  
  dataset <- gdidata::unhcr |>
    filter((t %in% years | t == max(t)), group == "refugee")
  
  output$range <- c(min(years), max(years))
  
  if (is.null(iso)) {
    
    output$data <- bind_rows(
      dataset |> 
        summarise(n = sum(n), .by = c(from, t)) |> 
        rename(geo = from) |> 
        mutate(panel = "orig"),
      dataset |> 
        summarise(n = sum(n), .by = c(geo, t)) |> 
        mutate(panel = "host")
    )
    
  } else {
    
    name_text <- "name_text"
    others <- "Others"
    unknown <- "Unknown"
    total <- "Total"
    
    if (lang == "pt") {
      name_text <- "name_pt"
      others <- "Outros"
      unknown <- "Desconhecido"
      total <- "Total"
    }
    
    orig <- filter(dataset, from == iso)
    
    if (nrow(orig) > 0) {
      
      orig_top_t1 <- filter(orig, t == max(t)) |>
        arrange(desc(n)) |>
        slice_head(n = 3)
      orig_top_names <- gdidata::countryname(orig_top_t1$geo, to = name_text)
      orig <- orig |>
        mutate(
          nat = ifelse(geo %in% orig_top_t1$geo, geo, others),
          nat_name = ifelse(
            nat == others,
            nat, 
            countryname(nat, to = name_text) |> suppressWarnings()
          )
        ) |>
        summarise(
          n = sum(n), 
          .by = c(nat, nat_name, t)
        ) |>
        mutate(panel = "orig") |>
        complete(
          t = years,
          tidyr::nesting(nat, nat_name, panel),
          fill = list(n = 0)
        )
      
    } else orig_top_names <- ""
    
    host <- filter(dataset, geo == iso)
    
    if (nrow(host) > 0) {
      
      host_without_unknown <- filter(host, from != "OOO")
      host_top_t1 <- filter(host_without_unknown, t == max(t)) |>
        arrange(desc(n)) |>
        slice_head(n = 3)
      host_top_names <- gdidata::countryname(host_top_t1$from, to = name_text)
      host <- host |>
        mutate(
          nat = case_when(
            from %in% c(host_top_t1$from, "OOO") ~ from, 
            .default = others
          ),
          nat_name = ifelse(
            nat == others,
            nat, 
            countryname(nat, to = name_text) |> suppressWarnings()
          )
        ) |>
        summarise(
          n = sum(n), 
          .by = c(nat, nat_name, t)
        ) |>
        mutate(panel = "host") |>
        complete(
          t = years,
          tidyr::nesting(nat, nat_name, panel),
          fill = list(n = 0)
        )
    } else {
      host <- host |> rename(nat = from)
      host_top_names <- NULL
    }
    
    output$data <- bind_rows(orig, host)
    
    if (nrow(output$data) > 0) {
      
      output$data <- output$data |>
        mutate(nat_name = factor(nat_name, levels = c(
          unique(c(orig_top_names, host_top_names)),
          unknown, others
        )))
      
      agg <- output$data |>
        summarise(n = sum(n), .by = c(t, panel)) |>
        mutate(nat_name = total)
    }
  
  }
  
  return(output)
}
  

# Money flows -------------------------------------------------------------

get_remt <- function(iso = NULL, ...) {
  
  remt <- gdidata::wdi |> filter(var %in% c("remin", "remout"))
  
  output <- list()
  output$data <- remt |> filter(geo == iso, v > 0) |> rename(n = v)
  output$range <- c(min(remt$t), max(remt$t))
  
  return(output)
}

get_fdi <- function(iso = NULL, lang = "en", ...) {
  
  fdi <- gdidata::wdi |> filter(var %in% c("fdiin", "fdiout"))
  
  output <- list()
  output$data <- fdi |> filter(geo == iso, v > 0) |> rename(n = v)
  output$range <- c(min(fdi$t), max(fdi$t))
  
  return(output)
}


# Socioeconomic -----------------------------------------------------------

get_pop <- function(iso = NULL, lang = "en") {
  
  output <- list()
  dataset <- gdidata::wdi |> filter(var == "pop")
  dataset_iso <- dataset |> filter(geo == iso)
  
  worldmed <- "World median"
  name_text <- "name_text"
  if (lang == "pt") {
    worldmed <- "Mediana mundial"
    name_text <- "name_pt"
  }
  
  if (nrow(dataset_iso) == 0) {
    
    output$data <- dataset_iso
    
  } else {
    
    output$data <- bind_rows(
      dataset_iso |> 
        mutate(var = countryname(iso, to = name_text)),
      dataset |> 
        summarise(v = median(v, na.rm = TRUE), .by = t) |> 
        mutate(var = worldmed)
    ) |> 
      mutate(var = forcats::fct_relevel(var, worldmed, after = 1)) |> 
      rename(n = v) |> 
      complete(t = min(dataset$t):max(dataset$t))
  }
  
  output$range <- c(min(dataset$t), max(dataset$t))
  
  return(output)
}

get_birth <- function(iso = NULL, lang = "en") {
  
  output <- list()
  dataset <- gdidata::wdi |> filter(var == "birth")
  dataset_iso <- dataset |> filter(geo == iso)
  
  worldmed <- "World median"
  name_text <- "name_text"
  if (lang == "pt") {
    worldmed <- "Mediana mundial"
    name_text <- "name_pt"
  }
  
  if (nrow(dataset_iso) == 0) {
    
    output$data <- dataset_iso
    
  } else {
    
    output$data <- bind_rows(
      dataset_iso |> 
        mutate(var = countryname(iso, to = name_text)),
      dataset |> 
        summarise(v = median(v, na.rm = TRUE), .by = t) |> 
        mutate(var = worldmed)
    ) |> 
      mutate(var = forcats::fct_relevel(var, worldmed, after = 1)) |> 
      rename(n = v) |> 
      complete(t = min(dataset$t):max(dataset$t))
  }
  
  output$range <- c(min(dataset$t), max(dataset$t))
  
  return(output)
}

get_depend <- function(iso = NULL, lang = "en") {
  
  output <- list()
  dataset <- gdidata::wdi |> filter(var == "depend")
  dataset_iso <- dataset |> filter(geo == iso)
  
  worldmed <- "World median"
  name_text <- "name_text"
  if (lang == "pt") {
    worldmed <- "Mediana mundial"
    name_text <- "name_pt"
  }
  
  if (nrow(dataset_iso) == 0) {
    
    output$data <- dataset_iso
    
  } else {
    
    output$data <- bind_rows(
      dataset_iso |> 
        mutate(var = countryname(iso, to = name_text)),
      dataset |> 
        summarise(v = median(v, na.rm = TRUE), .by = t) |> 
        mutate(var = worldmed)
    ) |> 
      mutate(var = forcats::fct_relevel(var, worldmed, after = 1)) |> 
      rename(n = v) |> 
      complete(t = min(dataset$t):max(dataset$t))
  }
  
  output$range <- c(min(dataset$t), max(dataset$t))
  
  return(output)
}

get_income <- function(iso = NULL, lang = "en") {
  
  output <- list()
  dataset <- gdidata::wdi |> filter(var == "income")
  dataset_iso <- dataset |> filter(geo == iso)
  
  worldmed <- "World median"
  name_text <- "name_text"
  if (lang == "pt") {
    worldmed <- "Mediana mundial"
    name_text <- "name_pt"
  }
  
  if (nrow(dataset_iso) == 0) {
    
    output$data <- dataset_iso
    
  } else {
    
    output$data <- bind_rows(
      dataset_iso |> 
        mutate(var = countryname(iso, to = name_text)),
      dataset |> 
        summarise(v = median(v, na.rm = TRUE), .by = t) |> 
        mutate(var = worldmed)
    ) |> 
      mutate(var = forcats::fct_relevel(var, worldmed, after = 1)) |> 
      rename(n = v) |> 
      complete(t = min(dataset$t):max(dataset$t))
  }
  
  output$range <- c(min(dataset$t), max(dataset$t))
  
  return(output)
}

get_unem <- function(iso = NULL, lang = "en") {
  
  output <- list()
  dataset <- gdidata::wdi |> filter(var == "unem")
  dataset_iso <- dataset |> filter(geo == iso)
  
  worldmed <- "World median"
  name_text <- "name_text"
  if (lang == "pt") {
    worldmed <- "Mediana mundial"
    name_text <- "name_pt"
  }
  
  if (nrow(dataset_iso) == 0) {
    
    output$data <- dataset_iso
    
  } else {
    
    output$data <- bind_rows(
      dataset_iso |> 
        mutate(var = countryname(iso, to = name_text)),
      dataset |> 
        summarise(v = median(v, na.rm = TRUE), .by = t) |> 
        mutate(var = worldmed)
    ) |> 
      mutate(var = forcats::fct_relevel(var, worldmed, after = 1)) |> 
      rename(n = v) |> 
      complete(t = min(dataset$t):max(dataset$t))
  }
  
  output$range <- c(min(dataset$t), max(dataset$t))
  
  return(output)
}

get_inf <- function(iso = NULL, lang = "en") {
  
  output <- list()
  dataset <- gdidata::wdi |> filter(var == "inf")
  dataset_iso <- dataset |> filter(geo == iso)
  
  worldmed <- "World median"
  name_text <- "name_text"
  if (lang == "pt") {
    worldmed <- "Mediana mundial"
    name_text <- "name_pt"
  }
  
  if (nrow(dataset_iso) == 0) {
    
    output$data <- dataset_iso
    
  } else {
    
    output$data <- bind_rows(
      dataset_iso |> 
        mutate(var = countryname(iso, to = name_text)),
      dataset |> 
        summarise(v = median(v, na.rm = TRUE), .by = t) |> 
        mutate(var = worldmed)
    ) |> 
      mutate(var = forcats::fct_relevel(var, worldmed, after = 1)) |> 
      rename(n = v) |> 
      complete(t = min(dataset$t):max(dataset$t))
  }
  
  output$range <- c(min(dataset$t), max(dataset$t))
  
  return(output)
}

  
  