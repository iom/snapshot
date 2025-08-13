
ranger <- function(data) c(min(data$t), max(data$t))


# UN DESA -----------------------------------------------------------------

get_stocks <- function(iso = NULL, ...) {
  
  args <- list(...)
  output <- list(data = NULL, range = NULL)
  
  dataset <- gdidata::undesa_stocks
  if ("use_2020" %in% names(args) && args$use_2020) {
    dataset <- gdidata::undesa_stocks_2020
  }
  
  dataset <- dataset |>
    left_join(gdidata::countrynames, by = c("geo" = "iso3")) |>
    rename(region_geo = iom_region) |>
    left_join(gdidata::countrynames, by = c("from" = "iso3")) |>
    rename(region_from = iom_region) |>
    
    # Determine whether flow is intra- or inter-regional
    mutate(region = case_when(
      is.na(region_geo) | is.na(region_from) ~ "Unknown",
      region_geo == region_from ~ "Within region",
      .default = "Outside region"
    )) |>
    summarise(n = sum(n), .by = c(geo, from, region, t))
  
  emig <- dataset |>
    summarise(n = sum(n), .by = c(from, region, t)) |>
    mutate(panel = "emig") |>
    rename(geo = from)
  
  immig <- dataset |>
    summarise(n = sum(n), .by = c(geo, region, t)) |>
    mutate(panel = "immig")
  
  output$data <- bind_rows(emig, immig)
  if (!is.null(iso)) output$data <- filter(output$data, geo == iso)
  output$range <- ranger(dataset)
  
  return(output)
}

get_nats <- function(iso, lang = "en", ...) {
  
  args <- list(...)
  output <- list(data = NULL, range = NULL)
  
  if (lang == "en") {
    others <- "Others"
    name_col <- "name_text"
  }
  
  if (lang == "pt") {
    others <- "Outros"
    name_col <- "name_pt"
  }
  
  topn <- 5
  panel_dest <- "Destinations of emigrants"
  panel_orig <- "Origins of immigrants"
  
  dataset <- gdidata::undesa_stocks
  if ("use_2020" %in% names(args) && args$use_2020) {
    dataset <- gdidata::undesa_stocks_2020
  }
  
  dataset <- dataset |>
    filter(t == max(t)) |>
    left_join(gdidata::countrynames, by = c("geo" = "iso3")) |>
    rename(region_geo = iom_region) |>
    left_join(gdidata::countrynames, by = c("from" = "iso3")) |>
    rename(region_from = iom_region) |>
    
    # Determine whether flow is intra- or inter-regional
    mutate(region = case_when(
      is.na(region_geo) | is.na(region_from) ~ "Unknown",
      region_geo == region_from ~ "Within region",
      .default = "Outside region"
    )) |>
    summarise(
      n = sum(n),
      .by = c(geo, from, region, t)
    )
  
  emig <- dataset |>
    filter(from == iso) |>
    mutate(panel = panel_dest) |>
    rename(nat = geo, iso = from)
  
  immig <- dataset |>
    filter(geo == iso) |>
    mutate(panel = panel_orig) |>
    rename(iso = geo, nat = from)
  
  df <- bind_rows(emig, immig) |>
    group_by(panel) |>
    arrange(desc(n), .by_group = TRUE) |>
    ungroup()
  
  if (nrow(df) > 0) {
    
    destin <- filter(df, panel == panel_dest) |>
      slice_head(n = topn) |>
      pull(nat)
    
    origin <- filter(df, panel == panel_orig) |>
      slice_head(n = topn) |>
      pull(nat)
    
    destin_order <- gdidata::countryname(destin, to = name_col) |> rev()
    origin_order <- gdidata::countryname(origin, to = name_col) |> rev()
    
    df_destin1 <- filter(df, panel == panel_dest) |>
      mutate(country = case_when(
        nat %in% destin ~
          gdidata::countryname(nat, from = "iso3", to = name_col),
        .default = others
      )) |>
      mutate(region = ifelse(country == others, others, region)) |>
      summarise(
        n = sum(n),
        .by = c(panel, country, region)
      )
    
    df_destin <- df_destin1 |>
      arrange(match(country, c(others, destin_order)))
    df_destin_print <- df_destin1 |>
      arrange(match(country, c(rev(destin_order), others))) |>
      rename(Destination = country)
    
    df_origin1 <- filter(df, panel == panel_orig) |>
      mutate(country = case_when(
        nat %in% origin ~
          gdidata::countryname(nat, from = "iso3", to = name_col),
        .default = others
      )) |>
      mutate(region = ifelse(country == others, others, region)) |>
      summarise(
        n = sum(n),
        .by = c(panel, country, region)
      )
    
    df_origin <- df_origin1 |>
      arrange(match(country, c(others, origin_order)))
    df_origin_print <- df_origin1 |>
      arrange(match(country, c(rev(origin_order), others))) |>
      rename(Origin = country)
    
    output$data <- bind_rows(df_destin, df_origin) |>
      mutate(share = 100 * n / sum(n), .by = panel)
  }
  
  output$range <- max(dataset$t)
  
  return(output)
}

get_pyr <- function(iso) {
  
  output <- list()
  
  output$data <- gdidata::undesa_wpp |> 
    filter(geo == iso, t == max(t)) |> 
    mutate(age = case_when(
      age %in% 0:4              ~ "0-4", 
      age %in% 5:9              ~ "5-9", 
      age %in% 10:14            ~ "10-14", 
      age %in% 15:19            ~ "15-19", 
      age %in% 20:24            ~ "20-24", 
      age %in% 25:29            ~ "25-29", 
      age %in% 30:34            ~ "30-34", 
      age %in% 35:39            ~ "35-39", 
      age %in% 40:44            ~ "40-44", 
      age %in% 45:49            ~ "45-49", 
      age %in% 50:54            ~ "50-54", 
      age %in% 55:59            ~ "55-59", 
      age %in% 60:64            ~ "60-64", 
      age %in% 65:69            ~ "65-69", 
      age %in% 70:74            ~ "70-74", 
      .default = "75+" 
    )) |> 
    summarise(
      n = sum(n), 
      .by = c(geo, t, sex, age)
    ) |> 
    mutate(
      v = 100 * n / sum(n),
      v = ifelse(sex == "male", -v, v),
      sex = factor(sex, levels = c("male", "female"))
    )
  
  output$range <- max(gdidata::undesa_wpp$t)
  
  return(output)
}

get_migpyr <- function(iso) {
  
  output <- list()
  
  dataset <- bind_rows(
    readr::read_csv("data-raw/Emigrants_AgeSex.csv") |> 
      mutate(panel = "Emigrants"),
    readr::read_csv("data-raw/Immigrants_AgeSex.csv") |> 
      mutate(panel = "Immigrants")
  ) |> 
    select(
      panel, 
      geo = Country, 
      t = Year, 
      sex = Sex, 
      age = Age, 
      n = Value
    )
  
  output$data <- dataset |> 
    filter(geo == iso, sex != "total", t == max(t)) |> 
    mutate(
      sex = factor(sex, levels = c("male", "female")),
      v = 100 * n / sum(n), 
      .by = panel
    )
  
  output$range <- max(dataset$t)
  
  return(output)
}

get_immorig <- function(iso = NULL, ...) {
  
  args <- list(...)
  
  output <- list(
    data = NULL,
    range = NULL,
    max_t = NULL
  )
  
  dataset <- gdidata::undesa_stocks
  if ("use_2020" %in% names(args) && args$use_2020) {
    dataset <- gdidata::undesa_stocks_2020
  }
  
  data_filter <- dataset |>
    filter(t == max(t), geo == iso) |> 
    summarise(n = sum(n), .by = c(geo, t, from)) |> 
    arrange(desc(n))
  
  unknown <- data_filter[data_filter$from == "OOO", ]
  top <- data_filter[data_filter$from != "OOO", ] |> slice_head(n = 10)
  
  others_count <- nrow(data_filter) - nrow(top) - nrow(unknown)
  selected <- c(top$from, unknown$from)
  
  if (others_count >= 2) {
    
    # Aggregate nationalities outside top 10 into "Others"
    others <- data_filter[!(data_filter$from %in% selected), ] |> 
      summarise(n = sum(n), .by = c(geo, t)) |> 
      mutate(from = "Others")
    
  } else if (others_count == 1) {
    
    # If only one country is covered in "Others", then just display that country
    others <- data_filter[!(data_filter$from %in% selected), ]
    
  } else {
    
    others <- NULL
  }
  
  data <- bind_rows(top, others, unknown) |> 
    mutate(
      label = ifelse(
        from == "Others", from, gdidata::countryname(from, to = "name_text")
      ),
      from = factor(from, levels = from),
    ) |> 
    suppressWarnings()
  
  output$data <- data
  output$max_t <- max(data$t)
  output$range <- max(data$t)
  
  return(output)
}

get_immdep <- function(iso = NULL) {
  
  output <- list(
    data = NULL,
    range = NULL,
    max_t = NULL
  )
  
  data_gen <- gdidata::wdi[gdidata::wdi$var == "depend", ] |> 
    select(geo, t, v_gen = v)
  
  depend_ages <- c("0-4", "5-9", "10-14", "65-69", "70-74", "75+")
  
  data_immig <- migdemog |> 
    filter(var == "immigrants", sex == "total") |> 
    mutate(group = case_when(
      age %in% depend_ages ~ "dependent",
      .default = "working"
    )) |> 
    summarise(n = sum(n), .by = c(geo, t, group)) |> 
    pivot_wider(names_from = group, values_from = n) |> 
    mutate(v_immig = 100 * dependent / working) |> 
    drop_na(v_immig) |> 
    select(geo, t, v_immig)
  
  data <- data_immig |> 
    left_join(data_gen, by = c("geo", "t")) |> 
    filter(geo == iso) |> 
    pivot_longer(
      cols = starts_with("v_"),
      names_to = "var",
      names_prefix = "v_",
      values_to = "v"
    )
  
  output$data <- data
  
  if (nrow(data) > 0) {
    output$max_t <- max(data$t)
    output$range <- ranger(data)
  }
  
  return(output)
}


# UNHCR -------------------------------------------------------------------

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
  

# MMP ---------------------------------------------------------------------

get_mmp <- function(iso, lang = "en") {
  
  output <- list(data = NULL, range = NULL)
  
  t1 <- max(gdidata::wdi$t)
  
  data_full <- gdidata::iom_mmp |>
    mutate(t = year(t)) |>
    summarise(n = sum(dead), .by = t)
  
  data <- gdidata::iom_mmp |> 
    filter(geo == iso) |>
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
    summarise(
      n = sum(dead),
      .by = c(geo, t, cause)
    ) |>
    drop_na() |> 
    filter(t <= t1)
  
  if (lang == "pt") {
    causes_trans <- c(
      "Drowning"          = "Afogamento",
      "Transport hazards" = "Riscos de transporte",
      "Harsh conditions"  = "Condições severas",
      "Accidental"        = "Acidental",
      "Sickness"          = "Doença",
      "Violence"          = "Violência",
      "Mixed or unknown"  = "Mista ou desconhecida"
    )
    data <- data |> 
      mutate(cause = recode(cause, !!!causes_trans))
  }
  
  output$data <- data
  output$range <- c(ranger(data_full)[1], t1)
  
  return(output)
}

get_mmpmap <- function(iso, lang = "en") {
  
  output <- list()
  
  map <- map_data(iso)
  t0 <- min(gdidata::iom_mmp$t) |> year()
  t1 <- max(gdidata::wdi$t)
  
  data <- gdidata::iom_mmp |> 
    filter(
      geo == iso,
      between(lon, map$lims$xlim["xmin"], map$lims$xlim["xmax"]),
      between(lat, map$lims$ylim["ymin"], map$lims$ylim["ymax"])
    ) |> 
    mutate(t = year(t)) |> 
    filter(t <= t1) |> 
    summarise(n = sum(dead), .by = c(t, cause, lon, lat)) |> 
    drop_na() |> 
    arrange(desc(n))
  
  if (lang == "en") {
    data <- data |> 
      mutate(cause = case_when(
        str_detect(cause, "Accidental") ~ "Accidental",
        str_detect(cause, "Harsh")      ~ "Harsh conditions",
        str_detect(cause, "Sickness")   ~ "Sickness",
        str_detect(cause, "transport")  ~ "Transport hazards",
        .default = cause
      ))
  }
  
  if (lang == "pt") {
    data <- data |> 
      mutate(cause = case_when(
        str_detect(cause, "Drowning")   ~ "Afogamento",
        str_detect(cause, "transport")  ~ "Riscos de transporte",
        str_detect(cause, "Harsh")      ~ "Condições severas",
        str_detect(cause, "Accidental") ~ "Acidental",
        str_detect(cause, "Sickness")   ~ "Doença",
        str_detect(cause, "Violence")   ~ "Violência",
        str_detect(cause, "Mixed")      ~ "Mista ou desconhecida",
        .default = cause
      ))
  }
  
  output$data <- data
  output$range <- c(t0, t1)
  
  return(output)
}


# IDMC --------------------------------------------------------------------

get_idp <- function(iso = NULL) {
  
  output <- list()
  
  causes <- c("Environmental impacts", "Conflict and violence")
  
  output$data <- gdidata::idmc_flows |> 
    filter(geo == iso) |>
    mutate(cause = ifelse(
      cause == "conflict",
      causes[2],
      causes[1]
    )) |>
    summarise(
      n = sum(n),
      .by = c(geo, t, cause)
    )
  
  output$range <- ranger(gdidata::idmc_flows)
  
  return(output)
}


get_idcause <- function(iso = NULL, lang = "en") {
  
  output <- list()
  
  data <- gdidata::idmc_flows |> 
    filter(t >= max(t) - 9, cause != "conflict") |> 
    summarise(n = sum(n), .by = c(geo, cause)) |> 
    group_by(geo) |> 
    arrange(desc(n), by_group = TRUE) |> 
    mutate(
      rank = 1:n(),
      cause = ifelse(
        rank >= 4,
        "Others",
        paste0(
          toupper(substr(cause, 1, 1)), 
          substr(cause, 2, nchar(cause))
        ))
    ) |> 
    ungroup() |> 
    summarise(n = sum(n), .by = c(geo, cause)) |> 
    mutate(v = n / sum(n), .by = geo) |> 
    arrange(geo, desc(n))
  
  if (lang == "pt") {
    data <- data |> 
      mutate(cause = case_when(
        cause == "Flood" ~ "Inundação",
        cause == "Earthquake" ~ "Terremoto",
        cause == "Drought" ~ "Seca",
        cause == "Storm" ~ "Tempestade",
        cause == "Mass movement" ~ "Movimento em massa",
        cause == "Wildfire" ~ "Incêndio florestal",
        cause == "Extreme temperature" ~ "Temperatura extrema",
        cause == "Volcanic activity" ~ "Atividade vulcânica",
        cause == "Wave action" ~ "Ação das ondas",
        cause == "Others" ~ "Outros",
      ))
  }
  
  output$data <- data
  
  if (!is.null(iso)) {
    
    output$data <- filter(output$data, geo == iso)
  }
  
  output$range <- c(
    max(gdidata::idmc_flows$t) - 9,
    max(gdidata::idmc_flows$t)
  )
  
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

get_nmig <- function(iso = NULL) {
  
  output <- list(data = NULL, range = NULL)
  
  output$data <- filter(gdidata::wdi, var == "nmig") |> rename(n = v)
  output$range <- ranger(output$data)
  if (!is.null(iso)) output$data <- filter(output$data, geo == iso)
  
  return(output)
}

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

  
  