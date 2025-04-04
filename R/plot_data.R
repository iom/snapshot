
#' Grab data behind a preset plot
#'
#' @param key Plot key.
#' @param iso Country.
#'
#' @export
plot_data <- function(key, iso = NULL, ...) {
  
  ranger <- function(data) c(min(data$t), max(data$t))
  name <- namer(iso)
  
  output <- list(
    data = NULL,
    print = NULL,
    range = NULL,
    max_t = NULL
  )
  
  args <- list(...)
  
  if (key == "stocks") {
    
    dataset <- gdidata::undesa_stocks
    if ("use_2020" %in% names(args) && args$use_2020) {
      dataset <- gdidata::undesa_stocks_2020
    }
    
    dataset <- dataset |>
      left_join(gdidata::countrynames, by = c("geo" = "iso3")) |>
      rename(region_geo = .data$iom_region) |>
      left_join(gdidata::countrynames, by = c("from" = "iso3")) |>
      rename(region_from = .data$iom_region) |>
      
      # Determine whether flow is intra- or inter-regional
      mutate(region = case_when(
        is.na(.data$region_geo) | is.na(.data$region_from) ~ "Unknown",
        .data$region_geo == .data$region_from ~ "Within region",
        .default = "Outside region"
      )) |>
      summarise(
        n = sum(.data$n),
        .by = c(.data$geo, .data$from, .data$region, .data$t)
      )
    
    emig <- dataset |>
      # filter(.data$from == iso) |>
      summarise(
        n = sum(.data$n),
        .by = c(.data$from, .data$region, .data$t)
      ) |>
      mutate(panel = "emig") |>
      rename(geo = .data$from)
    
    immig <- dataset |>
      # filter(.data$geo == iso) |>
      summarise(
        n = sum(.data$n),
        .by = c(.data$geo, .data$region, .data$t)
      ) |>
      mutate(panel = "immig")
    
    output$data <- bind_rows(emig, immig)
    
    if (!is.null(iso)) {
      
      output$data <- filter(output$data, .data$geo == iso)
      
      if (nrow(output$data) > 0) {
        
        output$print <- output$data |>
          mutate(
            country = gdidata::countryname(.data$geo),
            panel = ifelse(.data$panel == "emig", "Emigrants", "Immigrants")
          ) |>
          select(.data$country, .data$panel, .data$region, .data$t, .data$n) |>
          tidyr::pivot_wider(names_from = .data$region, values_from = .data$n) |>
          rename(
            Country = .data$country,
            Panel = .data$panel,
            Year = .data$t,
          )
      }
    }
    
    output$range <- ranger(dataset)
  }
  
  if (key == "nats") {
    
    topn <- 5
    
    panel_dest <- "Destinations of emigrants"
    panel_orig <- "Origins of immigrants"
    
    dataset <- gdidata::undesa_stocks
    if ("use_2020" %in% names(args) && args$use_2020) {
      dataset <- gdidata::undesa_stocks_2020
    }
    
    dataset <- dataset |>
      filter(.data$t == max(.data$t)) |>
      left_join(gdidata::countrynames, by = c("geo" = "iso3")) |>
      rename(region_geo = .data$iom_region) |>
      left_join(gdidata::countrynames, by = c("from" = "iso3")) |>
      rename(region_from = .data$iom_region) |>
      
      # Determine whether flow is intra- or inter-regional
      mutate(region = case_when(
        is.na(.data$region_geo) | is.na(.data$region_from) ~ "Unknown",
        .data$region_geo == .data$region_from ~ "Within region",
        .default = "Outside region"
      )) |>
      summarise(
        n = sum(.data$n),
        .by = c(.data$geo, .data$from, .data$region, .data$t)
      )
    
    emig <- dataset |>
      filter(.data$from == iso) |>
      mutate(panel = panel_dest) |>
      rename(nat = .data$geo, iso = .data$from)
    
    immig <- dataset |>
      filter(.data$geo == iso) |>
      mutate(panel = panel_orig) |>
      rename(iso = .data$geo, nat = .data$from)
    
    df <- bind_rows(emig, immig) |>
      group_by(.data$panel) |>
      arrange(desc(.data$n), .by_group = TRUE) |>
      ungroup()
    
    if (nrow(df) > 0) {
      
      destin <- filter(df, .data$panel == panel_dest) |>
        slice_head(n = topn) |>
        pull(.data$nat)
      
      origin <- filter(df, .data$panel == panel_orig) |>
        slice_head(n = topn) |>
        pull(.data$nat)
      
      destin_order <- gdidata::countryname(destin, to = "name_text") |> rev()
      origin_order <- gdidata::countryname(origin, to = "name_text") |> rev()
      
      df_destin1 <- filter(df, .data$panel == panel_dest) |>
        mutate(country = case_when(
          .data$nat %in% destin ~
            gdidata::countryname(.data$nat, from = "iso3", to = "name_text"),
          .default = "Others"
        )) |>
        mutate(region = ifelse(
          .data$country == "Others",
          "Others",
          .data$region
        )) |>
        summarise(
          n = sum(.data$n),
          .by = c(.data$panel, .data$country, .data$region)
        )
      
      df_destin <- df_destin1 |>
        arrange(match(.data$country, c("Others", destin_order)))
      df_destin_print <- df_destin1 |>
        arrange(match(.data$country, c(rev(destin_order), "Others"))) |>
        rename(Destination = .data$country)
      
      df_origin1 <- filter(df, .data$panel == panel_orig) |>
        mutate(country = case_when(
          .data$nat %in% origin ~
            gdidata::countryname(.data$nat, from = "iso3", to = "name_text"),
          .default = "Others"
        )) |>
        mutate(region = ifelse(
          .data$country == "Others",
          "Others",
          .data$region
        )) |>
        summarise(
          n = sum(.data$n),
          .by = c(.data$panel, .data$country, .data$region)
        )
      
      df_origin <- df_origin1 |>
        arrange(match(.data$country, c("Others", origin_order)))
      df_origin_print <- df_origin1 |>
        arrange(match(.data$country, c(rev(origin_order), "Others"))) |>
        rename(Origin = .data$country)
      
      output$data <- bind_rows(df_destin, df_origin) |>
        mutate(share = 100 * .data$n / sum(.data$n), .by = .data$panel)
      
      output$print <- bind_rows(df_destin_print, df_origin_print) |>
        mutate(
          Country = gdidata::countryname(iso, to = "name_text"),
          share = .data$n / sum(.data$n), .by = .data$panel
        ) |>
        select(
          .data$Country,
          .data$Destination,
          .data$Origin,
          Region = .data$region,
          Count = .data$n,
          Share = .data$share
        )
    }
    
    output$range <- max(dataset$t)
  }
  
  
  # Net migration -----------------------------------------------------------
  
  if (key == "nmig") {
    
    output$data <- filter(gdidata::wdi, .data$var == "nmig") |>
      rename(n = .data$v)
    
    output$range <- ranger(output$data)
    
    # pop <- filter(gdidata::wdi, .data$var == "pop") |>
    #   rename(pop = .data$v) |>
    #   select(-.data$var)
    # data <- inner_join(nmig, pop, by = c("geo", "t")) |>
    #   mutate(v = 1000 * .data$nmig / .data$pop)
    
    if (!is.null(iso)) {
      
      output$data <- filter(output$data, .data$geo == iso)
      
      if (nrow(output$data) > 0) {
        output$print <- output$data |>
          mutate(country = gdidata::countryname(.data$geo, to = "name_text")) |>
          select(
            Country = .data$country,
            Year = .data$t,
            `Net migration` = .data$n,
          )
      }
    }
  }
  
  
  # IDP ---------------------------------------------------------------------
  
  if (key == "idp") {
    
    causes <- c("Environmental impacts", "Conflict and violence")
    
    output$data <- filter(gdidata::idmc_flows, .data$geo == iso) |>
      mutate(cause = ifelse(
        .data$cause == "conflict",
        causes[2],
        causes[1]
      )) |>
      summarise(
        n = sum(.data$n),
        .by = c(.data$geo, .data$t, .data$cause)
      )
    
    if (nrow(output$data) > 0) {
      
      output$print <- output$data |>
        mutate(Country = gdidata::countryname(.data$geo, to = "name_sort")) |>
        select(.data$Country, Year = .data$t, .data$cause, .data$n) |>
        pivot_wider(names_from = .data$cause, values_from = .data$n) |>
        arrange(.data$Year)
    }
    
    output$range <- ranger(gdidata::idmc_flows)
  }
  
  if (key == "idcause") {
    
    output$data <- gdidata::idmc_flows |> 
      filter(.data$t >= max(.data$t) - 9, .data$cause != "conflict") |> 
      summarise(n = sum(.data$n), .by = c(.data$geo, .data$cause)) |> 
      group_by(.data$geo) |> 
      arrange(desc(.data$n), by_group = TRUE) |> 
      mutate(
        rank = 1:n(),
        cause = ifelse(
          .data$rank >= 4,
          "Others",
          paste0(
            toupper(substr(.data$cause, 1, 1)), 
            substr(cause, 2, nchar(.data$cause))
          ))
      ) |> 
      ungroup() |> 
      summarise(n = sum(.data$n), .by = c(.data$geo, .data$cause)) |> 
      mutate(v = .data$n / sum(.data$n), .by = .data$geo) |> 
      arrange(.data$geo, desc(.data$n))
    
    if (!is.null(iso)) {
      
      output$data <- filter(output$data, .data$geo == iso)
    }
    
    output$range <- c(
      max(gdidata::idmc_flows$t) - 9,
      max(gdidata::idmc_flows$t)
    )
  }
  
  
  # Missing migrants --------------------------------------------------------
  
  if (key == "mmp") {
    
    causes <- c(
      "Drowning",
      "Transport hazards",
      "Harsh conditions",
      "Accidental",
      "Sickness",
      "Violence",
      "Mixed or unknown"
    )
    
    data_full <- gdidata::iom_mmp |>
      mutate(t = lubridate::year(.data$t)) |>
      summarise(n = sum(.data$dead), .by = .data$t)
    
    output$data <- filter(gdidata::iom_mmp, .data$geo == iso) |>
      mutate(
        t = lubridate::year(t),
        cause = case_when(
          str_detect(.data$cause, "Accidental") ~ "Accidental",
          str_detect(.data$cause, "Harsh")      ~ "Harsh conditions",
          str_detect(.data$cause, "Sickness")   ~ "Sickness",
          str_detect(.data$cause, "transport")  ~ "Transport hazards",
          .default = .data$cause
        )
      ) |>
      summarise(
        n = sum(.data$dead),
        .by = c(.data$geo, .data$t, .data$cause)
      ) |>
      drop_na()
    
    if (nrow(output$data) > 0) {
      
      agg <- output$data |>
        summarise(n = sum(.data$n), .by = .data$t) |>
        mutate(geo = iso, cause = "Total")
      
      output$print <- bind_rows(output$data, agg) |>
        mutate(Country = gdidata::countryname(.data$geo, to = "name_text")) |>
        select(.data$Country, Cause = .data$cause, .data$t, .data$n) |>
        arrange(.data$t) |>
        pivot_wider(names_from = .data$t, values_from = .data$n) |>
        arrange(match(.data$Cause, c("Total", causes)))
    }
    
    output$range <- ranger(data_full)
  }
  
  
  # Refugees ----------------------------------------------------------------
  
  if (key == "refug") {
    
    panel_orig <- paste0(
      "Refugees from ", name, " and where they are hosted"
    )
    panel_host <- paste0(
      "Refugees hosted in ", name, " and where they come from"
    )
    
    years <- c(seq(1990, 2020, 5), 2021:max(gdidata::unhcr$t))
    
    dataset <- gdidata::unhcr |>
      filter(
        (.data$t %in% years | .data$t == max(.data$t)),
        .data$group == "refugee"
      )
    
    # check <- filter(dataset, t == max(t), .by = geo) |>
    #   group_by(geo) |>
    #   arrange(desc(n), by_group = TRUE) |>
    #   slice_head(n = 3) |>
    #   ungroup()
    
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
      
      orig <- filter(dataset, .data$from == iso)
      
      if (nrow(orig) > 0) {
        
        orig_top_t1 <- filter(orig, .data$t == max(t)) |>
          arrange(desc(.data$n)) |>
          slice_head(n = 3)
        orig_top_names <- gdidata::countryname(orig_top_t1$geo, to = "name_text")
        orig <- orig |>
          mutate(
            nat = ifelse(.data$geo %in% orig_top_t1$geo, .data$geo, "Others"),
            nat_name = ifelse(
              .data$nat == "Others",
              .data$nat, 
              countryname(.data$nat, to = "name_text") |> suppressWarnings()
            )
          ) |>
          summarise(
            n = sum(.data$n), 
            .by = c(.data$nat, .data$nat_name, .data$t)
          ) |>
          mutate(panel = "orig") |>
          complete(
            t = years,
            tidyr::nesting(nat, nat_name, panel),
            fill = list(n = 0)
          )
        
      } else orig_top_names <- ""

      host <- filter(dataset, .data$geo == iso)
      
      if (nrow(host) > 0) {

        host_without_unknown <- filter(host, .data$from != "OOO")
        host_top_t1 <- filter(host_without_unknown, .data$t == max(t)) |>
          arrange(desc(.data$n)) |>
          slice_head(n = 3)
        host_top_names <- gdidata::countryname(host_top_t1$from, to = "name_text")
        host <- host |>
          mutate(
            nat = case_when(
              .data$from %in% c(host_top_t1$from, "OOO") ~ .data$from, 
              .default = "Others"
            ),
            nat_name = ifelse(
              .data$nat == "Others",
              .data$nat, 
              countryname(.data$nat, to = "name_text") |> suppressWarnings()
            )
          ) |>
          summarise(
            n = sum(.data$n), 
            .by = c(.data$nat, .data$nat_name, .data$t)
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
          mutate(nat_name = factor(.data$nat_name, levels = c(
            unique(c(orig_top_names, host_top_names)),
            "Unknown", "Others"
          )))
        
        agg <- output$data |>
          summarise(n = sum(.data$n), .by = c(.data$t, .data$panel)) |>
          mutate(nat_name = "Total")
        
        # output$print <- bind_rows(agg, output$data) |>
        #   mutate(Panel = ifelse(.data$panel == "orig", panel_orig, panel_host)) |>
        #   select(.data$Panel, .data$nat_name, Year = .data$t, .data$n) |>
        #   arrange(.data$Panel,.data$Year) |>
        #   pivot_wider(names_from = .data$nat_name, values_from = .data$n) |>
        #   select(
        #     "Panel", "Year", "Total",
        #     all_of(c(orig_top_names, host_top_names)),
        #     "Others"
        #   )
      }
    }
    
    output$range <- ranger(gdidata::unhcr)
  }
  
  
  # Remittances -------------------------------------------------------------
  
  if (key == "remt") {
    
    remt <- filter(gdidata::wdi, .data$var %in% c("remin", "remout"))
    output$data <- filter(remt, .data$geo == iso & .data$v > 0) |>
      rename(n = .data$v)
    
    if (nrow(output$data) > 0) {
      
      vars <- c("Remittances received", "Remittances paid")
      
      output$print <- output$data |>
        mutate(
          Country = gdidata::countryname(iso, to = "name_text"),
          var = ifelse(.data$var == "remin", vars[1], vars[2])
        ) |>
        select(.data$Country, Year = .data$t, .data$var, .data$n) |>
        complete(
          .data$Country,
          .data$Year,
          var = vars,
          fill = list(n = NA)
        ) |>
        pivot_wider(names_from = .data$var, values_from = .data$n) |>
        arrange(.data$Year)
    }
    
    output$range <- ranger(remt)
  }
  
  
  # FDI ---------------------------------------------------------------------

  if (key == "fdi") {
    
    fdi <- filter(gdidata::wdi, .data$var %in% c("fdiin", "fdiout"))
    output$data <- filter(fdi, .data$geo == iso & .data$v > 0) |>
      rename(n = .data$v)
    
    if (nrow(output$data) > 0) {
      
      vars <- c("FDI inflow", "FDI outflow")
      
      output$print <- output$data |>
        mutate(
          Country = gdidata::countryname(iso, to = "name_text"),
          var = ifelse(.data$var == "fdiin", vars[1], vars[2])
        ) |>
        select(.data$Country, Year = .data$t, .data$var, .data$n) |>
        complete(
          .data$Country,
          .data$Year,
          var = vars,
          fill = list(n = NA)
        ) |>
        pivot_wider(names_from = .data$var, values_from = .data$n) |>
        arrange(.data$Year)
    }
    
    output$range <- ranger(fdi)
  }
  

  # Sex and age pyramid -----------------------------------------------------
  
  if (key == "pyr") {
    
    output$data <- gdidata::undesa_wpp |> 
      filter(.data$geo == iso, .data$t == max(.data$t)) |> 
      mutate(age = case_when(
        .data$age %in% 0:4              ~ "0-4", 
        .data$age %in% 5:9              ~ "5-9", 
        .data$age %in% 10:14            ~ "10-14", 
        .data$age %in% 15:19            ~ "15-19", 
        .data$age %in% 20:24            ~ "20-24", 
        .data$age %in% 25:29            ~ "25-29", 
        .data$age %in% 30:34            ~ "30-34", 
        .data$age %in% 35:39            ~ "35-39", 
        .data$age %in% 40:44            ~ "40-44", 
        .data$age %in% 45:49            ~ "45-49", 
        .data$age %in% 50:54            ~ "50-54", 
        .data$age %in% 55:59            ~ "55-59", 
        .data$age %in% 60:64            ~ "60-64", 
        .data$age %in% 65:69            ~ "65-69", 
        .data$age %in% 70:74            ~ "70-74", 
        .default = "75+" 
      )) |> 
      summarise(
        n = sum(.data$n), 
        .by = c(.data$geo, .data$t, .data$sex, .data$age)
      ) |> 
      mutate(
        v = 100 * .data$n / sum(.data$n),
        v = ifelse(sex == "male", -.data$v, .data$v),
        sex = factor(.data$sex, levels = c("male", "female"))
      )
    
    output$range <- max(gdidata::undesa_wpp$t)
  }
  

  # Migrant sex and age pyramid ---------------------------------------------

  if (key == "migpyr") {
    
    dataset <- bind_rows(
      readr::read_csv("data-raw/Emigrants_AgeSex.csv") |> 
        mutate(panel = "Emigrants"),
      readr::read_csv("data-raw/Immigrants_AgeSex.csv") |> 
        mutate(panel = "Immigrants")
    ) |> 
      select(
        .data$panel, 
        geo = .data$Country, 
        t = .data$Year, 
        sex = .data$Sex, 
        age = .data$Age, 
        n = .data$Value
      )
    
    output$data <- dataset |> 
      filter(.data$geo == iso, .data$sex != "total", .data$t == max(.data$t)) |> 
      mutate(
        sex = factor(.data$sex, levels = c("male", "female")),
        v = 100 * .data$n / sum(.data$n), 
        .by = .data$panel
      )
    
    output$range <- max(dataset$t)
  }
  

  # Population --------------------------------------------------------------

  if (key %in% c("pop", "birth", "depend", "income", "unem", "inf")) {
   
    dataset <- filter(gdidata::wdi, .data$var == key)
    dataset_iso <- filter(dataset, .data$geo == iso)
    
    if (nrow(dataset_iso) == 0) {
      
      output$data <- dataset_iso
      
    } else {
      
      output$data <- bind_rows(
        dataset_iso |> 
          mutate(var = countryname(iso, to = "name_text")),
        dataset |> 
          summarise(v = median(.data$v, na.rm = TRUE), .by = .data$t) |> 
          mutate(var = "World median")
      ) |> 
        mutate(var = forcats::fct_relevel(
          .data$var, 
          "World median", 
          after = 1
        )) |> 
        rename(n = .data$v) |> 
        complete(t = min(dataset$t):max(dataset$t))
    }
    
    output$range <- c(min(dataset$t), max(dataset$t))
  }
  

  return(output)
}

