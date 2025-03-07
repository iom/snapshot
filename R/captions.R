
get_pct <- function(vector, value) {
  pct <- 1 - ecdf(vector)(value)
  
  t10 <- '#box(baseline: .5pt, width: 5.5pt, image("inst/top10.png"))'
  t33 <- '#box(baseline: .5pt, width: 5.5pt, image("inst/top33.png"))'
  m33 <- '#box(baseline: .5pt, width: 5.5pt, image("inst/mid33.png"))'
  b33 <- '#box(baseline: .5pt, width: 5.5pt, image("inst/bot33.png"))'
  b10 <- '#box(baseline: .5pt, width: 5.5pt, image("inst/bot10.png"))'
  
  if (pct <= .1) return(t10)
  if (pct > .1 & pct <= 1/3) return(t33)
  if (pct > 1/3 & pct < 2/3) return(m33)
  if (pct >= 2/3 & pct < .9) return(b33)
  if (pct >= .9) return(b10)
}



pl <- function(num) prettylabel(num, signif = 2, spell = TRUE)
pl1 <- function(num) prettylabel(num, signif = 2, spell = TRUE)
pl_usd <- function(num) prettylabel(num, spell = TRUE, currency = "\\$")
pl_pct <- function(num) prettylabel(num, signif = 2, pct = TRUE)
pl_pct1 <- function(num) prettylabel(num, signif = 2, pct = TRUE)


# Migrant stocks ----------------------------------------------------------

caption_stocks <- function(iso) {
  
  name <- namer(iso)
  
  migrants <- paste(
    "Migration here refers to permanent residence outside of one's place of",
    "birth. It includes refugees and asylum-seekers but not irregular migrants. "
  )
  
  if (iso == "XKX") {
      
    caption <- paste0(
      "#caption1[",
      migrants,
      "For statistical purposes, UN DESA includes ", name, " in Serbia.",
      "]"
    )
    
  } else if (iso == "SDN") {
    
    caption <- paste0(
      "#caption3[",
      "- ", migrants, "\n",
      "- For statistical purposes, immigrant data during 1990\u20132005 ",
      "covers Sudan and South Sudan.\n",
      "#colbreak()\n",
      caption_stocks_emig(iso), "\n",
      "#colbreak()\n",
      caption_stocks_immig(iso),
      "]"
    )
    
  } else if (iso == "PSE") {
    
    caption <- paste0(
      "#caption3[",
      "- ", migrants, "\n",
      "- For statistical purposes, ", name, " includes East Jerusalem. ",
      "Moreover, its foreign-born migrant stock does not include refugees.\n", 
      "#colbreak()\n",
      caption_stocks_emig(iso), "\n",
      "#colbreak()\n",
      caption_stocks_immig(iso),
      "]"
    )
    
  } else {
    
    caption <- paste0(
      "#caption3[",
      "- ", migrants, "\n",
      "#colbreak()\n",
      caption_stocks_emig(iso), "\n",
      "#colbreak()\n",
      caption_stocks_immig(iso),
      "]"
    )
  }
  
  return(caption)
}

caption_stocks_emig <- function(iso) {
  
  name <- namer(iso)
  region <- filter(gdidata::countrynames, iso3 == iso)$iom_region
  
  data <- plot_data("stocks", use_2020 = TRUE)$data |> 
    filter(panel == "emig")
  data_iso <- plot_data("stocks", iso = iso, use_2020 = TRUE)$data |> 
    filter(panel == "emig")
  
  t0 <- min(data_iso$t)
  t1 <- max(data_iso$t)
  
  data_t0 <- filter(data, t == t0) |> summarise(n = sum(n), .by = geo)
  data_t1 <- filter(data, t == t1) |> summarise(n = sum(n), .by = geo)
  
  data_iso_t0 <- filter(data_iso, t == t0) |> 
    summarise(n = sum(n), .by = geo) |> pull(n)
  data_iso_t1 <- filter(data_iso, t == t1) |> 
    summarise(n = sum(n), .by = geo) |> pull(n)
  
  # Shares of population
  pop <- filter(gdidata::wdi, var == "pop") |> rename(pop = v)
  if (nrow(filter(pop, t == t1)) == 0) {
    pop_t1 <- filter(pop, t == max(t))
  } else {
    pop_t1 <- filter(pop, t == t1)
  }
  share_t1 <- left_join(data_t1, pop_t1, by = "geo") |> 
    mutate(v = 100 * n / pop) |> 
    drop_na()
  share_iso_t1 <- filter(share_t1, geo == iso) |> pull(v)
  
  # CAGR from earliest to latest
  change <- ((data_t1$n / data_t0$n)^(1 / (t1 - t0)) - 1)
  change_iso <- ((data_iso_t1 / data_iso_t0)^(1 / (t1 - t0)) - 1)
  change_iso_lab <- pl_pct1(change_iso)
  if (change_iso > 0) change_iso_lab <- paste0("+", pl_pct1(change_iso))

  # Region
  data_reg <- filter(data, t == t1) |> 
    mutate(v = 100 * n / sum(n), .by = geo)
  within <- filter(data_reg, region == "Within region")$v
  outside <- filter(data_reg, region == "Outside region")$v
  within_iso <- filter(data_reg, geo == iso, region == "Within region")$v
  outside_iso <- filter(data_reg, geo == iso, region == "Outside region")$v
  
  caption <- str_glue(
    "- By UN DESA estimates, emigrants from {name} ",
    'numbered #b[{ pl(data_iso_t1) }] { get_pct(data_t1$n, data_iso_t1) } ',
    "as of {t1}, equivalent to #b[{ pl_pct(share_iso_t1) }] ",
    "{ get_pct(share_t1$v, share_iso_t1) } of its population. This was a ",
    "#b[{ change_iso_lab }] { get_pct(change, change_iso) } change per year ",
    "over {t0}–{t1}.\n",
    # "#colbreak()\n",
    "- #b[{ pl_pct(within_iso) }] { get_pct(within, within_iso) } of ",
    "emigrants remained within the {region} region while ",
    "#b[{ pl_pct(outside_iso) }] { get_pct(outside, outside_iso) } ",
    "emigrated outside the region."
  )
  
  return(caption)
}

caption_stocks_immig <- function(iso) {
  
  name <- namer(iso)
  region <- filter(gdidata::countrynames, iso3 == iso)$iom_region
  data <- plot_data("stocks", use_2020 = TRUE)$data |> 
    filter(panel == "immig")
  data_iso <- plot_data("stocks", iso = iso, use_2020 = TRUE)$data |> 
    filter(panel == "immig", n > 0)
  
  t0 <- min(data_iso$t)
  t1 <- max(data_iso$t)
  
  data_t0 <- filter(data, t == t0) |> summarise(n = sum(n), .by = geo)
  data_t1 <- filter(data, t == t1) |> summarise(n = sum(n), .by = geo)
  
  data_iso_t0 <- filter(data_iso, t == t0) |> 
    summarise(n = sum(n), .by = geo) |> pull(n)
  data_iso_t1 <- filter(data_iso, t == t1) |> 
    summarise(n = sum(n), .by = geo) |> pull(n)
  
  # Shares of population
  pop <- filter(gdidata::wdi, var == "pop") |> rename(pop = v)
  if (nrow(filter(pop, t == t1)) == 0) {
    pop_t1 <- filter(pop, t == max(t))
  } else {
    pop_t1 <- filter(pop, t == t1)
  }
  share_t1 <- left_join(data_t1, pop_t1, by = "geo") |> 
    mutate(v = 100 * n / pop) |> 
    drop_na()
  share_iso_t1 <- filter(share_t1, geo == iso) |> pull(v)
  
  # CAGR from earliest to latest
  change <- ((data_t1$n / data_t0$n)^(1 / (t1 - t0)) - 1)
  change <- change[!is.infinite(change)]
  change_iso <- ((data_iso_t1 / data_iso_t0)^(1 / (t1 - t0)) - 1)
  change_iso_lab <- pl_pct1(change_iso)
  if (change_iso > 0) change_iso_lab <- paste0("+", pl_pct1(change_iso))
  
  # Region
  data_reg <- filter(data, t == t1) |> 
    mutate(v = 100 * n / sum(n), .by = geo)
  within <- filter(data_reg, region == "Within region")$v
  outside <- filter(data_reg, region == "Outside region")$v
  within_iso <- filter(data_reg, geo == iso, region == "Within region")$v
  outside_iso <- filter(data_reg, geo == iso, region == "Outside region")$v
  
  caption <- str_glue(paste(
    "- Immigrants in {name} numbered",
    "#b[{pl(data_iso_t1)}] {get_pct(data_t1$n, data_iso_t1)} as of {t1},", 
    "equivalent to #b[{pl_pct(share_iso_t1)}]",
    "{get_pct(share_t1$v, share_iso_t1)} of its population. This was a", 
    "#b[{change_iso_lab}] {get_pct(change, change_iso)} change per year over",
    "{t0}–{t1}.\n",
    "- #b[{pl_pct(within_iso)}] {get_pct(within, within_iso)} of immigrants",
    "came from within the region while #b[{pl_pct(outside_iso)}]",
    "{get_pct(outside, outside_iso)} immigrated from outside the region."
  ))
  
  return(caption)
}


# Net migration -----------------------------------------------------------

caption_nmig <- function(iso) {
  
  name <- namer(iso)
  
  data <- plot_data("nmig")$data
  # pop <- filter(gdidata::wdi, var == "pop") |> rename(pop = v) |> select(-var)
  # data <- full_join(nmig, pop, by = c("geo", "t")) |> 
  #   mutate(v = 1000 * nmig / pop)
  data_iso <- plot_data("nmig", iso)$data
  
  map <- paste(
    "In the map below, net migration is expressed relative to the local",
    "population. Blue represents net in-migration per capita while",
    "red represents net out-migration per capita. Because values",
    "are relative, the dominance of one color or the other",
    "does not necessarily signify the aggregate level of net migration."
  )
  
  if (nrow(drop_na(data_iso, .data$n)) == 0) {
    
    caption <- str_glue(paste(
      "The World Bank has no information on net migration for {name}.",
      "#colbreak()\n",
      "{map}"
    ))
    
  } else {
    
    t1 <- max(data_iso$t) |> as.numeric()
    t0 <- t1 - 9
    
    # tot <- filter(data, t >= t0) |> 
    #   summarise(nmig = sum(nmig), .by = geo) |> 
    #   pull(nmig)
    # tot_iso <- filter(data_iso, geo == iso & t >= t0) |> 
    #   summarise(nmig = sum(nmig), .by = geo) |> 
    #   pull(nmig)
    
    min <- min(data_iso$n)
    max <- max(data_iso$n)
    
    avg <- filter(data, t >= t0) |> 
      summarise(n = mean(n), .by = geo) |> 
      pull(n)
    avg_iso <- filter(data, geo == iso & t >= t0) |> 
      summarise(n = mean(n), .by = geo) |> 
      pull(n)
    sign <- ifelse(avg_iso > 0, "+", "")
    
    # verb <- "entered"
    # if (tot_iso < 0) verb <- "left"
    
    caption <- str_glue(paste(
      "- Annual net migration in {name} has ranged between #b[{ pl(min) }] and",
      "#b[{ pl(max) }] over {t0}\u2013{t1} (positive values mean more people",
      "are immigrating than emigrating; negative values mean the opposite).",
      "The average annual net migration during that period was",
      "#b[{ sign }{ pl(avg_iso) }] { get_pct(avg, avg_iso) }.\n",
      "#colbreak()\n",
      "- {map}"
    ))
  }
  
  return(caption)
}


# Age structure of migrants -----------------------------------------------

caption_migpyr <- function(iso) {
  
  name <- namer(iso)
  
  data_all <- bind_rows(
    readr::read_csv("data-raw/Emigrants_AgeSex.csv") |> 
      mutate(panel = "Emigrants"),
    readr::read_csv("data-raw/Immigrants_AgeSex.csv") |> 
      mutate(panel = "Immigrants")
  ) |> 
    select(panel, geo = Country, t = Year, sex = Sex, age = Age, n = Value) |> 
    filter(t == max(t)) |> 
    mutate(
      age = case_when(
        age %in% c("0-4", "5-9", "10-14") ~ "children",
        age %in% c("65-69", "70-74", "75+") ~ "elderly",
        .default = "working"
      )
    ) |> 
    summarise(n = sum(n), .by = c(panel, geo, t, sex, age))
  
  data <- filter(data_all, sex == "total") |> 
    mutate(v = 100 * n / sum(n), .by = c(panel, geo, t))
  
  data_iso <- filter(data, geo == iso)
  
  if (nrow(drop_na(data_iso)) == 0) {
    
    caption <- str_glue(
      "UN DESA has no information on the sex and age of migrants from {name}."
    )
    
  } else {
    
    t1 <- data$t[1]
    
    data_f <- data_all |> 
      summarise(n = sum(n), .by = c(panel, geo, sex)) |> 
      pivot_wider(names_from = sex, values_from = n) |> 
      mutate(v = 100 * female / total)
    
    # Emigrants
    
    data_emig_child <- filter(data, panel == "Emigrants" & age == "children")
    data_emig_work <- filter(data, panel == "Emigrants" & age == "working")
    data_emig_elder <- filter(data, panel == "Emigrants" & age == "elderly")
    data_emig_child_iso <- data_emig_child$v[data_emig_child$geo == iso]
    data_emig_work_iso <- data_emig_work$v[data_emig_work$geo == iso]
    data_emig_elder_iso <- data_emig_elder$v[data_emig_elder$geo == iso]
    
    data_f_emig <- filter(data_f, panel == "Emigrants")
    data_f_emig_iso <- data_f_emig$v[data_f_emig$geo == iso]
    
    caption_emig <- str_glue(paste(
      " - UN DESA has no information on the sex and age of emigrants from",
      "{name}."
    ))
    if (!is.na(data_emig_child_iso)) {
      caption_emig <- str_glue(paste(
        " - Among emigrants from {name}, IOM estimates from UN DESA data that",
        "#b[{pl_pct(data_emig_child_iso)}]",
        "{get_pct(data_emig_child$v, data_emig_child_iso)} were children (0-14),",
        "#b[{pl_pct(data_emig_work_iso)}]",
        "{get_pct(data_emig_work$v, data_emig_work_iso) } were working age",
        "(15-64), and #b[{pl_pct(data_emig_elder_iso)}]",
        "{get_pct(data_emig_elder$v, data_emig_elder_iso)} were elderly (65+) in",
        "{t1}.\n",
        " - #b[{pl_pct(data_f_emig_iso)}]",
        "{get_pct(data_f_emig$v, data_f_emig_iso)} of emigrants were female in",
        "{t1}."
      ))
    }
    
    # Immigrants
    
    data_immig_child <- filter(data, panel == "Immigrants" & age == "children")
    data_immig_work <- filter(data, panel == "Immigrants" & age == "working")
    data_immig_elder <- filter(data, panel == "Immigrants" & age == "elderly")
    data_immig_elder <- filter(data, panel == "Immigrants" & age == "elderly")
    data_immig_child_iso <- data_immig_child$v[data_immig_child$geo == iso]
    data_immig_work_iso <- data_immig_work$v[data_immig_work$geo == iso]
    data_immig_elder_iso <- data_immig_elder$v[data_immig_elder$geo == iso]
    
    data_f_immig <- filter(data_f, panel == "Immigrants")
    data_f_immig_iso <- data_f_immig$v[data_f_immig$geo == iso]
    
    caption_immig <- str_glue(paste(
      " - UN DESA has no information on the sex and age of immigrants from",
      "{name}."
    ))
    if (!is.na(data_immig_child_iso)) {
      caption_immig <- str_glue(paste(
        " - Among immigrants to {name}, UN DESA reports that",
        "#b[{pl_pct(data_immig_child_iso)}]",
        "{get_pct(data_immig_child$v, data_immig_child_iso)} were children",
        "(0-14), #b[{pl_pct(data_immig_work_iso)}]",
        "{get_pct(data_immig_work$v, data_immig_work_iso)} were working age",
        "(15-64), and #b[{ pl_pct(data_immig_elder_iso) }]",
        "{get_pct(data_immig_elder$v, data_immig_elder_iso)} were elderly (65+)",
        "in {t1}.\n",
        " - #b[{pl_pct(data_f_immig_iso)}]",
        "{get_pct(data_f_immig$v, data_f_immig_iso)} of immigrants were female",
        "in {t1}."
      ))
    }
    
    
    caption <- str_glue("{caption_emig}\n", "#colbreak()\n", "{caption_immig}")
  }
  
  return(caption)
}


# Refugees ----------------------------------------------------------------

caption_refug_orig <- function(iso) {
  
  name <- namer(iso)
  
  data <- left_join(gdidata::unhcr, gdidata::countrynames, by = c("geo" = "iso3")) |> 
    filter(group == "refugee") |> 
    summarise(n = sum(n), .by = c(geo, t, from, region))
  data_iso <- filter(data, from == iso) |> 
    summarise(n = sum(n), .by = c(from, t))

  if (nrow(data_iso) == 0) {
    
    caption <- str_glue(
      " - UNHCR has no information on refugees originating from {name}.\n\n"
    )
    
  } else {
    
    data_iso <- data_iso |> 
      complete(t = seq(min(data_iso$t), max(data_iso$t)), fill = list(n = 0))

    t1 <- max(data_iso$t)
    t0 <- t1 - 1
    
    data_t0t1 <- filter(gdidata::unhcr, group == "refugee" & t %in% c(t0, t1)) |> 
      summarise(n = sum(n), .by = c(from, t)) |> 
      mutate(t = case_when(t == t0 ~ "col_t0", t == t1 ~ "col_t1")) |> 
      pivot_wider(names_from = t, values_from = n) |> 
      drop_na() |> 
      mutate(change = 100 * ((col_t1 / col_t0)^(1 / (t1 - t0)) - 1))
    
    data_t0 <- filter(gdidata::unhcr, group == "refugee" & t == t0) |> 
      summarise(n = sum(n), .by = from)
    data_t1 <- filter(gdidata::unhcr, group == "refugee" & t == t1) |> 
      summarise(n = sum(n), .by = from)
    
    data_iso_t0 <- filter(data_iso, t == t0) |> pull(n)
    data_iso_t1 <- filter(data_iso, t == t1) |> pull(n)
    
    # Shares of population
    pop_t1 <- filter(gdidata::wdi, var == "pop" & t == t1) |> rename(pop = v)
    share_t1 <- left_join(data_t1, pop_t1, by = c("from" = "geo")) |> 
      mutate(v = 100 * n / pop) |> 
      drop_na()
    share_iso_t1 <- filter(share_t1, from == iso) |> pull(v)
    
    # 1 year change 
    change_text <- ""
    if (nrow(filter(data_iso, t == t0)) > 0) {
      change <- data_t0t1$change
      change_iso <- 100 * ((data_iso_t1 / data_iso_t0)^(1 / (t1 - t0)) - 1)
      change_iso_lab <- pl_pct1(change_iso)
      if (change_iso > 0) change_iso_lab <- paste0("+", pl_pct1(change_iso))
      change_text <- str_glue(paste(
        "This was a #b[{change_iso_lab}] {get_pct(change, change_iso)} change",
        "from the previous year."
      ))
    }
    
    if (length(data_iso_t0) > 0) {
      if (data_iso_t1 > 0 & data_iso_t0 == 0) {
        change_text <- "There were no recorded refugees the previous year."
      }
    }
    
    # Top host countries
    
    host_iso <- plot_data("refug", iso)$data |> 
      filter(.data$panel == "orig", .data$t == t1) |> 
      mutate(v = 100 * .data$n / sum(.data$n)) |> 
      arrange(desc(.data$n))
    
    hosts <- filter(host_iso, .data$nat != "Others")
    others <- filter(host_iso, .data$nat == "Others")
    
    if (nrow(hosts == 1)) {
      hosts_text <- str_glue(
        "All were hosted in { namer(hosts$nat[1], bold = TRUE) }."
      )
    }
    
    if (nrow(hosts == 2)) {
      hosts_text <- str_glue(
        "{ pl_pct(hosts$v[1]) } were hosted in ",
        "{ namer(hosts$nat[1], bold = TRUE) } while ",
        "{ pl_pct(hosts$v[2]) } were hosted in ",
        "{ namer(hosts$nat[2], bold = TRUE) }."
      )
    }
    
    if (nrow(hosts == 3)) {
      hosts_text <- str_glue(
        "{ pl_pct(hosts$v[1]) } were hosted in ",
        "{ namer(hosts$nat[1], bold = TRUE) }, ",
        "{ pl_pct(hosts$v[2]) } in ",
        "{ namer(hosts$nat[2], bold = TRUE) }, and ",
        "{ pl_pct(hosts$v[3]) } in { namer(hosts$nat[3], bold = TRUE) }."
      )
    }
    
    others_text <- ""
    if (nrow(others) == 1) {
      others_text <- str_glue(
        "The remaining { pl_pct(others$v) } were hosted elsewhere."
      )
    }
    
    caption <- str_glue(
      " - By UNHCR estimates, refugees from {name} numbered ",
      "#b[{pl(data_iso_t1)}] {get_pct(data_t1$n, data_iso_t1)} in {t1}, ",
      "equivalent to #b[{pl_pct(share_iso_t1)}]  ",
      "{get_pct(share_t1$v, share_iso_t1)} of its population. {change_text}\n",
      " - {hosts_text} {others_text}"
    )
  }
  
  return(caption)
}


caption_refug_host <- function(iso) {
  
  name <- namer(iso)
  
  data <- left_join(gdidata::unhcr, gdidata::countrynames, by = c("from" = "iso3")) |> 
    filter(group == "refugee") |> 
    summarise(n = sum(n), .by = c(geo, t, from, region)) |> 
    mutate(region = case_when(is.na(region) ~ "Unknown", .default = region))
  data_iso <- filter(data, geo == iso) |> 
    summarise(n = sum(n), .by = c(geo, t))
  
  if (nrow(data_iso) == 0) {
    
    caption <- str_glue(
      " - UNHCR has no information on refugees hosted in {name}."
    )
    
  } else {
    
    t1 <- max(data_iso$t)
    t0 <- t1 - 1
    
    data_t0t1 <- filter(gdidata::unhcr, group == "refugee" & t %in% c(t0, t1)) |> 
      summarise(n = sum(n), .by = c(geo, t)) |> 
      mutate(t = case_when(t == t0 ~ "col_t0", t == t1 ~ "col_t1")) |> 
      pivot_wider(names_from = t, values_from = n) |> 
      drop_na() |> 
      mutate(change = 100 * ((col_t1 / col_t0)^(1 / (t1 - t0)) - 1))
    
    data_t0 <- filter(gdidata::unhcr, group == "refugee" & t == t0) |> 
      summarise(n = sum(n), .by = geo)
    data_t1 <- filter(gdidata::unhcr, group == "refugee" & t == t1) |> 
      summarise(n = sum(n), .by = geo)

    data_iso_t0 <- filter(data_iso, t == t0) |> pull(n)
    data_iso_t1 <- filter(data_iso, t == t1) |> pull(n)
    
    # Shares of population
    pop_t1 <- filter(gdidata::wdi, var == "pop" & t == t1) |> rename(pop = v)
    share_t1 <- left_join(data_t1, pop_t1, by = "geo") |> 
      mutate(v = 100 * n / pop) |> 
      drop_na()
    share_iso_t1 <- filter(share_t1, geo == iso) |> pull(v)
    
    # 1 year change
    change_text <- ""
    if (nrow(filter(data_iso, t == t0)) > 0) {
      change <- data_t0t1$change
      change_iso <- 100 * ((data_iso_t1 / data_iso_t0)^(1 / (t1 - t0)) - 1)
      change_iso_lab <- pl_pct1(change_iso)
      if (change_iso > 0) change_iso_lab <- paste0("+", pl_pct1(change_iso))
      change_text <- str_glue(paste(
        "This was a #b[{change_iso_lab}] {get_pct(change, change_iso)} change",
        "from the previous year."
      ))
    }
    
    if (length(data_iso_t0) > 0) {
      if (data_iso_t1 > 0 & data_iso_t0 == 0) {
        change_text <- "There were no recorded refugees the previous year."
      }
    }
    
    # Top origin countries
    
    orig_iso <- plot_data("refug", iso)$data |> 
      filter(.data$panel == "host", .data$t == t1) |> 
      mutate(v = 100 * .data$n / sum(.data$n)) |> 
      arrange(desc(.data$n))
    
    origs <- filter(orig_iso, !(.data$nat %in% c("OOO", "Others")))
    unknowns <- filter(orig_iso, .data$nat == "Unknowns")
    others <- filter(orig_iso, .data$nat == "Others")
    
    if (nrow(origs == 1)) {
      origs_text <- str_glue(
        "All came from { namer(origs$nat[1], bold = TRUE) }."
      )
    }
    
    if (nrow(origs == 2)) {
      origs_text <- str_glue(
        "{ pl_pct(origs$v[1]) } came from ",
        "{ namer(origs$nat[1], bold = TRUE) } while ",
        "{ pl_pct(origs$v[2]) } came from { namer(origs$nat[2], bold = TRUE) }."
      )
    }
    
    if (nrow(origs == 3)) {
      origs_text <- str_glue(
        "{ pl_pct(origs$v[1]) } came from ",
        "{ namer(origs$nat[1], bold = TRUE) }, ",
        "{ pl_pct(origs$v[2]) } from ",
        "{ namer(origs$nat[2], bold = TRUE) }, and ",
        "{ pl_pct(origs$v[3]) } from { namer(origs$nat[3], bold = TRUE) }."
      )
    }
    
    unknown_text <- ""
    if (nrow(unknowns) == 1) {
      unknown_text <- str_glue(
        "Another { pl_pct(unknowns$v) } had unknown origins."
      )
    }
    
    others_text <- ""
    if (nrow(others) == 1) {
      others_text <- str_glue(
        "The remaining { pl_pct(others$v) } came from elsewhere."
      )
    }
    
    caption <- str_glue(
      " - Refugees hosted in {name} numbered  #b[{pl(data_iso_t1)}] ",
      "{get_pct(data_t1$n, data_iso_t1)} in {t1},  equivalent to ",
      "#b[{pl_pct(share_iso_t1)}] {get_pct(share_t1$v, share_iso_t1)} of its ",
      "population. {change_text}\n",
      " - {origs_text} {unknown_text} {others_text}"
    )
  }
  
  return(caption)
}


# Internal displacements --------------------------------------------------

caption_idp <- function(iso) {
  
  name <- namer(iso)
  
  data <- filter(gdidata::idmc_flows, t >= max(t) - 9 & n > 0)
  data_iso <- filter(data, geo == iso)
  t0 <- min(data$t) |> as.numeric()
  t1 <- max(data$t) |> as.numeric()
  
  if (nrow(data_iso) == 0) {
    
    caption <- str_glue(
      "#caption1[",
      "Over {t0}\u2013{t1}, IDMC reports no internal displacements in {name}.",
      "]"
    )
    
    return(caption)
  }
  
  data_all <- data |> 
    mutate(cause = ifelse(cause == "conflict", "conflict", "disaster"))
  
  # Totals
  
  agg_all <- data_all |> 
    summarise(n = sum(n), .by = c(geo, cause)) |> 
    complete(geo, cause, fill = list(n = 0))
  
  agg_conflict <- filter(agg_all, cause == "conflict")
  agg_disaster <- filter(agg_all, cause == "disaster")
  agg_iso_conflict <- filter(agg_conflict, geo == iso)$n
  agg_iso_disaster <- filter(agg_disaster, geo == iso)$n
  agg_iso <- agg_iso_conflict + agg_iso_disaster
  
  agg <- summarise(agg_all, n = sum(n), .by = geo)$n
  
  if (agg_iso_conflict == 0 & agg_iso_disaster > 0) {
    
    cause_text <- "All were caused by disasters."
  
  } else if (agg_iso_disaster == 0 & agg_iso_conflict > 0) {
    
    cause_text <- "All were caused by disasters."
    
  } else {
    
    cause_text <- str_glue(
      "Of these, #b[{ pl(agg_iso_conflict) }] (or ",
      "{ pl_pct(100 * agg_iso_conflict / agg_iso) }) were due to conflict and ",
      "#b[{ pl(agg_iso_disaster) }] (or ",
      "{ pl_pct(100 * agg_iso_disaster / agg_iso) }) were due to disasters."
    )
  }
  
  totals_text <- str_glue(
    "IDMC recorded #b[{ pl(agg_iso) }] { get_pct(agg, agg_iso) } internal ",
    "displacements in {name} over 2014\u20132023, equivalent to an annual ",
    "average of { pl(agg_iso / 10) }. {cause_text}"
  )
  
  if (agg_iso_disaster > 0) {
    
    disaster <- data |> 
      filter(cause != "conflict") |> 
      summarise(n = sum(n), .by = c(geo, cause))
    
    disaster_iso <- filter(disaster, geo == iso)
    
    disaster_iso_top3 <- disaster_iso |> 
      arrange(desc(n)) |> 
      slice_head(n = 3) |> 
      mutate(cause_lab = ifelse(
        cause == "volcanic activity",
        "Volcanic activities",
        paste0(cause, "s")
      ))
    
    
    if (nrow(disaster_iso_top3) == 1) {
      
      disaster_top1 <- filter(disaster, cause == disaster_iso_top3$cause[1])
      
      natdis <- str_glue(
        "#b[{disaster_iso_top3$cause_lab[1]}] caused a total of",
        "#b[{pl(disaster_iso_top3$n[1])}]", 
        "{get_pct(disaster_top1$n, disaster_iso_top3$n[1])} displacements."
      )
    }
    
    if (nrow(disaster_iso_top3) == 2) {
      
      disaster_top1 <- filter(disaster, cause == disaster_iso_top3$cause[1])
      disaster_top2 <- filter(disaster, cause == disaster_iso_top3$cause[2])
      
      natdis <- str_glue(paste(
        "#b[{ disaster_iso_top3$cause_lab[1] }] caused a total of",
        "#b[{ pl(disaster_iso_top3$n[1]) }]", 
        "{ get_pct(disaster_top1$n, disaster_iso_top3$n[1]) } displacements and",
        "#b[{ disaster_iso_top3$cause_lab[2] }] caused",
        "#b[{ pl(disaster_iso_top3$n[2]) }]", 
        "{ get_pct(disaster_top2$n, disaster_iso_top3$n[2]) } displacements."
      ))
    }
    
    if (nrow(disaster_iso_top3) == 3) {
      
      disaster_top1 <- filter(disaster, cause == disaster_iso_top3$cause[1])
      disaster_top2 <- filter(disaster, cause == disaster_iso_top3$cause[2])
      disaster_top3 <- filter(disaster, cause == disaster_iso_top3$cause[3])
      
      natdis <- str_glue(
        "#b[{ disaster_iso_top3$cause_lab[1] }] caused a total of ",
        "#b[{ pl(disaster_iso_top3$n[1]) }] ", 
        "{get_pct(disaster_top1$n, disaster_iso_top3$n[1])} displacements, ",
        "#b[{ disaster_iso_top3$cause_lab[2] }] caused ",
        "#b[{ pl(disaster_iso_top3$n[2]) }] ", 
        "{ get_pct(disaster_top2$n, disaster_iso_top3$n[2]) } displacements, ",
        "and #b[{ disaster_iso_top3$cause_lab[3] }] caused ",
        "#b[{ pl(disaster_iso_top3$n[3]) }] ", 
        "{ get_pct(disaster_top3$n, disaster_iso_top3$n[3]) } displacements."
      )
    }
  }
   
  caption <- str_glue(paste(
    "#caption[",
    " - {totals_text}\n",
    "#colbreak()\n",
    " - Among disasters, {natdis}",
    "]"
  ))
  
  return(caption)
}


# Missing migrants --------------------------------------------------------

caption_mmp <- function(iso) {
  
  name <- namer(iso)
  
  data <- summarise(gdidata::iom_mmp, n = sum(dead), .by = geo)
  data_iso <- filter(data, geo == iso)
  
  t0 <- min(gdidata::iom_mmp$t, na.rm = TRUE) |> year()
  t1 <- max(gdidata::iom_mmp$t, na.rm = TRUE) |> year()
  
  if (nrow(data_iso) == 0) {
    
    caption <- str_glue(paste(
      " - IOM recorded no deaths or disappearances of people in the act of",
      "international migration in {name} over {t0}–{t1}.\n",
      "#colbreak()\n",
      " - Note that given severe data limitations, this does not imply that no",
      "migrant deaths occurred in this territory during this period."
    ))
    
  } else {
    
    data_iso_cause <- filter(gdidata::iom_mmp, geo == iso) |> 
      summarise(n = sum(dead), .by = cause) |> 
      arrange(desc(n)) |> 
      mutate(
        cause = case_when(
          str_detect(cause, "Accidental") ~ "accidents",
          str_detect(cause, "Harsh") ~ "harsh conditions",
          str_detect(cause, "Sickness") ~ "sickness",
          str_detect(cause, "transport") ~ "transport hazards",
          .default = tolower(cause)
        ),
        sh = 100 * n / sum(n)
      )
    
    data_iso_mix <- filter(data_iso_cause, str_detect(cause, "mixed"))
    data_iso_cause <- filter(data_iso_cause, !str_detect(cause, "mixed"))
    
    if(nrow(data_iso_cause) > 0) {
      
      data_iso_cause <- data_iso_cause |> 
        mutate(rank = 1:n()) |> 
        filter(rank <= 3)
      
      if (nrow(data_iso_cause) == 1) {
        
        if (data_iso_cause$sh[1] == 100) {
          
          cause <- str_glue(paste(
            "All deaths and disappearances were caused by",
            "#b[{data_iso_cause$cause[1]}]"
          ))
          
        } else {
          
          cause <- str_glue(paste(
            "The top cause of deaths and disappearances was",
            "#b[{data_iso_cause$cause[1]}] ({pl_pct(data_iso_cause$sh[1])})."
          ))
        }
      }
      
      if (nrow(data_iso_cause) == 2) {
        cause <- str_glue(paste(
          "Top causes of deaths and disappearances were",
          "#b[{data_iso_cause$cause[1]}] ({pl_pct(data_iso_cause$sh[1])}) and",
          "#b[{data_iso_cause$cause[2]}] ({pl_pct(data_iso_cause$sh[2])})."
        ))
      }
      
      if (nrow(data_iso_cause) == 3) {
        cause <- str_glue(paste(
          "Top causes of deaths and disappearances were",
          "#b[{data_iso_cause$cause[1]}] ({pl_pct(data_iso_cause$sh[1])}),",
          "#b[{data_iso_cause$cause[2]}] ({pl_pct(data_iso_cause$sh[2])}), and",
          "#b[{data_iso_cause$cause[3]}] ({pl_pct(data_iso_cause$sh[3])})."
        ))
      }
    }
    
    if (nrow(data_iso_mix) == 1) {
      
      if (data_iso_mix$sh == 100) {
        
        cause <- "All deaths and disappearances had mixed or unknown causes."
          
      } else {
        
        cause <- paste(
          cause, 
          str_glue(paste(
            "Some {(pl_pct(data_iso_mix$sh))} of deaths had mixed or unknown",
            "causes."
          )))
      }
    }
    
    caption <- str_glue(paste(
      " - Over {t0}–{t1}, IOM reports that at least #b[{pl(data_iso$n)}]",
      "{get_pct(data$n, data_iso$n)} individuals died or went missing",
      "in the territory of {name} during the act of international migration.\n",
      "#colbreak()\n",
      " - {cause}"
    ))
  }
  
  return(caption)
}


# Remittances -------------------------------------------------------------

caption_remin <- function(iso) {
  
  name <- namer(iso)
  
  data <- filter(gdidata::wdi, var == "remin" & v > 0) |> 
    left_join(filter(gdidata::wdi, var == "gdp"), by = c("geo", "t")) |> 
    rename(remin = v.x, gdp = v.y) |> 
    select(geo, t, remin, gdp)
  
  t1_max <- max(data$t) |> as.numeric()
  
  data_iso_latest <- filter(data, geo == iso & t > t1_max - 10)
  
  if (nrow(data_iso_latest) == 0) {
    
    caption <- str_glue(paste(
      " - The World Bank has no data on inbound remittances for {name} from",
      "the last 10 years."
    ))
    
  } else {
    
    t1_latest <- max(data_iso_latest$t) |> as.numeric()
    
    data_iso <- filter(data, geo == iso) |> 
      drop_na() |> 
      filter(t > max(t) - 5)
    
    t0 <- min(data_iso$t) |> as.numeric()
    t1 <- max(data_iso$t) |> as.numeric()
    
    sh_iso_t0_t1 <- data_iso |> 
      summarise(remin = sum(remin), gdp = sum(gdp)) |> 
      mutate(sh = 100 * remin / gdp) |> 
      pull(sh)
    
    sh_t0_t1 <- filter(data, t >= t0 & t <= t1) |> 
      drop_na() |> 
      summarise(remin = sum(remin), gdp = sum(gdp), .by = geo) |> 
      mutate(sh = 100 * remin / gdp) |> 
      pull(sh)
    
    if (t0 == t1) period <- str_glue("In {t1}")
    else period <- str_glue("Averaged over {t0}–{t1}")
    
    data_t1_latest <- filter(data, t == t1_latest) |> 
      pull(remin)
    data_iso_t1_latest <- filter(data_iso_latest, t == t1_latest) |> 
      pull(remin)

    caption <- str_glue(paste(
      " - The World Bank reports that inbound remittances to {name} amounted",
      "to #b[{pl_usd(data_iso_t1_latest)}]",
      "{get_pct(data_t1_latest, data_iso_t1_latest)} in {t1_latest}. {period},",
      "these were #b[{pl_pct(sh_iso_t0_t1)}] {get_pct(sh_t0_t1, sh_iso_t0_t1)}",
      "of GDP."
    ))
  }
  
  return(caption)
}

caption_remout <- function(iso) {
  
  name <- namer(iso)
  
  data <- filter(gdidata::wdi, var == "remout" & v > 0) |> 
    left_join(filter(gdidata::wdi, var == "gdp"), by = c("geo", "t")) |> 
    rename(remout = v.x, gdp = v.y) |> 
    select(geo, t, remout, gdp)
  
  t1_max <- max(data$t) |> as.numeric()
  
  data_iso_latest <- filter(data, geo == iso & t > t1_max - 10)
  
  if (nrow(data_iso_latest) == 0) {
    
    caption <- str_glue(paste(
      " - The World Bank has no data on outbound remittances for {name} from",
      "the 10 years."
    ))
    
  } else {
    
    t1_latest <- max(data_iso_latest$t) |> as.numeric()
    
    data_iso <- filter(data, geo == iso) |> 
      drop_na() |> 
      filter(t > max(t) - 5)
    
    t0 <- min(data_iso$t) |> as.numeric()
    t1 <- max(data_iso$t) |> as.numeric()
    
    sh_iso_t0_t1 <- data_iso |> 
      summarise(remout = sum(remout), gdp = sum(gdp)) |> 
      mutate(sh = 100 * remout / gdp) |> 
      pull(sh)
    
    sh_t0_t1 <- filter(data, t >= t0 & t <= t1) |> 
      drop_na() |> 
      summarise(remout = sum(remout), gdp = sum(gdp), .by = geo) |> 
      mutate(sh = 100 * remout / gdp) |> 
      pull(sh)
    
    if (t0 == t1) period <- str_glue("In {t1}")
    else period <- str_glue("Averaged over {t0}–{t1}")
    
    data_t1_latest <- filter(data, t == t1_latest) |> 
      pull(remout)
    data_iso_t1_latest <- filter(data_iso_latest, t == t1_latest) |> 
      pull(remout)
    
    caption <- str_glue(paste(
      " - Outbound remittances, meanwhile, amounted to",
      "#b[{pl_usd(data_iso_t1_latest)}]",
      "{get_pct(data_t1_latest, data_iso_t1_latest)} in {t1_latest}. {period},",
      "these were #b[{pl_pct(sh_iso_t0_t1)}] {get_pct(sh_t0_t1, sh_iso_t0_t1)}",
      "of GDP."
    ))
  }
  
  return(caption)
}


# FDI ---------------------------------------------------------------------

caption_fdiin <- function(iso) {
  
  name <- namer(iso)
  
  data <- filter(gdidata::wdi, var == "fdiin" & v > 0) |> 
    left_join(filter(gdidata::wdi, var == "gdp"), by = c("geo", "t")) |> 
    rename(fdiin = v.x, gdp = v.y) |> 
    select(geo, t, fdiin, gdp) |> 
    group_by(geo) |> 
    fill(gdp, .direction = "down") |> 
    fill(gdp, .direction = "up") |> 
    ungroup()
  
  t1_max <- max(data$t) |> as.numeric()
  
  data_iso_latest <- filter(data, geo == iso & t > t1_max - 10)
  
  if (nrow(data_iso_latest) == 0) {
    
    caption <- str_glue(paste(
      " - The World Bank has no data on inbound FDI for {name} from the last",
      "10 years."
    ))
    
  } else {
    
    t1_latest <- max(data_iso_latest$t) |> as.numeric()
    
    data_iso <- filter(data, geo == iso) |> 
      drop_na(fdiin) |>
      filter(t > max(t) - 5)
    
    t0 <- min(data_iso$t) |> as.numeric()
    t1 <- max(data_iso$t) |> as.numeric()
    
    sh_iso_t0_t1 <- data_iso |> 
      summarise(fdiin = sum(fdiin), gdp = sum(gdp)) |> 
      mutate(sh = 100 * fdiin / gdp) |> 
      pull(sh)
    
    sh_t0_t1 <- filter(data, t >= t0 & t <= t1) |> 
      drop_na() |> 
      summarise(fdiin = sum(fdiin), gdp = sum(gdp), .by = geo) |> 
      mutate(sh = 100 * fdiin / gdp) |> 
      pull(sh)
    
    period <- str_glue("Averaged over {t0}–{t1}")
    share_text <- ""
    if (!is.na(sh_iso_t0_t1)) {
      share_text <- str_glue(paste(
        "{period}, these were #b[{pl_pct(sh_iso_t0_t1)}]",
        "{get_pct(sh_t0_t1, sh_iso_t0_t1)} of GDP."
      ))
    }
    if (t0 == t1) period <- str_glue("In {t1}")
    
    data_t1_latest <- filter(data, t == t1_latest) |> 
      pull(fdiin)
    data_iso_t1_latest <- filter(data_iso_latest, t == t1_latest) |> 
      pull(fdiin)
    
    caption <- str_glue(paste(
      " - The World Bank reports that inbound FDI to {name} amounted to",
      "#b[{pl_usd(data_iso_t1_latest)}]",
      "{get_pct(data_t1_latest, data_iso_t1_latest)} in {t1_latest}.",
      "{share_text}"
    ))
  }
  
  return(caption)
}

caption_fdiout <- function(iso) {
  
  name <- namer(iso)
  
  data <- filter(gdidata::wdi, var == "fdiout" & v > 0) |> 
    full_join(filter(gdidata::wdi, var == "gdp"), by = c("geo", "t")) |> 
    rename(fdiout = v.x, gdp = v.y) |> 
    select(geo, t, fdiout, gdp) |> 
    group_by(geo) |> 
    fill(gdp, .direction = "down") |> 
    fill(gdp, .direction = "up") |> 
    ungroup() |> 
    drop_na(fdiout)
  
  t1_max <- max(data$t) |> as.numeric()
  
  data_iso_latest <- filter(data, geo == iso & t > t1_max - 10)
  
  if (nrow(data_iso_latest) == 0) {
    
    caption <- str_glue(paste(
      " - The World Bank has no data on outbound FDI for {name} from the last",
      "10 years."
    ))
    
  } else {
    
    t1_latest <- max(data_iso_latest$t) |> as.numeric()
    
    data_iso <- filter(data, geo == iso) |> 
      drop_na(fdiout) |> 
      filter(t > max(t) - 5)
    
    t0 <- min(data_iso$t) |> as.numeric()
    t1 <- max(data_iso$t) |> as.numeric()
    
    sh_iso_t0_t1 <- data_iso |> 
      summarise(fdiout = sum(fdiout), gdp = sum(gdp)) |> 
      mutate(sh = 100 * fdiout / gdp) |> 
      pull(sh)
    
    sh_t0_t1 <- filter(data, t >= t0 & t <= t1) |> 
      drop_na() |> 
      summarise(fdiout = sum(fdiout), gdp = sum(gdp), .by = geo) |> 
      mutate(sh = 100 * fdiout / gdp) |> 
      pull(sh)
    
    period <- str_glue("Averaged over {t0}–{t1}")
    share_text <- ""
    if (!is.na(sh_iso_t0_t1)) {
      share_text <- str_glue(paste(
        "{period}, these were #b[{pl_pct(sh_iso_t0_t1)}]",
        "{get_pct(sh_t0_t1, sh_iso_t0_t1)} of GDP."
      ))
    }
    
    if (t0 == t1) period <- str_glue("In {t1}")
    
    data_t1_latest <- filter(data, t == t1_latest) |> 
      pull(fdiout)
    data_iso_t1_latest <- filter(data_iso_latest, t == t1_latest) |> 
      pull(fdiout)
    
    caption <- str_glue(paste(
      " - Outbound FDI, meanwhile, amounted to",
      "#b[{pl_usd(data_iso_t1_latest)}]",
      "{get_pct(data_t1_latest, data_iso_t1_latest)} in {t1_latest}.",
      "{share_text}"
    ))
  }
  
  return(caption)
}


# Population --------------------------------------------------------------

caption_pop <- function(iso) {
  
  name <- namer(iso)
  
  data <- filter(gdidata::wdi, var == "pop")
  data_iso <- filter(data, geo == iso)
  
  if (nrow(data_iso) == 0) {
    
    caption <- str_glue("UN DESA has no data on population for {name}.")
    
  } else {
    
    t0 <- min(data_iso$t) |> as.numeric()
    t1 <- max(data_iso$t) |> as.numeric()
    
    data_t1 <- filter(data, t == t1) |> pull(v)
    med_t1 <- median(data_t1, na.rm = TRUE)
    data_iso_t1 <- filter(data_iso, t == t1) |> pull(v)
    
    data_iso_t1_lab <- pl(data_iso_t1)
    if (data_iso_t1 > 10^9) {
      data_iso_t1_lab <- pl1(data_iso_t1)
    }
    caption <- str_glue(paste(
      "UN DESA estimates the population of {name} at #b[{data_iso_t1_lab}]",
      "{get_pct(data_t1, data_iso_t1)} as of {t1}. The global median across",
      "countries was {pl(med_t1)}."
    ))
  }
  
  return(caption)
}


# Birth rate --------------------------------------------------------------

caption_birth <- function(iso) {
  
  name <- namer(iso)
  
  data <- filter(gdidata::wdi, var == "birth")
  data_iso <- filter(data, geo == iso)
  
  if (nrow(data_iso) == 0) {
    
    caption <- str_glue("The World Bank has no data on birth rates for {name}.")
    
  } else {
    
    t0 <- min(data_iso$t) |> as.numeric()
    t1 <- max(data_iso$t) |> as.numeric()
    
    data_t1 <- filter(data, t == t1) |> pull(v)
    med_t1 <- median(data_t1, na.rm = TRUE)
    data_iso_t1 <- filter(data_iso, t == t1) |> pull(v)
    
    caption <- str_glue(paste(
      "The World Bank reports the {t1} birth rate at #b[{pl(data_iso_t1)}]",
      "{get_pct(data_t1, data_iso_t1)} live births per 1000 population. The",
      "global median was {pl(med_t1)}."
    ))
  }
  
  return(caption)
}


# Dependency ratio --------------------------------------------------------

caption_depend <- function(iso) {
  
  name <- namer(iso)
  
  data <- filter(gdidata::wdi, var == "depend")
  data_iso <- filter(data, geo == iso)
  
  if (nrow(data_iso) == 0) {
    
    caption <- str_glue(
      "The World Bank has no data on age dependency ratios for {name}."
    )
    
  } else {
    
    t0 <- min(data_iso$t) |> as.numeric()
    t1 <- max(data_iso$t) |> as.numeric()
    
    data_t1 <- filter(data, t == t1) |> pull(v)
    med_t1 <- median(data_t1, na.rm = TRUE)
    data_iso_t1 <- filter(data_iso, t == t1) |> pull(v)
    
    caption <- str_glue(paste(
      "The World Bank reports #b[{pl(data_iso_t1)}]",
      "{get_pct(data_t1, data_iso_t1)} dependents (younger than 15, older than",
      "64) per 100 working-age persons in {t1}. The global median was",
      "{pl(med_t1)}."
    ))
  }
  
  return(caption)
}


# Population pyramid ------------------------------------------------------

# caption_pyr <- function(iso) {
#   
#   name <- namer(iso)
#   
#   t1 <- max(gdidata::undesa_wpp$t)
#   
#   med <- filter(gdidata::undesa_wpp, t == max(t)) |> 
#     summarise(med = weighted.median(age, n), .by = geo)
#   med_iso <- filter(med, geo == iso) |> pull(med)
#   
#   age <- filter(gdidata::undesa_wpp, t == max(t)) |> 
#     mutate(age = case_when(
#       age %in% 0:14 ~ "children", 
#       age %in% 15:64 ~ "working", 
#       .default = "elderly" 
#     )) |> 
#     summarise(n = sum(n), .by = c(geo, age)) |> 
#     mutate(v = 100 * n / sum(n), .by = geo)
#   
#   sex <- filter(gdidata::undesa_wpp, t == max(t)) |> 
#     summarise(n = sum(n), .by = c(geo, sex)) |> 
#     mutate(v = 100 * n / sum(n), .by = geo) |> 
#     filter(sex == "female")
#   
#   age_iso <- filter(age, geo == iso)
#   sex_iso <- filter(sex, geo == iso)
#   
#   if (nrow(age_iso) == 0) {
#     
#     caption <- str_glue(
#       "UN DESA has no demographic information on the population of {name}."
#     )
#     
#   } else {
#     
#     age_child <- filter(age, age == "children")
#     age_work <- filter(age, age == "working")
#     age_elder <- filter(age, age == "elderly")
#     
#     age_child_iso <- filter(age_child, geo == iso) |> pull(v)
#     age_work_iso <- filter(age_work, geo == iso) |> pull(v)
#     age_elder_iso <- filter(age_elder, geo == iso) |> pull(v)
#     
#     sex_iso <- filter(sex, geo == iso) |> pull(v)
#     
#     caption <- str_glue(paste(
#       " - UN DESA estimates that the {t1} population of {name} comprised of",
#       "#b[{pl_pct(age_child_iso)}] {get_pct(age_child$v, age_child_iso)}",
#       "children (0-14), #b[{pl_pct(age_work_iso)}]",
#       "{get_pct(age_work$v, age_work_iso)} working age (15-64), and",
#       "#b[{pl_pct(age_elder_iso)}] {get_pct(age_elder$v, age_elder_iso)}",
#       "elderly (65+). The median age was #b[{pl(med_iso)}]",
#       "{get_pct(med$med, med_iso)}.\n",
#       "#colbreak()\n",
#       " - #b[{pl_pct(sex_iso)}] {get_pct(sex$v, sex_iso)} of the population",
#       "were female in {t1}.\n",
#       " - The map shows where populations are concentrated."
#     ))
#   }
#   
#   return(caption)
# }


# Income ------------------------------------------------------------------

caption_income <- function(iso) {
  
  name <- namer(iso)
  map <- "The map shows GNI per capita at the subnational level."
  
  data <- filter(gdidata::wdi, var == "income")
  data_iso <- filter(data, geo == iso)
  
  if (nrow(data_iso) == 0) {
    
    caption <- str_glue(
      "The World Bank has no data on per capita gross domestic product (GDP) ",
      "for {name}."
    )
    
  } else {
    
    t0 <- min(data_iso$t) |> as.numeric()
    t1 <- max(data_iso$t) |> as.numeric()
    
    data_t1 <- filter(data, t == t1) |> pull(v)
    med_t1 <- median(data_t1, na.rm = TRUE)
    
    data_iso_t1 <- filter(data_iso, t == t1) |> pull(v)
    
    data_iso_t1_lab <- pl_usd(data_iso_t1)
    if (data_iso_t1 < 955) {
      data_iso_t1_lab <- paste0(
        "\\$", 
        format(round(data_iso_t1, digits = -1), trim = TRUE, big.mark = ",")
      )
    }
    if (data_iso_t1 >= 995 & data_iso_t1 < 9955) {
      data_iso_t1_lab <- paste0(
        "\\$", 
        format(round(data_iso_t1, digits = -2), trim = TRUE, big.mark = ",")
      )
    }
    
    # growth <- filter(data, t >= t1 - 10 & t <= t1) |> 
    #   filter(t %in% c(min(t), max(t)), .by = geo) |> 
    #   mutate(p = case_when(t == min(t) ~ "0", t == max(t) ~ "1"), .by = geo) |> 
    #   pivot_wider(names_from = p, values_from = c(t, v), names_sep = "") |> 
    #   mutate(growth = 100 * ((v1 / v0)^(1 / (t1 - t0)) - 1))
    # 
    # growth_med <- median(growth$growth)
    # growth_iso <- filter(growth, geo == iso)$growth
    
    caption <- str_glue(paste(
      "The World Bank reports that {name} had a per capita GDP",
      "of #b[{data_iso_t1_lab}] {get_pct(data_t1, data_iso_t1)}",
      "(in constant 2021 international USD) in {t1}. The global median",
      "across countries was {pl_usd(med_t1)}."
    ))
  }
  
  return(caption)
}


# Unemployment rate -------------------------------------------------------

caption_unem <- function(iso) {
  
  name <- namer(iso)
  
  data <- filter(gdidata::wdi, var == "unem")
  t1_max <- max(data$t) |> as.numeric()
  data_iso_latest <- filter(data, geo == iso & t > t1_max - 10)

  if (nrow(data_iso_latest) == 0) {
    
    caption <- str_glue(paste(
      "The ILO has no data on unemployment rates for {name} from the last 10",
      "years."
    ))
    
  } else {
    
    t1_latest <- max(data_iso_latest$t) |> as.numeric()
    
    data_iso <- filter(data, geo == iso) |> 
      drop_na() |> 
      filter(t > max(t) - 5)
    
    t0 <- min(data_iso$t) |> as.numeric()
    t1 <- max(data_iso$t) |> as.numeric()
    
    data_t1_latest <- filter(data, t == t1_latest) |> pull(v)
    med_t1 <- median(data_t1_latest)
    data_iso_t1_latest <- filter(data_iso_latest, t == t1_latest) |> pull(v)
    data_iso_tL_latest <- filter(data_iso_latest, t == t1_latest - 1) |> 
      pull(v)
    
    # change <- round(data_iso_t1_latest - data_iso_tL_latest, digits = 1)
    # if (change == 0) verb <- "unchanged"
    # if (change > 0) verb <- "up"
    # if (change < 0) verb <- "down"
    
    caption <- str_glue(paste(
      "The ILO reports an unemployment rate of",
      "#b[{pl_pct1(data_iso_t1_latest)}]",
      "{get_pct(data_t1_latest, data_iso_t1_latest)} in {t1_latest}.",
      "The global median was {pl_pct1(med_t1)}."
    ))
  }
  
  return(caption)
}


# Inflation rate ----------------------------------------------------------

caption_inf <- function(iso) {
  
  name <- namer(iso)
  
  data <- filter(gdidata::wdi, var == "inf")
  t1_max <- max(data$t) |> as.numeric()
  data_iso_latest <- filter(data, geo == iso & t > t1_max - 10)
  
  if (nrow(data_iso_latest) == 0) {
    
    caption <- str_glue(paste(
      "The World Bank has no data on inflation rates for {name} from the last",
      "10 years."
    ))
    
  } else {
    
    t1_latest <- max(data_iso_latest$t) |> as.numeric()
    
    data_iso <- filter(data, geo == iso) |> 
      drop_na() |> 
      filter(t > max(t) - 5)
    
    t0 <- min(data_iso$t) |> as.numeric()
    t1 <- max(data_iso$t) |> as.numeric()
    
    data_t1_latest <- filter(data, t == t1_latest) |> pull(v)
    med_t1 <- median(data_t1_latest)
    data_iso_t1_latest <- filter(data_iso_latest, t == t1_latest) |> pull(v)
    data_iso_tL_latest <- filter(data_iso_latest, t == t1_latest - 1) |> 
      pull(v)
    
    # change_text <- ""
    # if (length(data_iso_tL_latest) > 0) {
    #   change <- round(data_iso_t1_latest - data_iso_tL_latest, digits = 1)
    #   if (change == 0) verb <- "unchanged"
    #   if (change > 0) verb <- "up"
    #   if (change < 0) verb <- "down"
    #   change_text <- str_glue(
    #     ", #b[{verb}] from {pl_pct1(data_iso_tL_latest)} the previous year"
    #   )
    # }
    
    caption <- str_glue(paste(
      "The World Bank reports an inflation rate of",
      "#b[{pl_pct1(data_iso_t1_latest)}]",
      "{get_pct(data_t1_latest, data_iso_t1_latest)} in",
      "{t1_latest}. The world median was {pl_pct1(med_t1)}."
    ))
  }
  
  return(caption)
}


