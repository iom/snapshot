
get_pct <- function(vector, value) {
  pct <- 1 - ecdf(vector)(value)
  
  if (pct <= .1) return("top10")
  if (pct > .1 & pct <= 1/3) return("top33")
  if (pct > 1/3 & pct < 2/3) return("mid33")
  if (pct >= 2/3 & pct < .9) return("bot33")
  if (pct >= .9) return("bot10")
}

pl <- function(num) prettylabel(num, signif = 2, spell = TRUE)
pl1 <- function(num) prettylabel(num, signif = 2, spell = TRUE)
pl_usd <- function(num) prettylabel(num, spell = TRUE, currency = "\\$")
pl_pct <- function(num) prettylabel(num, signif = 2, pct = TRUE)
pl_pct1 <- function(num) prettylabel(num, signif = 2, pct = TRUE)


# Placeholder -------------------------------------------------------------

caption_placeholder <- function(ncol = 2) {

  caption <- paste(
    "- Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nunc",
    "dignissim nec tellus sit amet cursus. Nam justo risus, dapibus in massa",
    "vitae, dictum feugiat elit.\n",
    "- Curabitur fermentum vehicula urna ac fringilla. Integer interdum orci",
    "vehicula volutpat maximus. Duis gravida vestibulum ex nec fermentum.",
    "Fusce sed aliquet ante, at ultrices leo.\n",
    "#colbreak()\n",
    "- Donec lectus urna, aliquam nec posuere quis, faucibus ac ligula.",
    "Phasellus tristique tortor eget urna scelerisque vehicula. Phasellus eu",
    "blandit felis, ac molestie neque. Vestibulum posuere leo at risus aliquam",
    "convallis.\n",
    "- Ut leo ante, faucibus vitae elit at, vulputate laoreet ligula. Sed",
    "dolor erat, ultricies eget fermentum vel, semper id nulla. Morbi sit amet",
    "feugiat ipsum."
  )

  if (ncol == 1) {
    
    caption <- paste(
      "- Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nunc",
      "dignissim nec tellus sit amet cursus. Nam justo risus, dapibus in massa",
      "vitae, dictum feugiat elit.\n",
      "- Curabitur fermentum vehicula urna ac fringilla. Integer interdum orci",
      "vehicula volutpat maximus. Duis gravida vestibulum ex nec fermentum.",
      "Fusce sed aliquet ante, at ultrices leo."
    )
  }
  
  if (ncol == 3) {
    
    caption <- paste(
      "- Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nunc",
      "dignissim nec tellus sit amet cursus. Nam justo risus, dapibus in massa",
      "vitae, dictum feugiat elit.\n",
      "#colbreak()\n",
      "- Curabitur fermentum vehicula urna ac fringilla. Integer interdum orci",
      "vehicula volutpat maximus. Duis gravida vestibulum ex nec fermentum.",
      "Fusce sed aliquet ante, at ultrices leo.\n",
      "#colbreak()\n",
      "- Donec lectus urna, aliquam nec posuere quis, faucibus ac ligula.",
      "Phasellus tristique tortor eget urna scelerisque vehicula. Phasellus eu",
      "blandit felis, ac molestie neque. Vestibulum posuere leo at risus aliquam",
      "convallis.\n"
    )
  }
  
  return(caption)
}


# Migrant stocks ----------------------------------------------------------

caption_stocks_pt <- function(iso) {
  
  name <- namer(iso, lang = "pt")
  
  if (iso == "XKX") {
      
    caption <- paste0(
      "#caption(ncol: 1)[",
      "Para efeitos estatísticos, o DAESNU inclui o Kosovo na Sérvia.",
      "]"
    )
    
  } else if (iso == "SDN") {
    
    caption <- paste0(
      "#caption[",
      caption_stocks_emig_pt(iso), "\n",
      "#colbreak()\n",
      caption_stocks_immig_pt(iso),
      "]"
    )
    
  } else if (iso == "PSE") {
    
    caption <- paste0(
      "#caption[",
      caption_stocks_emig_pt(iso), "\n",
      "#colbreak()\n",
      caption_stocks_immig_pt(iso),
      "]"
    )
    
  } else {
    
    caption <- paste0(
      "#caption[",
      caption_stocks_emig_pt(iso), "\n",
      "#colbreak()\n",
      caption_stocks_immig_pt(iso),
      "]"
    )
  }
  
  return(caption)
}

caption_stocks_emig_pt <- function(iso) {
  
  name <- namer(iso, lang = "pt")
  region <- filter(gdidata::countrynames, iso3 == iso)$iom_region
  
  data <- snap_data("stocks", use_2020 = TRUE)$data |> 
    filter(panel == "emig")
  data_iso <- snap_data("stocks", iso = iso, use_2020 = TRUE)$data |> 
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
  
  if (length(within_iso) == 0 & length(outside_iso) > 0) {
    region_text <- str_glue(
      "- #b[{ pl_pct(outside_iso) }] { get_pct(outside, outside_iso) } ",
      "dos emigrantes emigraram para fora da região."
    )
  }
  if (length(within_iso) > 0 & length(outside_iso) == 0) {
    region_text <- str_glue(
      "- #b[{ pl_pct(within_iso) }] { get_pct(within, within_iso) } ",
      "dos emigrantes permaneceram na {region}."
    )
  } else {
    region_text <- str_glue(
      "- #b[{ pl_pct(within_iso) }] { get_pct(within, within_iso) } ",
      "dos emigrantes permaneceram na {region} enquanto ",
      "#b[{ pl_pct(outside_iso) }] { get_pct(outside, outside_iso) } ",
      "emigraram para fora da região."
    )
  }
  
  caption <- str_glue(
    "- De acordo com as estimativas do DAESNU, os emigrantes da {name} ",
    'totalizavam #b[{ pl(data_iso_t1) }] { get_pct(data_t1$n, data_iso_t1) } ',
    "no {t1}, equivalente a #b[{ pl_pct(share_iso_t1) }] ",
    "{ get_pct(share_t1$v, share_iso_t1) } da sua população. Esta foi uma ",
    "mudança média de #b[{ change_iso_lab }] { get_pct(change, change_iso) } ",
    "ao ano durante o período de {t0}–{t1}.\n",
    "{region_text}"
  )
  
  return(caption)
}

caption_stocks_immig_pt <- function(iso) {
  
  name <- namer(iso, lang = "pt")
  region <- filter(gdidata::countrynames, iso3 == iso)$iom_region
  data <- snap_data("stocks", use_2020 = TRUE)$data |> 
    filter(panel == "immig")
  data_iso <- snap_data("stocks", iso = iso, use_2020 = TRUE)$data |> 
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
  
  if (length(within_iso) == 0 & length(outside_iso) > 0) {
    
    region_text <- str_glue(paste(
      "- #b[{pl_pct(outside_iso)}] {get_pct(outside, outside_iso)} dos",
      "imigrantes vieram de fora da região."
    ))
    
  } else if (length(within_iso) > 0 & length(outside_iso) == 0) {
    
    region_text <- str_glue(paste(
      "- #b[{pl_pct(within_iso)}] {get_pct(within, within_iso)} dos imigrantes",
      "vieram de dentro da região, enquanto."
    ))
    
  } else if (length(within_iso) == 0 & length(outside_iso) == 0) {
    
    region_text <- "- Todos tinham origens desconhecidas."
    
  } else {
    
    region_text <- str_glue(paste(
      "- #b[{pl_pct(within_iso)}] {get_pct(within, within_iso)} dos imigrantes",
      "vieram de dentro da região, enquanto #b[{pl_pct(outside_iso)}]",
      "{get_pct(outside, outside_iso)} imigraram de fora da região."
    ))
  }
  
  caption <- str_glue(paste(
    "- Os imigrantes na {name} totalizavam #b[{pl(data_iso_t1)}]",
    "{get_pct(data_t1$n, data_iso_t1)} no {t1}, equivalente a",
    "#b[{pl_pct(share_iso_t1)}] {get_pct(share_t1$v, share_iso_t1)} da sua",
    "população. Esta foi uma mudança média de #b[{change_iso_lab}]",
    "{get_pct(change, change_iso)} ao ano durante o período {t0}–{t1}.\n",
    "{region_text}"
  ))
  
  return(caption)
}


# Net migration -----------------------------------------------------------

caption_nmig_pt <- function(iso) {
  
  name <- namer(iso)
  
  data <- snap_data("nmig")$data
  data_iso <- snap_data("nmig", iso)$data
  
  map <- paste(
    "No mapa abaixo, a migração líquida é expressa em relação à população",
    "local. O azul representa a imigração líquida per capita, enquanto o",
    "vermelho representa a emigração líquida per capita. Como os valores são",
    "relativos, o domínio de uma cor ou de outra não significa necessariamente",
    "o nível agregado da migração líquida."
  )
  
  if (nrow(drop_na(data_iso, .data$n)) == 0) {
    
    caption <- str_glue(paste(
      "O Banco Mundial não tem informação sobre a migração líquida para o",
      "{name}.",
      "#colbreak()\n",
      "{map}"
    ))
    
  } else {
    
    t1 <- max(data_iso$t) |> as.numeric()
    t0 <- t1 - 9
    
    min <- min(data_iso$n)
    max <- max(data_iso$n)
    
    avg <- filter(data, t >= t0) |> 
      summarise(n = mean(n), .by = geo) |> 
      pull(n)
    avg_iso <- filter(data, geo == iso & t >= t0) |> 
      summarise(n = mean(n), .by = geo) |> 
      pull(n)
    sign <- ifelse(avg_iso > 0, "+", "")
    
    caption <- str_glue(paste(
      "- A migração líquida anual na {name} variou entre #b[{ pl(min) }] e",
      "#b[{ pl(max) }] durante o período de {t0}\u2013{t1} (valores positivos",
      "significam mais pessoas a imigrar do que emigrar; valores negativos",
      "significam o oposto).",
      "A migração líquida anual média durante esse período foi de",
      "#b[{ sign }{ pl(avg_iso) }] { get_pct(avg, avg_iso) }.\n",
      "#colbreak()\n",
      "- {map}"
    ))
  }
  
  return(caption)
}


# Age structure of migrants -----------------------------------------------

caption_migpyr_pt <- function(iso) {
  
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
      "O DAESNU não dispõe de informação sobre o sexo e a idade dos migrantes",
      "de {name}."
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
      "- O DAESNU não dispõe de informação sobre o sexo e a idade dos",
      "emigrantes {name}."
    ))
    if (!is.na(data_emig_child_iso)) {
      caption_emig <- str_glue(paste(
        "- A partir de dados do DAESNU,",
        "a OIM Estima que dos emigrantes de {name},",
        
        "#b[{pl_pct(data_emig_child_iso)}]",
        "{get_pct(data_emig_child$v, data_emig_child_iso)}",
        "eram crianças (0-14),",
        
        "#b[{pl_pct(data_emig_work_iso)}]",
        "{get_pct(data_emig_work$v, data_emig_work_iso) }",
        "tinham idade ativa (ou seja trabalhavam) (15-64), and",
        
        "#b[{pl_pct(data_emig_elder_iso)}]",
        "{get_pct(data_emig_elder$v, data_emig_elder_iso)}",
        "eram idosos (65+) em {t1}.\n",
        
        "- #b[{pl_pct(data_f_emig_iso)}]",
        "{get_pct(data_f_emig$v, data_f_emig_iso)}",
        "dos emigrantes eram mulheres em {t1}."
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
      "- O DAESNU não tem informação sobre o sexo e a idade dos",
      "imigrantes {name}."
    ))
    if (!is.na(data_immig_child_iso)) {
      caption_immig <- str_glue(paste(
        "- Entre os imigrantes indo para {name}, DAESNU reporta que",
        
        "#b[{pl_pct(data_immig_child_iso)}]",
        "{get_pct(data_immig_child$v, data_immig_child_iso)}",
        "eram crianças (0-14),",
        
        "#b[{pl_pct(data_immig_work_iso)}]",
        "{get_pct(data_immig_work$v, data_immig_work_iso)}",
        "tinham idade ativa (15-64), e",
        
        "#b[{ pl_pct(data_immig_elder_iso) }]",
        "{get_pct(data_immig_elder$v, data_immig_elder_iso)}",
        "eram idosos (65+) em {t1}.\n",
        
        "- #b[{pl_pct(data_f_immig_iso)}]",
        "{get_pct(data_f_immig$v, data_f_immig_iso)}",
        "dos imigrantes eram mulheres em {t1}."
      ))
    }
    
    caption <- str_glue("{caption_emig}\n", "#colbreak()\n", "{caption_immig}")
  }
  
  return(caption)
}


# Refugees ----------------------------------------------------------------

caption_refug_orig_pt <- function(iso) {
  
  name <- namer(iso, lang = "pt")
  
  data <- gdidata::unhcr |> 
    left_join(gdidata::countrynames, by = c("geo" = "iso3")) |> 
    filter(group == "refugee") |> 
    summarise(n = sum(n), .by = c(geo, t, from, region))
  data_iso <- filter(data, from == iso) |> 
    summarise(n = sum(n), .by = c(from, t))

  if (nrow(data_iso) == 0) {
    
    caption <- str_glue(
      "- O ACNUR não dispõe de informação sobre refugiados originários de ",
      "países com {name}.\n\n"
    )
    
  } else {
    
    data_iso <- data_iso |> 
      complete(t = seq(min(data_iso$t), max(data_iso$t)), fill = list(n = 0))

    t1 <- max(data_iso$t)
    t0 <- t1 - 1
    
    data_t0t1 <- gdidata::unhcr |> 
      filter(group == "refugee", t %in% c(t0, t1)) |> 
      summarise(n = sum(n), .by = c(from, t)) |> 
      mutate(t = case_when(t == t0 ~ "col_t0", t == t1 ~ "col_t1")) |> 
      pivot_wider(names_from = t, values_from = n) |> 
      drop_na() |> 
      mutate(change = 100 * ((col_t1 / col_t0)^(1 / (t1 - t0)) - 1))
    
    data_t0 <- gdidata::unhcr |> 
      filter(group == "refugee", t == t0) |> 
      summarise(n = sum(n), .by = from)
    data_t1 <- gdidata::unhcr |> 
      filter(group == "refugee", t == t1) |> 
      summarise(n = sum(n), .by = from)
    
    data_iso_t0 <- filter(data_iso, t == t0) |> pull(n)
    data_iso_t1 <- filter(data_iso, t == t1) |> pull(n)
    
    # Shares of population
    pop_t1 <- gdidata::undesa_wpp |> 
      filter(t == t1) |> 
      summarise(n = 1000 * sum(n), .by = geo) |> 
      rename(pop = n)
    share_t1 <- data_t1 |> 
      left_join(pop_t1, by = c("from" = "geo")) |> 
      mutate(v = 100 * n / pop) |> 
      drop_na()
    share_iso_t1 <- share_t1 |> filter(from == iso) |> pull(v)
    
    # 1 year change 
    change_text <- ""
    if (nrow(filter(data_iso, t == t0)) > 0) {
      change <- data_t0t1$change
      change_iso <- 100 * ((data_iso_t1 / data_iso_t0)^(1 / (t1 - t0)) - 1)
      change_iso_lab <- pl_pct1(change_iso)
      if (change_iso > 0) change_iso_lab <- paste0("+", pl_pct1(change_iso))
      change_text <- str_glue(paste(
        "Esta foi uma mudança de #b[{change_iso_lab}]",
        "{get_pct(change, change_iso)} em relação ao ano anterior."
      ))
    }
    
    if (length(data_iso_t0) > 0) {
      if (data_iso_t1 > 0 & data_iso_t0 == 0) {
        change_text <- "Não houve refugiados registados no ano anterior."
      }
    }
    
    # Top host countries
    host_iso <- snap_data("refug", iso, lang = "pt")$data |> 
      filter(panel == "orig", t == t1) |> 
      mutate(v = 100 * n / sum(n)) |> 
      arrange(desc(n))
    
    hosts <- filter(host_iso, nat != "Outros")
    others <- filter(host_iso, nat == "Outros")
    
    if (nrow(hosts) == 0) {
      hosts_text <- ""
    }
    
    if (nrow(hosts) == 1) {
      hosts_text <- str_glue(
        "- Todos foram alojados no { namer(hosts$nat[1], bold = TRUE) }."
      )
    }
    
    if (nrow(hosts) == 2) {
      hosts_text <- str_glue(
        "- { pl_pct(hosts$v[1]) } foram acolhidos em ",
        "{ namer(hosts$nat[1], bold = TRUE) } enquanto ",
        "{ pl_pct(hosts$v[2]) } foram acolhidos em ",
        "{ namer(hosts$nat[2], bold = TRUE) }."
      )
    }
    
    if (nrow(hosts) == 3) {
      hosts_text <- str_glue(
        "- { pl_pct(hosts$v[1]) } foram acolhidos em ",
        "{ namer(hosts$nat[1], bold = TRUE) }, ",
        "{ pl_pct(hosts$v[2]) } em ",
        "{ namer(hosts$nat[2], bold = TRUE) }, e ",
        "{ pl_pct(hosts$v[3]) } em { namer(hosts$nat[3], bold = TRUE) }."
      )
    }
    
    others_text <- ""
    if (nrow(others) == 1) {
      others_text <- str_glue(
        "Os restantes { pl_pct(others$v) } foram alojados noutro local."
      )
    }
    
    caption <- str_glue(
      "- De acordo com as estimativas do ACNUR, os refugiados da {name} ",
      "totalizavam #b[{pl(data_iso_t1)}] {get_pct(data_t1$n, data_iso_t1)} em ",
      "{t1}, equivalente a #b[{pl_pct(share_iso_t1)}] ",
      "{get_pct(share_t1$v, share_iso_t1)} da sua população. {change_text}\n",
      "{hosts_text} {others_text}"
    )
  }
  
  return(caption)
}


caption_refug_host_pt <- function(iso) {
  
  name <- namer(iso)
  
  data <- gdidata::unhcr |> 
    left_join(gdidata::countrynames, by = c("from" = "iso3")) |> 
    filter(group == "refugee") |> 
    summarise(n = sum(n), .by = c(geo, t, from, region)) |> 
    mutate(region = case_when(is.na(region) ~ "Unknown", .default = region))
  data_iso <- filter(data, geo == iso) |> 
    summarise(n = sum(n), .by = c(geo, t))
  
  if (nrow(data_iso) == 0) {
    
    caption <- str_glue(
      "- O ACNUR não tem informação sobre os refugiados acolhidos em {name}."
    )
    
  } else {
    
    t1 <- max(data_iso$t)
    t0 <- t1 - 1
    
    data_t0t1 <- gdidata::unhcr |> 
      filter(group == "refugee", t %in% c(t0, t1)) |> 
      summarise(n = sum(n), .by = c(geo, t)) |> 
      mutate(t = case_when(t == t0 ~ "col_t0", t == t1 ~ "col_t1")) |> 
      pivot_wider(names_from = t, values_from = n) |> 
      drop_na() |> 
      mutate(change = 100 * ((col_t1 / col_t0)^(1 / (t1 - t0)) - 1))
    
    data_t0 <- gdidata::unhcr |> 
      filter(group == "refugee", t == t0) |> 
      summarise(n = sum(n), .by = geo)
    data_t1 <- gdidata::unhcr |> 
      filter(group == "refugee", t == t1) |> 
      summarise(n = sum(n), .by = geo)

    data_iso_t0 <- filter(data_iso, t == t0) |> pull(n)
    data_iso_t1 <- filter(data_iso, t == t1) |> pull(n)
    
    # Shares of population
    pop_t1 <- gdidata::undesa_wpp |> 
      filter(t == t1) |> 
      summarise(n = 1000 * sum(n), .by = geo) |> 
      rename(pop = n)
    share_t1 <- data_t1 |> 
      left_join(pop_t1, by = "geo") |> 
      mutate(v = 100 * n / pop) |> 
      drop_na()
    share_iso_t1 <- share_t1 |> filter(geo == iso) |> pull(v)
    
    # 1 year change
    change_text <- ""
    if (nrow(filter(data_iso, t == t0)) > 0) {
      change <- data_t0t1$change
      change_iso <- 100 * ((data_iso_t1 / data_iso_t0)^(1 / (t1 - t0)) - 1)
      change_iso_lab <- pl_pct1(change_iso)
      if (change_iso > 0) change_iso_lab <- paste0("+", pl_pct1(change_iso))
      change_text <- str_glue(paste(
        "Esta foi uma mudança de #b[{change_iso_lab}]",
        "{get_pct(change, change_iso)} em relação ao ano anterior."
      ))
    }
    
    if (length(data_iso_t0) > 0) {
      if (data_iso_t1 > 0 & data_iso_t0 == 0) {
        change_text <- "Não houve refugiados registados no ano anterior."
      }
    }
    
    # Top origin countries
    orig_iso <- snap_data("refug", iso, lang = "pt")$data |> 
      filter(panel == "host", t == t1) |> 
      mutate(v = 100 * n / sum(n)) |> 
      arrange(desc(n))
    
    origs <- orig_iso |> filter(!(nat %in% c("OOO", "Outros")))
    unknowns <- orig_iso |> filter(nat == "Desconhecida")
    others <- orig_iso |> filter(nat == "Outros")
    
    if (nrow(origs) == 0) {
      origs_text <- str_glue(
        orig_text <- ""
      )
    }
    
    if (nrow(origs) == 1) {
      origs_text <- str_glue(
        "- Todos vieram do { namer(origs$nat[1], bold = TRUE) }."
      )
    }
    
    if (nrow(origs) == 2) {
      origs_text <- str_glue(
        "- { pl_pct(origs$v[1]) } vieram de ",
        "{ namer(origs$nat[1], bold = TRUE) } enquanto ",
        "{ pl_pct(origs$v[2]) } vieram de { namer(origs$nat[2], bold = TRUE) }."
      )
    }
    
    if (nrow(origs) == 3) {
      origs_text <- str_glue(
        "- { pl_pct(origs$v[1]) } vieram de ",
        "{ namer(origs$nat[1], bold = TRUE) }, ",
        "{ pl_pct(origs$v[2]) } vieram de ",
        "{ namer(origs$nat[2], bold = TRUE) }, e ",
        "{ pl_pct(origs$v[3]) } vieram de { namer(origs$nat[3], bold = TRUE) }."
      )
    }
    
    unknown_text <- ""
    if (nrow(unknowns) == 1) {
      unknown_text <- str_glue(
        "Outros { pl_pct(unknowns$v) } tinham origens desconhecidas."
      )
    }
    
    others_text <- ""
    if (nrow(others) == 1) {
      others_text <- str_glue(
        "Os restantes { pl_pct(others$v) } vieram de outros locais."
      )
    }
    
    caption <- str_glue(
      "- Os refugiados acolhidos na {name} totalizavam #b[{pl(data_iso_t1)}] ",
      "{get_pct(data_t1$n, data_iso_t1)} em {t1}, equivalente a ",
      "#b[{pl_pct(share_iso_t1)}] {get_pct(share_t1$v, share_iso_t1)} da sua ",
      "população. {change_text}\n",
      "{origs_text} {unknown_text} {others_text}"
    )
  }
  
  return(caption)
}


# Internal displacements --------------------------------------------------

caption_idp_pt <- function(iso) {
  
  name <- namer(iso)
  
  data <- filter(gdidata::idmc_flows, t >= max(t) - 9 & n > 0)
  data_iso <- filter(data, geo == iso)
  t0 <- min(data$t) |> as.numeric()
  t1 <- max(data$t) |> as.numeric()
  
  if (nrow(data_iso) == 0) {
    
    caption <- str_glue(
      "#caption(ncol: 1)[",
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
    
    cause_text <- "Todos foram causados por desastres."
  
  } else if (agg_iso_disaster == 0 & agg_iso_conflict > 0) {
    
    cause_text <- "Todos foram causados por conflitos."
    
  } else {
    
    cause_text <- str_glue(
      "Of these, #b[{ pl(agg_iso_conflict) }] (or ",
      "{ pl_pct(100 * agg_iso_conflict / agg_iso) }) were due to conflict and ",
      "#b[{ pl(agg_iso_disaster) }] (or ",
      "{ pl_pct(100 * agg_iso_disaster / agg_iso) }) were due to disasters."
    )
  }
  
  totals_text <- str_glue(
    "O IDMC registou #b[{ pl(agg_iso) }] { get_pct(agg, agg_iso) } ",
    "deslocamentos internos na {name} durante o período 2014\u20132023, ",
    "equivalente a uma média anual de { pl(agg_iso / 10) }. {cause_text}"
  )
  
  # Disasters
  
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
    
    caption <- str_glue(paste(
      "#caption[",
      " - {totals_text}\n",
      "#colbreak()\n",
      " - Among disasters, {natdis}",
      "]"
    ))
  }
  
  caption <- str_glue("#caption(ncol: 1)[{totals_text}]")
  
  return(caption)
}


# Missing migrants --------------------------------------------------------

caption_mmp_pt <- function(iso) {
  
  name <- namer(iso)
  
  data <- summarise(gdidata::iom_mmp, n = sum(dead), .by = geo)
  data_iso <- filter(data, geo == iso)
  
  t0 <- min(gdidata::iom_mmp$t, na.rm = TRUE) |> year()
  t1 <- max(gdidata::iom_mmp$t, na.rm = TRUE) |> year()
  
  if (nrow(data_iso) == 0) {
    
    caption <- str_glue(paste(
      "- A OIM não registrou mortes ou desaparecimentos de pessoas no ato de",
      "migração internacional na {name} entre {t0}–{t1}.\n",
      "#colbreak()\n",
      "- Note que, dadas as graves limitações de dados, isso não implica que",
      "não ocorreram mortes de migrantes neste território durante este período."
    ))
    
  } else {
    
    data_iso_cause <- filter(gdidata::iom_mmp, geo == iso) |> 
      summarise(n = sum(dead), .by = cause) |> 
      arrange(desc(n)) |> 
      mutate(
        cause = case_when(
          str_detect(cause, "Drowning") ~ "afogamento",
          str_detect(cause, "Accidental") ~ "acidentes",
          str_detect(cause, "Harsh") ~ "condições adversas",
          str_detect(cause, "Sickness") ~ "doença",
          str_detect(cause, "transport") ~ "riscos de transporte",
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
            "Todas as mortes e desaparecimentos foram causados por",
            "#b[{data_iso_cause$cause[1]}]"
          ))
          
        } else {
          
          cause <- str_glue(paste(
            "A principal causa de mortes e desaparecimentos foi",
            "#b[{data_iso_cause$cause[1]}] ({pl_pct(data_iso_cause$sh[1])})."
          ))
        }
      }
      
      if (nrow(data_iso_cause) == 2) {
        cause <- str_glue(paste(
          "As principais causas de mortes e desaparecimentos foram",
          "#b[{data_iso_cause$cause[1]}] ({pl_pct(data_iso_cause$sh[1])}) e",
          "#b[{data_iso_cause$cause[2]}] ({pl_pct(data_iso_cause$sh[2])})."
        ))
      }
      
      if (nrow(data_iso_cause) == 3) {
        cause <- str_glue(paste(
          "As principais causas de mortes e desaparecimentos foram",
          "#b[{data_iso_cause$cause[1]}] ({pl_pct(data_iso_cause$sh[1])}),",
          "#b[{data_iso_cause$cause[2]}] ({pl_pct(data_iso_cause$sh[2])}), e",
          "#b[{data_iso_cause$cause[3]}] ({pl_pct(data_iso_cause$sh[3])})."
        ))
      }
    }
    
    if (nrow(data_iso_mix) == 1) {
      
      if (data_iso_mix$sh == 100) {
        
        cause <- paste(
          "Todas as mortes e desaparecimentos tiveram causas mistas ou",
          "desconhecidas."
        )
          
      } else {
        
        cause <- paste(
          cause, 
          str_glue(paste(
            "Cerca de {(pl_pct(data_iso_mix$sh))} das mortes tiveram causas",
            "mistas ou desconhecidas."
          )))
      }
    }
    
    caption <- str_glue(paste(
      "- Durante o período {t0}–{t1}, a OIM relata que pelo menos",
      "#b[{pl(data_iso$n)}] {get_pct(data$n, data_iso$n)} indivíduos morreram",
      "ou desapareceram no território da {name} durante o ato de migração",
      "internacional.\n",
      "#colbreak()\n",
      " - {cause}"
    ))
  }
  
  return(caption)
}


# Remittances -------------------------------------------------------------

caption_remin_pt <- function(iso) {
  
  name <- namer(iso, lang = "pt")
  
  data <- gdidata::wdi |> 
    filter(var == "remin" & v > 0) |> 
    left_join(filter(gdidata::wdi, var == "gdp"), by = c("geo", "t")) |> 
    rename(remin = v.x, gdp = v.y) |> 
    select(geo, t, remin, gdp)
  
  t1_max <- max(data$t) |> as.numeric()
  
  data_iso_latest <- data |> filter(geo == iso, t > t1_max - 10)
  
  if (nrow(data_iso_latest) == 0) {
    
    caption <- str_glue(paste(
      "- The World Bank has no data on inbound remittances for {name} from",
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
      "- The World Bank reports that inbound remittances to {name} amounted",
      "to #b[{pl_usd(data_iso_t1_latest)}]",
      "{get_pct(data_t1_latest, data_iso_t1_latest)} in {t1_latest}. {period},",
      "these were #b[{pl_pct(sh_iso_t0_t1)}] {get_pct(sh_t0_t1, sh_iso_t0_t1)}",
      "of its GDP."
    ))
  }
  
  return(caption)
}

caption_remout_pt <- function(iso) {
  
  name <- namer(iso)
  
  data <- filter(gdidata::wdi, var == "remout" & v > 0) |> 
    left_join(filter(gdidata::wdi, var == "gdp"), by = c("geo", "t")) |> 
    rename(remout = v.x, gdp = v.y) |> 
    select(geo, t, remout, gdp)
  
  t1_max <- max(data$t) |> as.numeric()
  
  data_iso_latest <- filter(data, geo == iso & t > t1_max - 10)
  
  if (nrow(data_iso_latest) == 0) {
    
    caption <- str_glue(paste(
      "- The World Bank has no data on outbound remittances for {name} from",
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
      "- Outbound remittances, meanwhile, amounted to",
      "#b[{pl_usd(data_iso_t1_latest)}]",
      "{get_pct(data_t1_latest, data_iso_t1_latest)} in {t1_latest}. {period},",
      "these were #b[{pl_pct(sh_iso_t0_t1)}] {get_pct(sh_t0_t1, sh_iso_t0_t1)}",
      "of its GDP."
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
        "{get_pct(sh_t0_t1, sh_iso_t0_t1)} of its GDP."
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

caption_fdiin_pt <- function(iso) {
  
  name <- namer(iso, lang = "pt")
  
  data <- gdidata::wdi |> 
    filter(var == "fdiin", v > 0) |> 
    left_join(filter(gdidata::wdi, var == "gdp"), by = c("geo", "t")) |> 
    rename(fdiin = v.x, gdp = v.y) |> 
    select(geo, t, fdiin, gdp) |> 
    group_by(geo) |> 
    fill(gdp, .direction = "down") |> 
    fill(gdp, .direction = "up") |> 
    ungroup()
  
  t1_max <- max(data$t) |> as.numeric()
  
  data_iso_latest <- data |> filter(geo == iso, t > t1_max - 10)
  
  if (nrow(data_iso_latest) == 0) {
    
    caption <- str_glue(paste(
      "- O Banco Mundial não tem dados sobre IED recebido para a",
      "{name} nos últimos 10 anos."
    ))
    
  } else {
    
    t1_latest <- max(data_iso_latest$t) |> as.numeric()
    
    data_iso <- data |> 
      filter(geo == iso) |> 
      drop_na(fdiin) |>
      filter(t > max(t) - 5)
    
    t0 <- min(data_iso$t) |> as.numeric()
    t1 <- max(data_iso$t) |> as.numeric()
    
    sh_iso_t0_t1 <- data_iso |> 
      summarise(fdiin = sum(fdiin), gdp = sum(gdp)) |> 
      mutate(sh = 100 * fdiin / gdp) |> 
      pull(sh)
    
    sh_t0_t1 <- data |> 
      filter(t >= t0, t <= t1) |> 
      drop_na() |> 
      summarise(fdiin = sum(fdiin), gdp = sum(gdp), .by = geo) |> 
      mutate(sh = 100 * fdiin / gdp) |> 
      pull(sh)
    
    period <- str_glue("Em média, durante o período {t0}–{t1}")
    share_text <- ""
    if (!is.na(sh_iso_t0_t1)) {
      share_text <- str_glue(paste(
        "{period}, este representou #b[{pl_pct(sh_iso_t0_t1)}]",
        "{get_pct(sh_t0_t1, sh_iso_t0_t1)} do seu PIB."
      ))
    }
    if (t0 == t1) period <- str_glue("In {t1}")
    
    data_t1_latest <- filter(data, t == t1_latest) |> 
      pull(fdiin)
    data_iso_t1_latest <- filter(data_iso_latest, t == t1_latest) |> 
      pull(fdiin)
    
    caption <- str_glue(paste(
      "- O Banco Mundial reporta que o IED recebido na {name}, totalizou",
      "#b[{pl_usd(data_iso_t1_latest)}]",
      "{get_pct(data_t1_latest, data_iso_t1_latest)} em {t1_latest}.",
      "{share_text}"
    ))
  }
  
  return(caption)
}

caption_fdiout <- function(iso) {
  
  name <- namer(iso)
  
  data <- gdidata::wdi |> 
    filter(var == "fdiout", v > 0) |> 
    full_join(filter(gdidata::wdi, var == "gdp"), by = c("geo", "t")) |> 
    rename(fdiout = v.x, gdp = v.y) |> 
    select(geo, t, fdiout, gdp) |> 
    group_by(geo) |> 
    fill(gdp, .direction = "down") |> 
    fill(gdp, .direction = "up") |> 
    ungroup() |> 
    drop_na(fdiout)
  
  t1_max <- max(data$t) |> as.numeric()
  
  data_iso_latest <- data |> filter(geo == iso, t > t1_max - 10)
  
  if (nrow(data_iso_latest) == 0) {
    
    caption <- str_glue(paste(
      "- The World Bank has no data on outbound FDI for {name} from the last",
      "10 years."
    ))
    
  } else {
    
    t1_latest <- max(data_iso_latest$t) |> as.numeric()
    
    data_iso <- data |> filter(geo == iso) |> 
      drop_na(fdiout) |> 
      filter(t > max(t) - 5)
    
    t0 <- min(data_iso$t) |> as.numeric()
    t1 <- max(data_iso$t) |> as.numeric()
    
    sh_iso_t0_t1 <- data_iso |> 
      summarise(fdiout = sum(fdiout), gdp = sum(gdp)) |> 
      mutate(sh = 100 * fdiout / gdp) |> 
      pull(sh)
    
    sh_t0_t1 <- data |> filter(t >= t0, t <= t1) |> 
      drop_na() |> 
      summarise(fdiout = sum(fdiout), gdp = sum(gdp), .by = geo) |> 
      mutate(sh = 100 * fdiout / gdp) |> 
      pull(sh)
    
    period <- str_glue("Averaged over {t0}–{t1}")
    share_text <- ""
    if (!is.na(sh_iso_t0_t1)) {
      share_text <- str_glue(paste(
        "{period}, these were #b[{pl_pct(sh_iso_t0_t1)}]",
        "{get_pct(sh_t0_t1, sh_iso_t0_t1)} of its GDP."
      ))
    }
    
    if (t0 == t1) period <- str_glue("In {t1}")
    
    data_t1_latest <- filter(data, t == t1_latest) |> 
      pull(fdiout)
    data_iso_t1_latest <- filter(data_iso_latest, t == t1_latest) |> 
      pull(fdiout)
    
    caption <- str_glue(paste(
      "- Outbound FDI, meanwhile, amounted to",
      "#b[{pl_usd(data_iso_t1_latest)}]",
      "{get_pct(data_t1_latest, data_iso_t1_latest)} in {t1_latest}.",
      "{share_text}"
    ))
  }
  
  return(caption)
}

caption_fdiout_pt <- function(iso) {
  
  name <- namer(iso, lang = "pt")
  
  data <- gdidata::wdi |> 
    filter(var == "fdiout", v > 0) |> 
    full_join(filter(gdidata::wdi, var == "gdp"), by = c("geo", "t")) |> 
    rename(fdiout = v.x, gdp = v.y) |> 
    select(geo, t, fdiout, gdp) |> 
    group_by(geo) |> 
    fill(gdp, .direction = "down") |> 
    fill(gdp, .direction = "up") |> 
    ungroup() |> 
    drop_na(fdiout)
  
  t1_max <- max(data$t) |> as.numeric()
  
  data_iso_latest <- data |> filter(geo == iso, t > t1_max - 10)
  
  if (nrow(data_iso_latest) == 0) {
    
    caption <- str_glue(paste(
      "- O Banco Mundial não tem dados sobre IED enviado para a {name} nos",
      "últimos 10 anos."
    ))
    
  } else {
    
    t1_latest <- max(data_iso_latest$t) |> as.numeric()
    
    data_iso <- data |> filter(geo == iso) |> 
      drop_na(fdiout) |> 
      filter(t > max(t) - 5)
    
    t0 <- min(data_iso$t) |> as.numeric()
    t1 <- max(data_iso$t) |> as.numeric()
    
    sh_iso_t0_t1 <- data_iso |> 
      summarise(fdiout = sum(fdiout), gdp = sum(gdp)) |> 
      mutate(sh = 100 * fdiout / gdp) |> 
      pull(sh)
    
    sh_t0_t1 <- data |> filter(t >= t0, t <= t1) |> 
      drop_na() |> 
      summarise(fdiout = sum(fdiout), gdp = sum(gdp), .by = geo) |> 
      mutate(sh = 100 * fdiout / gdp) |> 
      pull(sh)
    
    period <- str_glue("Tomando em consideração a média entre {t0}–{t1}")
    share_text <- ""
    if (!is.na(sh_iso_t0_t1)) {
      share_text <- str_glue(paste(
        "{period}, este representou #b[{pl_pct(sh_iso_t0_t1)}]",
        "{get_pct(sh_t0_t1, sh_iso_t0_t1)} do seu PIB."
      ))
    }
    
    if (t0 == t1) period <- str_glue("In {t1}")
    
    data_t1_latest <- filter(data, t == t1_latest) |> 
      pull(fdiout)
    data_iso_t1_latest <- filter(data_iso_latest, t == t1_latest) |> 
      pull(fdiout)
    
    caption <- str_glue(paste(
      "- O IED enviado, por sua vez, ascendeu a",
      "#b[{pl_usd(data_iso_t1_latest)}]",
      "{get_pct(data_t1_latest, data_iso_t1_latest)} em {t1_latest}.",
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

caption_pop_pt <- function(iso) {
  
  name <- namer(iso, lang = "pt")
  
  data <- filter(gdidata::wdi, var == "pop")
  data_iso <- filter(data, geo == iso)
  
  if (nrow(data_iso) == 0) {
    
    caption <- str_glue("O DAESNU não tem dados sobre a população da {name}.")
    
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
      "O DAESNU estima a população da {name} em #b[{data_iso_t1_lab}]",
      "{get_pct(data_t1, data_iso_t1)} em {t1}. A mediana global foi de",
      "{pl(med_t1)}."
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

caption_birth_pt <- function(iso) {
  
  name <- namer(iso, lang = "pt")
  
  data <- filter(gdidata::wdi, var == "birth")
  data_iso <- filter(data, geo == iso)
  
  if (nrow(data_iso) == 0) {
    
    caption <- str_glue(
      "O Banco Mundial não tem dados sobre as taxas de natalidade para a {name}."
    )
    
  } else {
    
    t0 <- min(data_iso$t) |> as.numeric()
    t1 <- max(data_iso$t) |> as.numeric()
    
    data_t1 <- filter(data, t == t1) |> pull(v)
    med_t1 <- median(data_t1, na.rm = TRUE)
    data_iso_t1 <- filter(data_iso, t == t1) |> pull(v)
    
    caption <- str_glue(paste(
      "O Banco Mundial relata que a taxa de natalidade em {t1} foi de",
      "#b[{pl(data_iso_t1)}] {get_pct(data_t1, data_iso_t1)} nascimenos vivos",
      "por 1000 habitantes. A mediana global foi de {pl(med_t1)}."
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
      "64 years) per 100 working-age persons in {t1}. The global median was",
      "{pl(med_t1)}."
    ))
  }
  
  return(caption)
}

caption_depend_pt <- function(iso) {
  
  name <- namer(iso, lang = "pt")
  
  data <- filter(gdidata::wdi, var == "depend")
  data_iso <- filter(data, geo == iso)
  
  if (nrow(data_iso) == 0) {
    
    caption <- str_glue(
      "O Banco Mundial não tem dados sobre o Taxa de dependência demográfica para {name}."
    )
    
  } else {
    
    t0 <- min(data_iso$t) |> as.numeric()
    t1 <- max(data_iso$t) |> as.numeric()
    
    data_t1 <- filter(data, t == t1) |> pull(v)
    med_t1 <- median(data_t1, na.rm = TRUE)
    data_iso_t1 <- filter(data_iso, t == t1) |> pull(v)
    
    caption <- str_glue(paste(
      "O Banco Mundial relata #b[{pl(data_iso_t1)}]",
      "{get_pct(data_t1, data_iso_t1)} dependentes (menores de 15, maiores de",
      "64 anos) por 100 pessoas em idade ativa em {t1}. A mediana global foi",
      "de {pl(med_t1)}."
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

caption_income_pt <- function(iso) {
  
  name <- namer(iso, lang = "pt")
  
  data <- filter(gdidata::wdi, var == "income")
  data_iso <- filter(data, geo == iso)
  
  if (nrow(data_iso) == 0) {
    
    caption <- str_glue(
      "O Banco Mundial não tem dados sobre o produto interno bruto per capita",
      "(PIB) para a{name}."
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
    
    caption <- str_glue(paste(
      "O Banco Mundial reporta que a {name} teve um PIB per capita de",
      "#b[{data_iso_t1_lab}] {get_pct(data_t1, data_iso_t1)}",
      "(em USD internacionais constantes de 2021) em {t1}. A mediana global",
      "foi de {pl_usd(med_t1)}."
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

caption_unem_pt <- function(iso) {
  
  name <- namer(iso, lang = "pt")
  
  data <- filter(gdidata::wdi, var == "unem")
  t1_max <- max(data$t) |> as.numeric()
  data_iso_latest <- filter(data, geo == iso & t > t1_max - 10)

  if (nrow(data_iso_latest) == 0) {
    
    caption <- str_glue(paste(
      "A OIT não tem dados sobre taxas de desemprego para a {name} nos últimos",
      "10 anos."
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
    
    caption <- str_glue(paste(
      "A OIT relata uma taxa de desemprego de",
      "#b[{pl_pct1(data_iso_t1_latest)}]",
      "{get_pct(data_t1_latest, data_iso_t1_latest)} em {t1_latest}.",
      "A mediana global foi de {pl_pct1(med_t1)}."
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

caption_inf_pt <- function(iso) {
  
  name <- namer(iso, lang = "pt")
  
  data <- filter(gdidata::wdi, var == "inf")
  t1_max <- max(data$t) |> as.numeric()
  data_iso_latest <- filter(data, geo == iso & t > t1_max - 10)
  
  if (nrow(data_iso_latest) == 0) {
    
    caption <- str_glue(paste(
      "O Banco Mundial não tem dados sobre taxas de inflação para a {name} nos",
      "últimos 10 anos."
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
    
    caption <- str_glue(paste(
      "O Banco Mundial reporta uma taxa de inflação de",
      "#b[{pl_pct1(data_iso_t1_latest)}]",
      "{get_pct(data_t1_latest, data_iso_t1_latest)} em",
      "{t1_latest}. A mediana global foi de {pl_pct1(med_t1)}."
    ))
  }
  
  return(caption)
}


