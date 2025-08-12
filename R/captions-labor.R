
library(tidyverse)
library(spatstat)
library(gdiviz)
library(gdidata)

label_occup <- function(id) {
  labels <- c()
  for (i in id) {
    labels <- c(
      labels, 
      occup_labels$occup_lab[occup_labels$occup == i]
    )
  }
  return(labels)
}


# Figure 1 ----------------------------------------------------------------

caption_immig <- function(iso) {
  
  name <- namer(iso)
  
  data <- filter(undesa_stocks, t == max(t))
  data_iso <- filter(data, geo == iso) |> 
    summarise(n = sum(n), .by = c(geo, from)) |> 
    mutate(v = n / sum(n)) |> 
    arrange(desc(n))
  
  if (nrow(data_iso) == 0) {
    
    caption <- str_glue("No data for {name}.")
    
  } else {
    
    income <- filter(wdi, var == "income" & t %in% 2010:2022) |> 
      summarise(income = mean(v), .by = geo)
    unem <- filter(wdi, var == "unem" & t %in% 2010:2022) |> 
      summarise(unem = mean(v), .by = geo)
    
    data_iso_var <- full_join(income, unem, by = "geo") |> 
      filter(geo == iso)
    
    data_from_var <- data_iso |> 
      left_join(income, by = c("from" = "geo")) |> 
      left_join(unem, by = c("from" = "geo")) |> 
      summarise(
        income = weighted.mean(income, w = v, na.rm = TRUE),
        unem = weighted.mean(unem, w = v, na.rm = TRUE)
      )
    
    data_iso_tot <- sum(data_iso$n)
    data_iso_top5 <- data_iso |> 
      filter(from != "OOO") |> 
      slice_head(n = 5) |> 
      mutate(name = countryname(from, to = "name_text"))
    
    caption <- str_glue(paste(
      " - There were #b[{ pl(data_iso_tot) }] immigrants in {name} in 2020.",
      "The top five countries of origin were #b[{ data_iso_top5$name[1] }]",
      "({ pl_pct(100 * data_iso_top5$v[1]) } of all immigrants),",
      "#b[{ data_iso_top5$name[2] }] ({ pl_pct(100 * data_iso_top5$v[2]) }),",
      "#b[{ data_iso_top5$name[3] }] ({ pl_pct(100 * data_iso_top5$v[3]) }),",
      "#b[{ data_iso_top5$name[4] }] ({ pl_pct(100 * data_iso_top5$v[4]) })",
      "and #b[{ data_iso_top5$name[5] }]",
      "({ pl_pct(100 * data_iso_top5$v[5]) }).\n",
      "#colbreak()\n",
      " - Immigrants moved from countries where annual per-capita GDP was on",
      "average #b[\\${ pl(data_from_var$income) }] (compared to",
      "\\${ pl(data_iso_var$income) } in {name}) and where unemployment was on",
      "average #b[{ pl_pct1(data_from_var$unem) }] (compared to",
      "{ pl_pct1(data_iso_var$unem) } in {name})."
    ))
  }
  
  return(caption)
}


# Figure 2 ----------------------------------------------------------------

caption_immpyr <- function(iso) {
  
  name <- namer(iso)
  
  t1 <- 2020
  
  data_gen_all <- filter(undesa_wpp, t == t1) |> 
    mutate(
      age = case_when(
        age %in% 0:14 ~ "children",
        age >= 65 ~ "elderly",
        .default = "working"
      )
    ) |> 
    summarise(n = sum(n), .by = c(geo, sex, age))
  data_gen <- data_gen_all |> 
    summarise(n = sum(n), .by = c(geo, age)) |> 
    mutate(v_gen = 100 * n / sum(n), .by = geo)
  data_gen_f <- data_gen_all |> 
    summarise(n = sum(n), .by = c(geo, sex)) |> 
    mutate(v_gen = 100 * n / sum(n), .by = geo)
  
  data_imm_all <- read_csv("data-raw/Immigrants_AgeSex.csv") |> 
    select(geo = Country, t = Year, sex = Sex, age = Age, n = Value) |> 
    filter(t == t1) |> 
    mutate(
      age = case_when(
        age %in% c("0-4", "5-9", "10-14") ~ "children",
        age %in% c("65-69", "70-74", "75+") ~ "elderly",
        .default = "working"
      )
    ) |> 
    summarise(n = sum(n), .by = c(geo, sex, age))
  data_imm <- filter(data_imm_all, sex == "total") |> 
    mutate(v_imm = 100 * n / sum(n), .by = geo)
  data_imm_f <- filter(data_imm_all, sex != "total") |> 
    summarise(n = sum(n), .by = c(geo, sex)) |> 
    mutate(v_imm = 100 * n / sum(n), .by = geo)
  
  data <- inner_join(data_gen, data_imm, by = c("geo", "age")) |> 
    select(geo, age, v_gen, v_imm) |> 
    pivot_longer(
      cols = starts_with("v_"), 
      names_to = "var", 
      names_prefix = "v_",
      values_to = "v"
    )
  data_f <- inner_join(data_gen_f, data_imm_f, by = c("geo", "sex")) |> 
    filter(sex == "female") |> 
    select(geo, v_gen, v_imm) |> 
    pivot_longer(
      cols = starts_with("v_"), 
      names_to = "var", 
      names_prefix = "v_",
      values_to = "v"
    )
  
  data_iso <- filter(data, geo == iso)
  
  if (nrow(drop_na(data_iso)) <= 3) {
    
    caption <- str_glue(
      "UN DESA has no information on the sex and age of migrants from {name}."
    )
    
  } else {
    
    data_gen_child <- filter(data, age == "children" & var == "gen") |> pull(v)
    data_gen_work <- filter(data, age == "working" & var == "gen") |> pull(v)
    data_gen_elder <- filter(data, age == "elderly" & var == "gen") |> pull(v)
    data_imm_child <- filter(data, age == "children" & var == "imm") |> pull(v)
    data_imm_work <- filter(data, age == "working" & var == "imm") |> pull(v)
    data_imm_elder <- filter(data, age == "elderly" & var == "imm") |> pull(v)
    
    data_gen_iso <- filter(data_iso, var == "gen")
    data_imm_iso <- filter(data_iso, var == "imm")
    data_gen_child_iso <- filter(data_gen_iso, age == "children") |> pull(v)
    data_gen_work_iso <- filter(data_gen_iso, age == "working") |> pull(v)
    data_gen_elder_iso <- filter(data_gen_iso, age == "elderly") |> pull(v)
    data_imm_child_iso <- filter(data_imm_iso, age == "children") |> pull(v)
    data_imm_work_iso <- filter(data_imm_iso, age == "working") |> pull(v)
    data_imm_elder_iso <- filter(data_imm_iso, age == "elderly") |> pull(v)
    
    data_gen_f <- filter(data_f, var == "gen") |> pull(v)
    data_imm_f <- filter(data_f, var == "imm") |> pull(v)
    data_gen_f_iso <- filter(data_f, var == "gen" & geo == iso) |> pull(v)
    data_imm_f_iso <- filter(data_f, var == "imm" & geo == iso) |> pull(v)
    
    diff_f <- data_gen_f_iso - data_imm_f_iso
    
    comparison <- "comparable to"
    if (diff_f > 2) comparison <- "#b[significantly less] than"
    if (diff_f < 0) comparison <- "#b[more] than"
    if (diff_f < 2) comparison <- "#b[significantly more] than"
    
    caption <- str_glue(paste(
      "- Among immigrants in {name} in {t1},", 
      "#b[{ pl_pct(data_imm_child_iso) }] were children (0-14 years old),",
      "#b[{ pl_pct(data_imm_work_iso) }] were working-age (15-64 years old),",
      "and",
      "#b[{ pl_pct(data_imm_elder_iso) }] were elderly (65 years and older)",
      
      "(compared to",
      "{ pl_pct(data_gen_child_iso) },",
      "{ pl_pct(data_gen_work_iso) }, and",
      "{ pl_pct(data_gen_elder_iso) }",
      "for the general population).\n",
      
      "- #b[{ pl_pct(data_imm_f_iso) }] of immigrants were female",
      "(compared to the { pl_pct(data_gen_f_iso) } for the general population)."
    ))
  }
  
  return(caption)
}


# Figure 3 ----------------------------------------------------------------

caption_immdep <- function(iso) {
  
  name <- namer(iso)
  
  data_gen <- filter(wdi, var == "depend") |> 
    select(geo, t, v_gen = v)
  
  # t1 <- max(undesa_stocks$t)
  t1 <- 2020
  
  depend_ages <- c("0-4", "5-9", "10-14", "65-69", "70-74", "75+")
  data_imm <- read_csv("data-raw/Immigrants_AgeSex.csv") |> 
    rename(geo = Country, t = Year, sex = Sex, age = Age, n = Value) |> 
    filter(sex == "total" & t == t1) |> 
    mutate(group = case_when(
      age %in% depend_ages ~ "dependent",
      .default = "working"
    )) |> 
    summarise(n = sum(n), .by = c(geo, t, group)) |> 
    pivot_wider(names_from = group, values_from = n) |> 
    mutate(v_imm = 100 * dependent / working) |> 
    select(geo, t, v_imm)
  
  data <- data_imm |> 
    left_join(data_gen, by = c("geo", "t")) |> 
    pivot_longer(
      cols = starts_with("v_"),
      names_to = "var",
      names_prefix = "v_",
      values_to = "v"
    )
  
  data_iso <- filter(data, geo == iso)
  
  if (nrow(drop_na(data_iso)) < 2) {
    
    caption <- str_glue(paste(
      "UN DESA and the World Bank has no data on age dependency ratios for",
      "{name} and its immigrants."
    ))
    
  } else {
    
    data_gen <- filter(data, var == "gen") |> pull(v)
    data_imm <- filter(data, var == "imm") |> pull(v)
    
    data_gen_iso <- filter(data_iso, var == "gen") |> pull(v)
    data_imm_iso <- filter(data_iso, var == "imm") |> pull(v)
    diff <- data_gen_iso - data_imm_iso
    
    comparison <- "comparable to"
    if (diff > 2) comparison <- "#b[less] than"
    if (diff < 2) comparison <- "#b[more] than"
    
    caption <- str_glue(paste(
      "- Among immigrants in {name}, there were",
      "#b[{ pl(data_imm_iso) }] dependents (younger than 15, older than 64)",
      "for every 100 working-age persons",
      "(compared to { pl(data_gen_iso) } dependents for the general population)."
    ))
  }
  
  return(caption)
}


# Figure 4 ----------------------------------------------------------------

caption_emp <- function(iso) {
  
  name <- namer(iso)
  
  data <- snap_data("emp")$data |> drop_na()
  data_iso <- filter(data, geo == iso) |> 
    filter(n() == 2, .by = c(geo, t))
  
  if (nrow(data_iso) == 0) {
    
    caption <- str_glue("ILO has no information on the labor force of {name}.")
    
  } else {
  
    data_iso_t1 <- filter(data_iso, t == max(t))
    t1 <- data_iso_t1$t[1]
    
    data_iso_tot <- sum(data_iso_t1$n)
    data_iso_for <- filter(data_iso_t1, str_detect(birth, "Foreign"))$n |> sum()
    data_iso_dom <- filter(data_iso_t1, str_detect(birth, "Native"))$n |> sum()
    data_iso_fsh <- 100 * data_iso_for / data_iso_tot
    data_iso_fsh_lab <- pl_pct(data_iso_fsh)
    if (data_iso_fsh < 9.5) data_iso_fsh_lab <- pl_pct1(data_iso_fsh)
    if (data_iso_fsh < .5) data_iso_fsh_lab <- "\\<1%"
    
    agg_text <- str_glue(paste(
      "#b[{ pl(data_iso_tot) }] people were employed in {name} in {t1}, of",
      "which #b[{ pl(data_iso_for) }] (or {data_iso_fsh_lab}) were",
      "foreign-born."
    ))
    
    span <- max(data_iso$t) - min(data_iso$t)
    
    if (span == 0) {
      
      caption <- str_glue("- {agg_text}")
      
    } else {
      
      # Choose comparison year
      
      lag <- 10
      
      if (span <= lag) {
        
        t0 <- min(data_iso$t)
        
      } else {
        
        t_vec <- sort(unique(data_iso$t), decreasing = TRUE)
        lags <- tibble(t = t_vec) |> 
          mutate(
            lags = cumsum(ifelse(is.na(lag(t)), 0, lag(t) - t)),
            dist = lags - lag
          ) |> 
          filter(abs(dist) == min(abs(dist)))
        
        if (nrow(lags) == 1) t0 <- lags$t
        if (nrow(lags) > 1) t0 <- min(lags$t)
        
      }
      
      dist <- t1 - t0
      data_iso_t0 <- filter(data_iso, t == t0)
      data_iso_dom_t0 <- filter(data_iso_t0, str_detect(birth, "Native"))$n
      data_iso_for_t0 <- filter(data_iso_t0, str_detect(birth, "Foreign"))$n
      
      dom_chg <- 100 * ((data_iso_dom / data_iso_dom_t0) ^ (1 / dist) - 1)
      for_chg <- 100 * ((data_iso_for / data_iso_for_t0) ^ (1 / dist) - 1)
      
      verb <- "increased"
      verb1 <- "an increase"
      if (for_chg < 0) verb <- "decreased"
      if (dom_chg < 0) verb1 <- "a decrease"
      
      change_text <- str_glue(paste(
        "Employment of foreign-borns has #b[{verb}] by an average",
        "#b[{ pl_pct1(for_chg) }] annually over {t0}–{t1} (compared to {verb1}",
        "of { pl_pct1(dom_chg) } for native-borns)."
      ))
      

      caption <- str_glue(paste(
        "- {agg_text}\n",
        "- {change_text}"
      ))
    }
  }
  
  return(caption)
}


# Figure 5 ----------------------------------------------------------------

caption_empeduc <- function(iso) {
  
  name <- namer(iso)
  
  data <- snap_data("empeduc")$data
  data_iso <- filter(data, geo == iso)
  
  if (nrow(data_iso) == 0) {
    
    caption <- ""
    
  } else {
    
    t1 <- snap_data("empeduc", iso)$range[2]

    educ_order <- snap_data("empeduc")$data$educ |> levels()
    educ_cats <- unique(filter(data_iso, birth == "Foreign-born") |> pull(educ))
    educ_cats <- educ_cats[order(match(educ_cats, educ_order))]
    
    data_educ <- data_iso |> 
      summarise(n = sum(n), .by = c(birth, educ)) |> 
      mutate(educ_sh = 100 * n / sum(n), .by = birth)
    
    if (length(educ_cats) == 1) {
      
      educ_text <- 
        "There is no information on the education levels of employed foreign-borns."
    }
    
    if (length(educ_cats) > 1) {
      
      data_educ_dom <- filter(data_educ, str_detect(birth, "Native"))
      data_educ_for <- filter(data_educ, str_detect(birth, "Foreign"))
      
      sh1_dom <- filter(data_educ_dom, educ == educ_cats[1])
      sh2_dom <- filter(data_educ_dom, educ == educ_cats[2])
      sh3_dom <- filter(data_educ_dom, educ == educ_cats[3])
      sh1_for <- filter(data_educ_for, educ == educ_cats[1])
      sh2_for <- filter(data_educ_for, educ == educ_cats[2])
      sh3_for <- filter(data_educ_for, educ == educ_cats[3])
      
      # Intermediate, Advanced
      
      if (length(educ_cats) == 2) {
        
        educ_text <- str_glue(paste(
          "#b[{ pl_pct(sh1_for$educ_sh) }] of employed foreign-borns",
          "had { tolower(sh1_for$educ) } education and",
          
          "#b[{ pl_pct(sh2_for$educ_sh) }]",
          "had { tolower(sh2_for$educ) } education in {t1}",
          
          "(compared to",
          "{ pl_pct(sh1_dom$educ_sh) } and",
          "{ pl_pct(sh2_dom$educ_sh) },",
          "respectively, for employed native-borns)."
        ))
      }
      
      # Basic, Intermediate, Advanced
      
      if (length(educ_cats) == 3) {
        
        educ_text <- str_glue(paste(
          "#b[{ pl_pct(sh1_for$educ_sh) }] of employed foreign-borns",
          "had { tolower(sh1_for$educ) } education,",
          
          "#b[{ pl_pct(sh2_for$educ_sh) }]",
          "had { tolower(sh2_for$educ) } education, and",
          
          "#b[{ pl_pct(sh3_for$educ_sh) }]",
          "had { tolower(sh3_for$educ) } education in {t1}",
          
          "(compared to",
          "{ pl_pct(sh1_dom$educ_sh) },",
          "{ pl_pct(sh2_dom$educ_sh) } and",
          "{ pl_pct(sh3_dom$educ_sh) },",
          "respectively, for employed native-borns)."
        ))
      }
      
      # Basic, Intermediate, Advanced, Unknown
      # Less than basic, Basic, Advanced, Unknown
      
      if (length(educ_cats) == 4 & educ_cats[4] == "Unknown") {
        
        educ_text <- str_glue(paste(
          "#b[{ pl_pct(sh1_for$educ_sh) }] of employed foreign-borns",
          "had { tolower(sh1_for$educ) } education,",
          
          "#b[{ pl_pct(sh2_for$educ_sh) }]",
          "had { tolower(sh2_for$educ) } education, and",
          
          "#b[{ pl_pct(sh3_for$educ_sh) }]",
          "had { tolower(sh3_for$educ) } education in {t1}",
          
          "(compared to",
          "{ pl_pct(sh1_dom$educ_sh) },",
          "{ pl_pct(sh2_dom$educ_sh) }, and",
          "{ pl_pct(sh3_dom$educ_sh) },",
          "respectively, for employed native-borns)."
        ))
      }
      
      # Less than basic, Basic, Intermediate, Advanced
      
      if (length(educ_cats) == 4 & educ_cats[4] != "Unknown") {
        
        sh4_dom <- filter(data_educ_dom, educ == educ_cats[4])
        sh4_for <- filter(data_educ_for, educ == educ_cats[4])
        
        educ_text <- str_glue(paste(
          "#b[{ pl_pct(sh1_for$educ_sh) }] of employed foreign-borns",
          "had { tolower(sh1_for$educ) } education,",
          
          "#b[{ pl_pct(sh2_for$educ_sh) }]",
          "had { tolower(sh2_for$educ) } education,",
          
          "#b[{ pl_pct(sh3_for$educ_sh) }]",
          "had { tolower(sh3_for$educ) } education, and",
          
          "#b[{ pl_pct(sh4_for$educ_sh) }]",
          "had { tolower(sh4_for$educ) } education in {t1}",
          
          "(compared to",
          "{ pl_pct(sh1_dom$educ_sh) },",
          "{ pl_pct(sh2_dom$educ_sh) },",
          "{ pl_pct(sh3_dom$educ_sh) }, and",
          "{ pl_pct(sh4_dom$educ_sh) },",
          "respectively, for employed native-borns)."
        ))
      }
      
      # Less than basic, Basic, Intermediate, Advanced, Unknown
      
      if (length(educ_cats) == 5) {
        
        sh4_dom <- filter(data_educ_dom, educ == educ_cats[4])
        sh4_for <- filter(data_educ_for, educ == educ_cats[4])
        
        educ_text <- str_glue(paste(
          "#b[{ pl_pct(sh1_for$educ_sh) }] of employed foreign-borns",
          "had { tolower(sh1_for$educ) } education,",
          
          "#b[{ pl_pct(sh2_for$educ_sh) }]",
          "had { tolower(sh2_for$educ) } education,",
          
          "#b[{ pl_pct(sh3_for$educ_sh) }]",
          "had { tolower(sh3_for$educ) } education, and",
          
          "#b[{ pl_pct(sh4_for$educ_sh) }]",
          "had { tolower(sh4_for$educ) } education in {t1}",
          
          "(compared to",
          "{ pl_pct(sh1_dom$educ_sh) },",
          "{ pl_pct(sh2_dom$educ_sh) },",
          "{ pl_pct(sh3_dom$educ_sh) }, and",
          "{ pl_pct(sh4_dom$educ_sh) },",
          "respectively, for employed native-borns)."
        ))
      }
    }
    
    data_sex <- data_iso |>
      summarise(n = sum(n), .by = c(geo, sex, birth)) |> 
      mutate(sh = 100 * n / sum(n), .by = c(geo, birth))
    data_sex_dom <- filter(data_sex, str_detect(birth, "Native"))
    data_sex_for <- filter(data_sex, str_detect(birth, "Foreign"))

    sh_f_dom <- filter(data_sex_dom, sex == "Female") |> pull(sh)
    sh_f_for <- filter(data_sex_for, sex == "Female") |> pull(sh)

    data_sex_iso <- filter(data_sex, geo == iso)
    data_sex_dom_iso <- filter(data_sex_iso, str_detect(birth, "Native"))
    data_sex_for_iso <- filter(data_sex_iso, str_detect(birth, "Foreign"))

    sh_f_dom_iso <- filter(data_sex_dom_iso, sex == "Female") |> pull(sh)
    sh_f_for_iso <- filter(data_sex_for_iso, sex == "Female") |> pull(sh)

    sex_text <- str_glue(paste(
      "#b[{ pl_pct(sh_f_for_iso) }] of employed foreign-borns were female",
      "(compared to { pl_pct(sh_f_dom_iso) } for native-borns)."
    ))
    
    caption <- str_glue(paste(
      "- {educ_text}\n",
      "- {sex_text}"
    ))
  }
  
  return(caption)
}


# Figure 6 ----------------------------------------------------------------

caption_unemrate <- function(iso) {
  
  name <- namer(iso)
  
  data <- snap_data("unemrate")$data
  data_iso <- filter(data, geo == iso)
  
  if (nrow(data_iso) == 0) {
    
    caption <- str_glue("The ILO has no data on unemployment rates for {name}.")
    
  } else {
    
    t1 <- max(data_iso$t)
    span <- max(data_iso$t) - min(data_iso$t)
    data_iso_t1 <- filter(data_iso, t == max(t))
    data_iso_dom_t1 <- filter(data_iso_t1, str_detect(birth, "Native"))$v
    data_iso_for_t1 <- filter(data_iso_t1, str_detect(birth, "Foreign"))$v
    
    current_text <- str_glue(paste(
      "In {name}, the unemployment rate among foreign-borns was",
      "#b[{ pl_pct1(data_iso_for_t1) }] in {t1} (compared to",
      "{ pl_pct1(data_iso_dom_t1) } among native-borns)."
    ))
    
    if (span == 0) {
      
      caption <- str_glue("- {current_text}")
      
    } else {
      
      # Choose comparison year
      lag <- 1
      if (span <= lag) t0 <- min(data_iso$t)
      
      else {
        
        t_vec <- sort(unique(data_iso$t), decreasing = TRUE)
        lags <- tibble(t = t_vec) |> 
          mutate(
            lags = cumsum(ifelse(is.na(lag(t)), 0, lag(t) - t)),
            dist = lags - lag
          ) |> 
          filter(dist >= 0) |> 
          filter(dist == min(dist))
        
        if (nrow(lags) == 1) t0 <- lags$t
        if (nrow(lags) > 1) t0 <- min(lags$t)
      }
      
      data_iso_t0 <- filter(data_iso, t == t0)
      data_iso_dom_t0 <- filter(data_iso_t0, str_detect(birth, "Native"))$v
      data_iso_for_t0 <- filter(data_iso_t0, str_detect(birth, "Foreign"))$v
      
      dom_chg <- data_iso_dom_t1 - data_iso_dom_t0
      for_chg <- data_iso_for_t1 - data_iso_for_t0
      
      change_text <- str_glue(paste(
        "This was a #b[{ pl1(for_chg) }-point] change",
        "between {t0} and {t1}",
        "(compared to a { pl1(dom_chg) }-change for native-borns)."
      ))
      
      caption <- str_glue(paste(
        "- {current_text}\n",
        "- {change_text}"
      ))
    }
  }
  
  return(caption)
}


# Figure 7 ----------------------------------------------------------------

caption_unemeduc <- function(iso) {
  
  name <- namer(iso)
  data <- snap_data("unemeduc")$data
  data_iso <- filter(data, geo == iso)
  data_iso_for <- filter(data_iso, birth == "Foreign-born")
  
  if (nrow(data_iso) == 0) {
    
    caption <- ""
    
  } else if (nrow(data_iso_for) == 0) {
    
    caption <- 
      "There is no information on the unemployment rates of foreign-borns."
    
  } else {
    
    t1 <- snap_data("unemeduc", iso)$range[2]
    data_educ <- filter(data_iso, sex == "Total", educ != "Total")
    
    educ_order <- levels(snap_data("unemeduc")$data$educ)
    educ_order <- educ_order[educ_order != "Total"]
    
    educ_cats <- unique(filter(data_educ, birth == "Foreign-born")$educ)
    educ_cats <- educ_cats[order(match(educ_cats, educ_order))]
    
    data_educ_dom <- filter(data_educ, str_detect(birth, "Native")) |> 
      complete(
        geo,
        birth = "Native-born",
        sex = "Total",
        educ = educ_cats,
        t,
        fill = list(v = 0)
      )
    data_educ_for <- filter(data_educ, str_detect(birth, "Foreign"))
    
    sh1_dom <- filter(data_educ_dom, educ == educ_cats[1])
    sh2_dom <- filter(data_educ_dom, educ == educ_cats[2])
    sh3_dom <- filter(data_educ_dom, educ == educ_cats[3])
    sh1_for <- filter(data_educ_for, educ == educ_cats[1])
    sh2_for <- filter(data_educ_for, educ == educ_cats[2])
    sh3_for <- filter(data_educ_for, educ == educ_cats[3])
    
    # Intermediate
    
    if (length(educ_cats) == 1) {
      
      educ_text <- str_glue(paste(
        "The unemployment rate among foreign-borns in {name} was",
        
        "#b[{ pl_pct(sh1_for$v) }]",
        "for those with { tolower(sh1_for$educ) } education in {t1}",
        
        "(compared to",
        "{ pl_pct(sh1_dom$v) } for employed native-borns)."
      ))
    }
    
    if (length(educ_cats) == 2) {
      
      educ_text <- str_glue(paste(
        "The unemployment rate among foreign-borns in {name} was",
        
        "#b[{ pl_pct(sh1_for$v) }]",
        "for those with { tolower(sh1_for$educ) } education and",
        
        "#b[{ pl_pct(sh2_for$v) }]",
        "for those with { tolower(sh2_for$educ) } education in {t1}",
        
        "(compared to",
        "{ pl_pct(sh1_dom$v) } and",
        "{ pl_pct(sh2_dom$v) }, respectively, for employed native-borns)."
      ))
    }
    
    # Basic, Intermediate, Advanced
    
    if (length(educ_cats) == 3) {
      
      educ_text <- str_glue(paste(
        "The unemployment rate among foreign-borns in {name} was",
        
        "#b[{ pl_pct(sh1_for$v) }]",
        "for those with { tolower(sh1_for$educ) } education,",
        
        "#b[{ pl_pct(sh2_for$v) }]",
        "for those with { tolower(sh2_for$educ) } education, and",
        
        "#b[{ pl_pct(sh3_for$v) }]",
        "for those with { tolower(sh3_for$educ) } education in {t1}",
        
        "(compared to",
        "{ pl_pct(sh1_dom$v) },",
        "{ pl_pct(sh2_dom$v) }, and",
        "{ pl_pct(sh3_dom$v) }, respectively, for employed native-borns)."
      ))
    }
    
    # Basic, Intermediate, Advanced, Unknown
    # Less than basic, Basic, Advanced, Unknown
    
    if (length(educ_cats) == 4 & educ_cats[4] == "Unknown") {
      
      educ_text <- str_glue(paste(
        "The unemployment rate among foreign-borns in {name} was",
        
        "#b[{ pl_pct(sh1_for$v) }]",
        "for those with { tolower(sh1_for$educ) } education,",
        
        "#b[{ pl_pct(sh2_for$v) }]",
        "for those with { tolower(sh2_for$educ) } education, and",
        
        "#b[{ pl_pct(sh3_for$v) }]",
        "for those with { tolower(sh3_for$educ) } education in {t1}",
        
        "(compared to",
        "{ pl_pct(sh1_dom$v) },",
        "{ pl_pct(sh2_dom$v) }, and",
        "{ pl_pct(sh3_dom$v) },",
        "respectively, for employed native-borns).",
        
        "The remaining shares had unknown educational levels."
      ))
    }
    
    # Less than basic, Basic, Intermediate, Advanced
    
    if (length(educ_cats) == 4 & educ_cats[4] != "Unknown") {
      
      sh4_dom <- filter(data_educ_dom, educ == educ_cats[4])
      sh4_for <- filter(data_educ_for, educ == educ_cats[4])
      
      educ_text <- str_glue(paste(
        "The unemployment rate among foreign-borns in {name} was",
        
        "#b[{ pl_pct(sh1_for$v) }]",
        "for those with { tolower(sh1_for$educ) } education,",
        
        "#b[{ pl_pct(sh2_for$v) }]",
        "for those with { tolower(sh2_for$educ) } education,",
        
        "#b[{ pl_pct(sh3_for$v) }]",
        "for those with { tolower(sh3_for$educ) } education, and",
        
        "#b[{ pl_pct(sh4_for$v) }]",
        "for those with { tolower(sh4_for$educ) } education in {t1}",
        
        "(compared to",
        "{ pl_pct(sh1_dom$v) },",
        "{ pl_pct(sh2_dom$v) },",
        "{ pl_pct(sh3_dom$v) }, and",
        "{ pl_pct(sh4_dom$v) }, respectively, for employed native-borns)."
      ))
    }
    
    # Less than basic, Basic, Intermediate, Advanced, Unknown
    
    if (length(educ_cats) == 5) {
      
      sh4_dom <- filter(data_educ_dom, educ == educ_cats[4])
      sh4_for <- filter(data_educ_for, educ == educ_cats[4])
      
      educ_text <- str_glue(paste(
        "The unemployment rate among foreign-borns in {name} was",
        
        "#b[{ pl_pct(sh1_for$v) }]",
        "for those with { tolower(sh1_for$educ) } education,",
        
        "#b[{ pl_pct(sh2_for$v) }]",
        "for those with { tolower(sh2_for$educ) } education,",
        
        "#b[{ pl_pct(sh3_for$v) }]",
        "for those with { tolower(sh3_for$educ) } education, and",
        
        "#b[{ pl_pct(sh4_for$v) }]",
        "for those with { tolower(sh4_for$educ) } education in {t1}",
        
        "(compared to",
        "{ pl_pct(sh1_dom$v) },",
        "{ pl_pct(sh2_dom$v) },",
        "{ pl_pct(sh3_dom$v) }, and",
        "{ pl_pct(sh4_dom$v) }, respectively, for employed native-borns).",
        
        "The remaining shares had unknown educational levels."
      ))
    }
    
    data_sex <- filter(data_iso, sex != "Total", educ == "Total")
    data_sex_dom <- filter(data_sex, str_detect(birth, "Native"))
    data_sex_for <- filter(data_sex, str_detect(birth, "Foreign"))
    
    sh_f_dom <- filter(data_sex_dom, sex == "Female")$v
    sh_f_for <- filter(data_sex_for, sex == "Female")$v
    
    data_sex_iso <- filter(data_sex, geo == iso)
    data_sex_dom_iso <- filter(data_sex_iso, str_detect(birth, "Native"))
    data_sex_for_iso <- filter(data_sex_iso, str_detect(birth, "Foreign"))
    
    sh_f_dom_iso <- filter(data_sex_dom_iso, sex == "Female")$v
    sh_f_for_iso <- filter(data_sex_for_iso, sex == "Female")$v
    
    sex_text <- str_glue(paste(
      "The unemployment rate among female foreign-borns was",
      "#b[{ pl_pct(sh_f_for_iso) }]",
      "(compared to { pl_pct(sh_f_dom_iso) } for native-borns)."
    ))
    
    caption <- str_glue(paste(
      "- {educ_text}\n",
      "- {sex_text}"
    ))
  }
  
  return(caption)
}


# Figure 8 ----------------------------------------------------------------

caption_earnings <- function(iso) {
  
  name <- namer(iso)
  data <- snap_data("earnings")$data |> drop_na()
  data_iso <- filter(data, geo == iso)
  
  if (nrow(data_iso) == 0) {
    
    caption <- str_glue("ILO has no information on earnings in {name}.")
    
  } else {
    
    dom_text <- "native-borns"
    for_text <- "foreign-borns"
    if (data_iso$type[1] == "citizenship") {
      dom_text <- "native citizens"
      for_text <- "foreign citizens"
    }
    
    data_iso_t1 <- filter(data_iso, t == max(t))
    t1 <- data_iso_t1$t[1]
    
    data_iso_for <- filter(data_iso_t1, status == "Foreign")$v
    data_iso_dom <- filter(data_iso_t1, status == "Native")$v
    
    agg_text <- str_glue(paste(
      "In {name}, the average monthly earnings of {for_text} was",
      "#b[{ pl_usd(data_iso_for) }]",
      
      "(compared to #b[{ pl_usd(data_iso_dom) }] for { dom_text }."
    ))
    
    # Comparison with previous year
    # Choose value from lag years ago. If not available, choose value from
    # closest year. If dataset only has one year, skip this part of the caption.
    
    lag <- 10
    span <- max(data_iso$t) - min(data_iso$t)
    
    if (span == 0) {
      
      caption <- str_glue("- {agg_text}")
      
    } else {
      
      if (span <= lag) {
        
        t0 <- min(data_iso$t)
        
      } else {
        
        t_vec <- sort(unique(data_iso$t), decreasing = TRUE)
        lags <- tibble(t = t_vec) |> 
          mutate(
            lags = cumsum(ifelse(is.na(lag(t)), 0, lag(t) - t)),
            dist = lags - lag
          ) |> 
          filter(abs(dist) == min(abs(dist)))
        
        if (nrow(lags) == 1) t0 <- lags$t
        if (nrow(lags) > 1) t0 <- min(lags$t)
      }
      
      dist <- t1 - t0
      data_iso_t0 <- filter(data_iso, t == t0)
      data_iso_dom_t0 <- filter(data_iso_t0, status == "Native")$v
      data_iso_for_t0 <- filter(data_iso_t0, status == "Foreign")$v
      
      dom_chg <- 100 * ((data_iso_dom / data_iso_dom_t0) ^ (1 / dist) - 1)
      for_chg <- 100 * ((data_iso_for / data_iso_for_t0) ^ (1 / dist) - 1)
      
      verb <- "increased"
      verb1 <- "an increase"
      if (for_chg < 0) verb <- "decreased"
      if (dom_chg < 0) verb1 <- "a decrease"
      
      change_text <- str_glue(paste(
        "Earnings of { for_text  } have #b[{verb}] by an average",
        "#b[{ pl_pct1(for_chg) }] annually over {t0}–{t1} (compared to {verb1}",
        "of { pl_pct1(dom_chg) } for { dom_text })."
      ))
      
      caption <- str_glue(paste(
        "- {agg_text}\n",
        "- {change_text}"
      ))
    }
  }
  
  return(caption)
}


# Figure 9 ----------------------------------------------------------------

caption_hours <- function(iso) {
  
  name <- namer(iso)
  data <- snap_data("hours")$data |> drop_na()
  data_iso <- filter(data, geo == iso) |> 
    filter(n() == 2, .by = c(geo, t))
  
  if (nrow(data_iso) == 0) {
    
    caption <- str_glue("ILO has no information on working hours in {name}.")
    
  } else {
    
    dom_text <- "native-borns"
    for_text <- "foreign-borns"
    if (data_iso$type[1] == "citizenship") {
      dom_text <- "native citizens"
      for_text <- "foreign citizens"
    }
    
    data_iso_t1 <- filter(data_iso, t == max(t))
    t1 <- data_iso_t1$t[1]
    
    data_iso_for <- filter(data_iso_t1, status == "Foreign")$v
    data_iso_dom <- filter(data_iso_t1, status == "Native")$v
    
    agg_text <- str_glue(paste(
      "In {name}, {for_text} worked an average",
      "#b[{ pl1(data_iso_for) }] hours per week",

      "(compared to #b[{ pl1(data_iso_dom) }] hours for { dom_text }."
    ))
    
    # Comparison with previous year
    # Choose value from lag years ago. If not available, choose value from
    # closest year. If dataset only has one year, skip this part of the caption.
    
    lag <- 10
    span <- max(data_iso$t) - min(data_iso$t)
    
    if (span == 0) {
      
      caption <- str_glue("- {agg_text}")
      
    } else {
      
      if (span <= lag) {
        
        t0 <- min(data_iso$t)
        
      } else {
        
        t_vec <- sort(unique(data_iso$t), decreasing = TRUE)
        lags <- tibble(t = t_vec) |> 
          mutate(
            lags = cumsum(ifelse(is.na(lag(t)), 0, lag(t) - t)),
            dist = lags - lag
          ) |> 
          filter(abs(dist) == min(abs(dist)))
        
        if (nrow(lags) == 1) t0 <- lags$t
        if (nrow(lags) > 1) t0 <- min(lags$t)
      }
      
      dist <- t1 - t0
      data_iso_t0 <- filter(data_iso, t == t0)
      data_iso_dom_t0 <- filter(data_iso_t0, status == "Native")$v
      data_iso_for_t0 <- filter(data_iso_t0, status == "Foreign")$v
      
      dom_chg <- 100 * ((data_iso_dom / data_iso_dom_t0) ^ (1 / dist) - 1)
      for_chg <- 100 * ((data_iso_for / data_iso_for_t0) ^ (1 / dist) - 1)
      
      verb <- "increased"
      verb1 <- "an increase"
      if (for_chg < 0) verb <- "decreased"
      if (dom_chg < 0) verb1 <- "a decrease"
      
      change_text <- str_glue(paste(
        "Weekly hours worked by { for_text } have #b[{verb}] by an average",
        "#b[{ pl_pct1(for_chg) }] annually over {t0}–{t1} (compared to {verb1}",
        "of { pl_pct1(dom_chg) } for { dom_text })."
      ))
      
      caption <- str_glue(paste(
        "- {agg_text}\n",
        "- {change_text}"
      ))
    }
  }
  
  return(caption)
}


# Figure 10 ---------------------------------------------------------------

caption_empsector <- function(iso) {
  
  name <- namer(iso)
  
  data <- snap_data("empsector")$data
  data_iso <- filter(data, geo == iso)
  
  if (nrow(data_iso) == 0) {
    
    caption <- str_glue(
      "#caption(ncol: 1)[",
      "Eurostat has no information on employment by sector for {name}.",
      "]"
    )
    
  } else {
    
    t1 <- data_iso$t[1]
    
    share <- data_iso |> 
      left_join(sectors, by = c("sector" = "code")) |> 
      mutate(
        label = tolower(label), 
        v = 100 * v,
      ) |> 
      arrange(desc(v)) |> 
      filter(sector != "T-X") |> 
      slice_head(n = 5) |> 
      mutate(lab = prettylabel(v, pct = TRUE))
    
    caption <- str_glue(paste(
      "#caption(ncol: 1)[",
      "The five sectors in {name} with the highest proportions of",
      "foreign-borns among all employees were",
      "#b[{ share$label[1] }] ({ share$lab[1] }),",
      "#b[{ share$label[2] }] ({ share$lab[2] }),",
      "#b[{ share$label[3] }] ({ share$lab[3] }),",
      "#b[{ share$label[4] }] ({ share$lab[4] }), and",
      "#b[{ share$label[5] }] ({ share$lab[5] }).",
      "]"
    ))
  }
  
  return(caption)
}


# Figure 11 ---------------------------------------------------------------

caption_gva <- function(iso) {
  
  name <- namer(iso)
  data <- snap_data("gva", iso)$data
  
  if (nrow(data) == 0) {
    
    caption <- str_glue(
      "UNSD has no information on value added by sector in {name}."
    )
    
  } else {
    
    t0 <- snap_data("gva", iso)$range[1]
    t1 <- snap_data("gva", iso)$range[2]
    
    data_iso <- data |> 
      mutate(
        label = tolower(sector),
        sh = 100 * v / sum(v),
        .by = c(geo, t)
      ) |> 
      mutate(
        gr = 100 * (v / lag(v) - 1),
        .by = c(geo, sector)
      )
      
    va <- data_iso |> 
      filter(t == max(t)) |> 
      arrange(desc(v)) |> 
      slice_head(n = 3)
    
    growth <- data_iso |> 
      filter(!is.na(gr)) |> 
      arrange(desc(v)) |> 
      slice_head(n = 3)
    
    caption <- str_glue(paste(
      "- The three largest sectors in {name} by value added were",
      "#b[{ va$label[1] }]",
      "({ pl_pct(va$sh[1]) } of total value added),",
      "#b[{ va$label[2] }]",
      "({ pl_pct(va$sh[2]) }), and",
      "#b[{ va$label[3] }]",
      "({ pl_pct(va$sh[3]) }) in {t1}.\n",
      
      "- The three sectors with the highest growth over {t0}–{t1} were",
      "#b[{ growth$label[1] }]",
      "(average { pl_pct(growth$gr[1]) } annual growth),",
      "#b[{ growth$label[2] }]",
      "({ pl_pct(growth$gr[2]) }), and",
      "#b[{ growth$label[3] }]",
      "({ pl_pct(growth$gr[3]) })."
    ))
  }
  
  return(caption)
}


# Figure 12 ---------------------------------------------------------------

caption_exports <- function(iso) {
  
  name <- namer(iso)
  data <- snap_data("exports", iso)$data
  
  if (sum(data$v) == 0) {
    
    caption <- str_glue(
      "UNSD has no information on exports by sector in {name}."
    )
    
  } else {
    
    t0 <- snap_data("exports", iso)$range[1]
    t1 <- snap_data("exports", iso)$range[2]
    
    data_iso <- data |> 
      mutate(
        label = tolower(sector),
        sh = 100 * v / sum(v),
        .by = c(geo, t)
      ) |> 
      mutate(
        gr = 100 * (v / lag(v) - 1),
        .by = c(geo, sector)
      )
    
    va <- data_iso |> 
      filter(t == max(t)) |> 
      arrange(desc(v)) |> 
      slice_head(n = 3)
    
    growth <- data_iso |> 
      filter(!is.na(gr)) |> 
      arrange(desc(v)) |> 
      slice_head(n = 3)
    
    caption <- str_glue(paste(
      "- The three largest sectors in {name} by export value were",
      "#b[{ va$label[1] }]",
      "({ pl_pct(va$sh[1]) } of total exports),",
      "#b[{ va$label[2] }]",
      "({ pl_pct(va$sh[2]) }), and",
      "#b[{ va$label[3] }]",
      "({ pl_pct(va$sh[3]) }) in {t1}.\n",
      
      "- The three sectors with the highest growth over {t0}–{t1} were",
      "#b[{ growth$label[1] }]",
      "(average { pl_pct(growth$gr[1]) } annual growth),",
      "#b[{ growth$label[2] }]",
      "({ pl_pct(growth$gr[2]) }), and",
      "#b[{ growth$label[3] }]",
      "({ pl_pct(growth$gr[3]) })."
    ))
  }
  
  return(caption)
}


# Figure 13 ---------------------------------------------------------------

caption_empoccup <- function(iso) {
  
  name <- namer(iso)
  data <- snap_data("empoccup", iso)$data

  if (nrow(data) == 0) {
    
    caption <- str_glue(
      "ILO has no information on employment by skill level in {name}."
    )
    
  } else {
    
    t1 <- data$t[1]
    
    data_iso <- data |> 
      left_join(sectors, by = c("sector" = "code")) |> 
      mutate(label = tolower(label))
    
    data_agg <- data_iso |> 
      summarise(n = sum(n), .by = c(geo, t, sector, label)) |> 
      mutate(v = 100 * n / sum(n)) |> 
      arrange(desc(v)) |> 
      slice_head(n = 3)
    
    data_high <- data_iso |> 
      filter(skill == "high") |> 
      mutate(v = 100 * n / sum(n)) |> 
      arrange(desc(v)) |> 
      slice_head(n = 3)
    
    caption <- str_glue(paste(
      "- The three largest sectors in {name} by employment were",
      "#b[{ data_agg$label[1] }]",
      "({ pl_pct(data_agg$v[1]) } of total employment),",
      "#b[{ data_agg$label[2] }]",
      "({ pl_pct(data_agg$v[2]) }), and",
      "#b[{ data_agg$label[3] }]",
      "({ pl_pct(data_agg$v[3]) }) in {t1}.\n",
      
      "- High-skilled employees were employed in",
      "#b[{ data_high$label[1] }]",
      "({ pl_pct(data_high$v[1]) } of all high-skilled employees),",
      "#b[{ data_high$label[2] }]",
      "({ pl_pct(data_high$v[2]) }), and",
      "#b[{ data_high$label[3] }]",
      "({ pl_pct(data_high$v[3]) })."
    ))
  }
  
  return(caption)
}


# Figure 14 ---------------------------------------------------------------

caption_unemoccup <- function(iso) {
  
  name <- namer(iso)
  data <- snap_data("unemoccup", iso)$data
  
  if (nrow(data) == 0) {
    
    caption <- str_glue(
      "ILO has no information on unemployment by sector in {name}."
    )
    
  } else {
    
    t1 <- data$t[1]
    
    data <- data |> 
      arrange(desc(n)) |> 
      left_join(sectors, by = c("sector" = "code"))
    
    labels <- tolower(data$label)
    shares <- 100 * data$v
    
    caption <- str_glue(paste(
      "- Most unemployed persons in {name} previously worked in",
      "#b[{ labels[1] }] ({ pl_pct(shares[1]) } of all unemployed persons),",
      "followed by",
      "#b[{ labels[2] }] ({ pl_pct(shares[2]) })",
      "in {t1}."
    ))
  }
  
  return(caption)
}



# Fig. 15. Employment by skill --------------------------------------------

caption_empskill <- function(iso) {
  
  name <- namer(iso)
  data <- snap_data("empskill", iso)$data
  
  if (nrow(data) == 0) {
    
    caption <- str_glue(
      "#caption(ncol: 1)",
      "[ILO has no information on employment by skill in {name}.]"
    )
    
  } else {
    
    # data <- data |> 
    #   summarise(n = sum(n), .by = c(geo, t, skill, var)) |> 
    #   mutate(v = 100 * n / sum(n), .by = c(geo, t, var))
    data <- data |> 
      summarise(v = 100 * sum(v), .by = c(geo, t, skill, var))
    
    data_iso <- filter(data, var == "iso")
    data_imm <- filter(data, var == "imm")
    data_orig <- filter(data, var == "orig")
    data_world <- filter(data, var == "world")
    
    high_iso <- filter(data_iso, skill == "High-skill")$v
    med_iso <- filter(data_iso, skill == "Medium-skill")$v
    low_iso <- filter(data_iso, skill == "Low-skill")$v
    
    high_imm <- filter(data_imm, skill == "High-skill")$v
    med_imm <- filter(data_imm, skill == "Medium-skill")$v
    low_imm <- filter(data_imm, skill == "Low-skill")$v
    
    high_orig <- filter(data_orig, skill == "High-skill")$v
    med_orig <- filter(data_orig, skill == "Medium-skill")$v
    low_orig <- filter(data_orig, skill == "Low-skill")$v
    
    high_world <- filter(data_world, skill == "High-skill")$v
    med_world <- filter(data_world, skill == "Medium-skill")$v
    low_world <- filter(data_world, skill == "Low-skill")$v

    caption <- str_glue(paste(
      "#caption(ncol: 2)[",
      "- #b[{ pl_pct(high_iso) }],",
      "#b[{ pl_pct(med_iso) }], and",
      "#b[{ pl_pct(low_iso) }]", 
      "of all employed persons in {name} are categorized as",
      "high-skilled, medium-skilled, and low-skilled,",
      
      "compared to",
      "{ pl_pct(high_imm) },",
      "{ pl_pct(med_imm) }, and",
      "{ pl_pct(low_imm) }", 
      "of foreign-borns in {name}.\n",
      
      "#colbreak()\n",
      
      "- In the origin countries from which immigrants moved to",
      "{name} (see Fig. 1), an average", 
      "#b[{ pl_pct(high_orig) }],",
      "#b[{ pl_pct(med_orig) }], and",
      "#b[{ pl_pct(low_orig) }]",
      "of employed persons are categorized as high-, medium- and low-skilled,",
      
      "compared to",
      "{ pl_pct(high_world) },",
      "{ pl_pct(med_world) }, and",
      "{ pl_pct(low_world) }",
      "in countries worldwide.",
      "]"
    ))
  }
  
  return (caption)
}



# Figure 20 ---------------------------------------------------------------

caption_empcountry <- function(iso, row = 1) {
  
  name <- namer(iso)
  data <- snap_data("empcountry", iso)$data
  
  if (nrow(data) == 0) {
    
    caption <- str_glue(
      "#caption(ncol: 1)",
      "[ILO has no information on employment by sector in {name}.]"
    )
    
  } else {
    
    top_sectors <- levels(data$sector)
    
    top_origins <- snap_data("immorig", iso, use_2020 = TRUE)$data |> 
      filter(from != "Others") |> 
      pull(from)
    
    get_orig_emp <- function(m) {
      
      country_sector_emp <- empoccup |> 
        summarise(n = sum(n), .by = c(geo, t, sector)) |> 
        filter(t == max(t), .by = geo) |> 
        filter(geo %in% top_origins) |> 
        mutate(v = n / sum(n), .by = geo)
      
      data <- country_sector_emp |> 
        filter(sector == top_sectors[m]) |> 
        arrange(desc(v)) |> 
        slice_head(n = 1)
      
      name <- countryname(data$geo[1], to = "name_text")
      
      return(list(name = name, v = 100 * data$v))
    }
    
    write_caption <- function(m) {
      
      sector_label <- tolower(sectors$label[sectors$code == top_sectors[m]])
      sh <- 100 * filter(data, sector == top_sectors[m])$v
      orig <- get_orig_emp(m)
      
      text <- str_glue(paste(
        "- #b[{ pl_pct(sh[1]) }] of all employed persons in {name},",
        "and { pl_pct(sh[2]) } of foreign-borns in {name},",
        "are employed in {sector_label}.\n",
        
        "- In the origin countries from which immigrants moved to",
        "{name} (see Fig. 1), an average",
        "#b[{ pl_pct(sh[3]) }] is employed in {sector_label},",
        "compared to { pl_pct(sh[4]) } in countries worldwide.\n",
        
        "- Of the top 10 origin countries,",
        "#b[{ orig$name }] had the most employees working in {sector_label}."
      ))
      
      return(text)
    }
    
    if (row == 1) {
      caption <- str_glue(paste(
        "#caption(ncol: 3)[
        { write_caption(1) }\n
        #colbreak()\n
        { write_caption(2) }\n
        #colbreak()\n
        { write_caption(3) }
        ]"
      ))
    } else {
      caption <- str_glue(paste(
        "#caption(ncol: 3)[
        { write_caption(4) }\n
        #colbreak()\n
        { write_caption(5) }\n
        #colbreak()\n
        { write_caption(6) }
        ]"
      ))
    }
  }
  
  return(caption)
}





