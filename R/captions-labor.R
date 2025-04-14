
library(tidyverse)
library(spatstat)
library(gdiviz)
library(gdidata)

# get_pct <- function(vector, value) {
#   pct <- 1 - ecdf(vector)(value)
#   
#   t10 <- '#box(baseline: 10%, width: 5pt, image("images/top10.png"))'
#   t33 <- '#box(baseline: 10%, width: 5pt, image("images/top33.png"))'
#   m33 <- '#box(baseline: 10%, width: 5pt, image("images/mid33.png"))'
#   b33 <- '#box(baseline: 10%, width: 5pt, image("images/bot33.png"))'
#   b10 <- '#box(baseline: 10%, width: 5pt, image("images/bot10.png"))'
#   
#   if (pct <= .1) return(t10)
#   if (pct > .1 & pct <= 1/3) return(t33)
#   if (pct > 1/3 & pct < 2/3) return(m33)
#   if (pct >= 2/3 & pct < .9) return(b33)
#   if (pct >= .9) return(b10)
# }
# 
# namer <- function(iso) {
#   name_iso <- filter(countrynames, iso3 == iso)
#   if (name_iso$with_the == 1) name <- paste0("the ", name_iso$name_text)
#   else name <- name_iso$name_text
#   return(name)
# }
# 
# pl <- function(number, d = 0) prettylabel(number, d = d, spell = TRUE)
# pl1 <- function(number, d = 1) prettylabel(number, d = d, spell = TRUE)
# pl2 <- function(number, d = 2) prettylabel(number, d = d, spell = TRUE)
# pl_usd <- function(number, d = 0) {
#   prettylabel(number, d = d, spell = TRUE, currency = "\\$")
# }
# pl_pct <- function(number) prettylabel(number, pct = TRUE)
# pl_pct1 <- function(number) prettylabel(number, d = 1, pct = TRUE)

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


# Fig 1 -------------------------------------------------------------------

caption_emp <- function(iso) {
  
  name <- namer(iso)
  
  data <- snap_data("emp")$data |> drop_na()
  data_iso <- filter(data, geo == iso)
  
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


# Fig 2 -------------------------------------------------------------------

caption_empeduc <- function(iso) {
  
  name <- namer(iso)
  
  data <- snap_data("empeduc")$data
  
  # data <- read_csv("data-raw/Employment_SexEducation.csv") |> 
  #   rename(
  #     geo = Country, 
  #     sex = Sex,
  #     educ = Education, 
  #     birth = PlaceOfBirth, 
  #     t = Year,
  #     n = Value
  #   ) |> 
  #   filter(t == max(t), .by = geo) |> 
  #   mutate(educ = case_when(
  #     educ == "Level not stated" ~ "Unknown",
  #     .default = educ
  #   ))
  
  data_iso <- filter(data, geo == iso)
  
  if (nrow(data_iso) == 0) {
    
    caption <- ""
    
  } else {
    
    # year <- data_iso$t[1]
    t1 <- snap_data("empeduc", iso)$range[2]
    # educ_order <- c(
    #   "Less than basic", 
    #   "Basic", 
    #   "Intermediate", 
    #   "Advanced", 
    #   "Unknown"
    # )
    educ_order <- snap_data("empeduc")$data$educ |> levels()
    educ_cats <- unique(filter(data_iso, birth == "Foreign-born") |> pull(educ))
    educ_cats <- educ_cats[order(match(educ_cats, educ_order))]
    
    data_educ <- data_iso |> 
      summarise(n = sum(n), .by = c(birth, educ)) |> 
      mutate(educ_sh = 100 * n / sum(n), .by = birth)
    
    if (length(educ_cats) == 1) {
      
      educ_text <- "There is no information on the education levels of employed foreign-borns."
      
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
      
      # Basic, Intermediate, Advanced
      
      if (length(educ_cats) == 3) {
        
        educ_text <- str_glue(paste(
          "#b[{ pl_pct(sh1_for$educ_sh) }] of employed foreign-borns",
          "had { tolower(sh1_for$educ) } education,",
          "#b[{ pl_pct(sh2_for$educ_sh) }] had { tolower(sh2_for$educ) }",
          "education and #b[{ pl_pct(sh3_for$educ_sh) }] had",
          "{ tolower(sh3_for$educ) } education in {t1} (compared to",
          "{ pl_pct(sh1_dom$educ_sh) }, { pl_pct(sh2_dom$educ_sh) } and",
          "{ pl_pct(sh3_dom$educ_sh) }, respectively, for employed",
          "native-borns)."
        ))
      }
      
      # Basic, Intermediate, Advanced, Unknown
      # Less than basic, Basic, Advanced, Unknown
      
      if (length(educ_cats) == 4 & educ_cats[4] == "Unknown") {
        
        educ_text <- str_glue(paste(
          "#b[{ pl_pct(sh1_for$educ_sh) }] of employed foreign-borns",
          "had { tolower(sh1_for$educ) } education,",
          "#b[{ pl_pct(sh2_for$educ_sh) }] had { tolower(sh2_for$educ) }",
          "education and #b[{ pl_pct(sh3_for$educ_sh) }] had",
          "{ tolower(sh3_for$educ) } education in {t1} (compared to",
          "{ pl_pct(sh1_dom$educ_sh) }, { pl_pct(sh2_dom$educ_sh) } and",
          "{ pl_pct(sh3_dom$educ_sh) }, respectively, for employed",
          "native-borns). The remaining shares had unknown educational levels."
        ))
      }
      
      # Less than basic, Basic, Intermediate, Advanced
      
      if (length(educ_cats) == 4 & educ_cats[4] != "Unknown") {
        
        sh4_dom <- filter(data_educ_dom, educ == educ_cats[4])
        sh4_for <- filter(data_educ_for, educ == educ_cats[4])
        
        educ_text <- str_glue(paste(
          "In {t1}, #b[{ pl_pct(sh1_for$educ_sh) }] of employed foreign-borns",
          "had { tolower(sh1_for$educ) } education,",
          "#b[{ pl_pct(sh2_for$educ_sh) }] had { tolower(sh2_for$educ) } education,",
          "#b[{ pl_pct(sh3_for$educ_sh) }] had { tolower(sh3_for$educ) } education and",
          "#b[{ pl_pct(sh4_for$educ_sh) }] had { tolower(sh4_for$educ) } education",
          "(compared to { pl_pct(sh1_dom$educ_sh) }, { pl_pct(sh2_dom$educ_sh) },",
          "{ pl_pct(sh3_dom$educ_sh) } and { pl_pct(sh4_dom$educ_sh) }, respectively,",
          "for employed native-borns)."
        ))
      }
      
      # Less than basic, Basic, Intermediate, Advanced, Unknown
      
      if (length(educ_cats) == 5) {
        
        sh4_dom <- filter(data_educ_dom, educ == educ_cats[4])
        sh4_for <- filter(data_educ_for, educ == educ_cats[4])
        
        educ_text <- str_glue(paste(
          "In {t1}, #b[{ pl_pct(sh1_for$educ_sh) }] of employed foreign-borns",
          "had { tolower(sh1_for$educ) } education,",
          "#b[{ pl_pct(sh2_for$educ_sh) }] had { tolower(sh2_for$educ) } education,",
          "#b[{ pl_pct(sh3_for$educ_sh) }] had { tolower(sh3_for$educ) } education and",
          "#b[{ pl_pct(sh4_for$educ_sh) }] had { tolower(sh4_for$educ) } education",
          "(compared to { pl_pct(sh1_dom$educ_sh) }, { pl_pct(sh2_dom$educ_sh) },",
          "{ pl_pct(sh3_dom$educ_sh) } and { pl_pct(sh4_dom$educ_sh) }, respectively,",
          "for employed native-borns).",
          "The remaining shares had unknown educational levels."
        ))
      }
    }
    
    # data_educ <- data |> 
    #   summarise(n = sum(n), .by = c(geo, educ, birth, t)) |> 
    #   mutate(sh = 100 * n / sum(n), .by = c(geo, birth))
    # data_educ_dom <- filter(data_educ, str_detect(birth, "Native"))
    # data_educ_for <- filter(data_educ, str_detect(birth, "Foreign"))
    # 
    # sh_basic_dom <- filter(data_educ_dom, educ == "Basic or less") |> pull(sh)
    # sh_inter_dom <- filter(data_educ_dom, educ == "Intermediate") |> pull(sh)
    # sh_adv_dom <- filter(data_educ_dom, educ == "Advanced") |> pull(sh)
    # sh_basic_for <- filter(data_educ_for, educ == "Basic or less") |> pull(sh)
    # sh_inter_for <- filter(data_educ_for, educ == "Intermediate") |> pull(sh)
    # sh_adv_for <- filter(data_educ_for, educ == "Advanced") |> pull(sh)
    # 
    # data_educ_iso <- filter(data_educ, geo == iso)
    # data_educ_dom_iso <- filter(data_educ_iso, str_detect(birth, "Native"))
    # data_educ_for_iso <- filter(data_educ_iso, str_detect(birth, "Foreign"))
    # 
    # sh_basic_dom_iso <- filter(data_educ_dom_iso, educ == "Basic or less") |> 
    #   pull(sh)
    # sh_inter_dom_iso <- filter(data_educ_dom_iso, educ == "Intermediate") |> 
    #   pull(sh)
    # sh_adv_dom_iso <- filter(data_educ_dom_iso, educ == "Advanced") |> 
    #   pull(sh)
    # sh_basic_for_iso <- filter(data_educ_for_iso, educ == "Basic or less") |> 
    #   pull(sh)
    # sh_inter_for_iso <- filter(data_educ_for_iso, educ == "Intermediate") |> 
    #   pull(sh)
    # sh_adv_for_iso <- filter(data_educ_for_iso, educ == "Advanced") |> 
    #   pull(sh)
    # 
    # caption1 <- str_glue(paste(
    #   "In {t1}, #b[{ pl_pct(sh_basic_for_iso) }]",
    #   "{ get_pct(sh_basic_for, sh_basic_for_iso) } of the foreign-born labor",
    #   "force in {name} had basic education or less,",
    #   "#b[{ pl_pct(sh_inter_for_iso) }]",
    #   "{ get_pct(sh_inter_for, sh_inter_for_iso) } had intermediate education",
    #   "and #b[{ pl_pct(sh_adv_for_iso) }]",
    #   "{ get_pct(sh_adv_for, sh_adv_for_iso) } had advanced education",
    #   "(compared to { pl_pct(sh_basic_dom_iso) }",
    #   "{ get_pct(sh_basic_dom, sh_basic_dom_iso) },",
    #   "{ pl_pct(sh_inter_dom_iso) }",
    #   "{ get_pct(sh_inter_dom, sh_inter_dom_iso) },",
    #   "and { pl_pct(sh_adv_for_iso) }",
    #   "{ get_pct(sh_adv_dom, sh_adv_dom_iso) }, respectively, for",
    #   "native-borns)."
    # ))
    
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


# Fig 3 -------------------------------------------------------------------

caption_unem <- function(iso) {
  
  name <- namer(iso)
  
  data <- read_csv("data/UnemploymentRate_SexEducation.csv") |> 
    rename(
      geo = Country, 
      var = PlaceOfBirth, 
      sex = Sex,
      educ = Education,
      t = Year, 
      v = Value
    ) |> 
    filter(
      var != "Status unknown", 
      sex == "Total", 
      educ %in% c("Total", "Unknown")
    ) |> 
    filter("Native-born" %in% var & "Foreign-born" %in% var, .by = c(geo, t))
  
  data_iso <- filter(data, geo == iso)
  
  if (nrow(data_iso) == 0) {
    
    caption <- str_glue("The ILO has no data on unemployment rates for {name}.")
    
  } else {
    
    t1 <- max(data_iso$t)
    span <- max(data_iso$t) - min(data_iso$t)
    data_iso_t1 <- filter(data_iso, t == max(t))
    data_iso_dom_t1 <- filter(data_iso_t1, str_detect(var, "Native"))$v
    data_iso_for_t1 <- filter(data_iso_t1, str_detect(var, "Foreign"))$v
    
    current_text <- str_glue(paste(
      "In {name} in {t1}, the unemployment rate among foreign-borns was",
      "#b[{ pl_pct1(data_iso_for_t1) }] (compared to",
      "{ pl_pct1(data_iso_dom_t1) } among native-borns)."
    ))
    
    if (span == 0) {
      
      caption <- str_glue("- {current_text}")
      
    } else {
      
      # Choose comparison year
      
      lag <- 5
      
      if (span <= lag) {
        
        t0 <- min(data_iso$t)
        
      } else {
        
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
      data_iso_dom_t0 <- filter(data_iso_t0, str_detect(var, "Native"))$v
      data_iso_for_t0 <- filter(data_iso_t0, str_detect(var, "Foreign"))$v
      
      dom_chg <- data_iso_dom_t1 - data_iso_dom_t0
      for_chg <- data_iso_for_t1 - data_iso_for_t0
      
      change_text <- str_glue(paste(
        "This was a #b[{ pl1(for_chg) }-point] change between {t0} and {t1}",
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


# Fig 4 -------------------------------------------------------------------

caption_unem_disagg <- function(iso) {
  
  name <- namer(iso)
  
  data_all <- read_csv("data/UnemploymentRate_SexEducation.csv") |> 
    rename(
      geo = Country, 
      birth = PlaceOfBirth, 
      sex = Sex,
      educ = Education,
      t = Year, 
      v = Value
    ) |> 
    filter(t == max(t), .by = geo) |> 
    mutate(educ = case_when(
      educ == "Level not stated" ~ "Unknown",
      .default = educ
    ))
  
  data_all_iso <- filter(data_all, geo == iso)
  
  if (nrow(data_all_iso) == 0) {
    
    caption <- ""
    
  } else {
    
    data_iso <- data_all_iso |> 
      filter(sex == "Total" & educ != "Total")
    year <- data_iso$t[1]
    
    educ_order <- c(
      "Less than basic", 
      "Basic", 
      "Intermediate", 
      "Advanced", 
      "Unknown"
    )
    educ_cats <- unique(filter(data_iso, birth == "Foreign-born") |> pull(educ))
    educ_cats <- educ_cats[order(match(educ_cats, educ_order))]
    educ_cats1 <- educ_cats[educ_cats != "Unknown"]
    # data_educ <- data_iso |> 
    #   summarise(n = sum(n), .by = c(birth, educ)) |> 
    #   mutate(educ_sh = 100 * n / sum(n), .by = birth)
    
    if (length(educ_cats) <= 1) {
      
      educ_text <- "There is no information on the education levels of unemployed foreign-borns."
      
    }
    
    if (length(educ_cats1) == 2) {
      
      data_educ_dom <- filter(data_iso, str_detect(birth, "Native"))
      data_educ_for <- filter(data_iso, str_detect(birth, "Foreign"))
      
      sh1_dom <- filter(data_educ_dom, educ == educ_cats[1])
      sh2_dom <- filter(data_educ_dom, educ == educ_cats[2])
      sh1_for <- filter(data_educ_for, educ == educ_cats[1])
      sh2_for <- filter(data_educ_for, educ == educ_cats[2])

      educ_text <- str_glue(paste(
        "In {year}, unemployment rates among foreign-borns in {name} were",
        "#b[{ pl_pct1(sh1_for$v) }] for those with { tolower(sh1_for$educ) }",
        "education and #b[{ pl_pct1(sh2_for$v) }] for those with",
        "{ tolower(sh2_for$educ) } education (compared to",
        "{ pl_pct1(sh1_dom$v) } and { pl_pct1(sh2_dom$v) }, respectively, for",
        "native-borns)."
      ))
    }
    
    if (length(educ_cats1) == 3) {
      
      data_educ_dom <- filter(data_iso, str_detect(birth, "Native"))
      data_educ_for <- filter(data_iso, str_detect(birth, "Foreign"))
      
      sh1_dom <- filter(data_educ_dom, educ == educ_cats[1])
      sh2_dom <- filter(data_educ_dom, educ == educ_cats[2])
      sh3_dom <- filter(data_educ_dom, educ == educ_cats[3])
      sh1_for <- filter(data_educ_for, educ == educ_cats[1])
      sh2_for <- filter(data_educ_for, educ == educ_cats[2])
      sh3_for <- filter(data_educ_for, educ == educ_cats[3])

      educ_text <- str_glue(paste(
        "In {year}, unemployment rates among foreign-borns in {name} were",
        "#b[{ pl_pct1(sh1_for$v) }] for those with { tolower(sh1_for$educ) }",
        "education, #b[{ pl_pct1(sh2_for$v) }] for those with",
        "{ tolower(sh2_for$educ) } education and #b[{ pl_pct1(sh3_for$v) }]",
        "for those with { tolower(sh3_for$educ) } education",
        "(compared to { pl_pct1(sh1_dom$v) }, { pl_pct1(sh2_dom$v) }",
        "and { pl_pct1(sh3_dom$v) }, respectively, for native-borns)."
      ))
    }
    
    if (length(educ_cats1) == 4) {
      
      data_educ_dom <- filter(data_iso, str_detect(birth, "Native"))
      data_educ_for <- filter(data_iso, str_detect(birth, "Foreign"))
      
      sh1_dom <- filter(data_educ_dom, educ == educ_cats[1])
      sh2_dom <- filter(data_educ_dom, educ == educ_cats[2])
      sh3_dom <- filter(data_educ_dom, educ == educ_cats[3])
      sh4_dom <- filter(data_educ_dom, educ == educ_cats[4])
      sh1_for <- filter(data_educ_for, educ == educ_cats[1])
      sh2_for <- filter(data_educ_for, educ == educ_cats[2])
      sh3_for <- filter(data_educ_for, educ == educ_cats[3])
      sh4_for <- filter(data_educ_for, educ == educ_cats[4])

      educ_text <- str_glue(paste(
        "In {year}, unemployment rates among foreign-borns in {name} were",
        "#b[{ pl_pct1(sh1_for$v) }] for those with { tolower(sh1_for$educ) }",
        "education, #b[{ pl_pct1(sh2_for$v) }] for those with",
        "{ tolower(sh2_for$educ) } education, #b[{ pl_pct1(sh3_for$v) }]",
        "for those with { tolower(sh3_for$educ) } education and",
        "#b[{ pl_pct1(sh4_for$v) }] for those with { tolower(sh4_for$educ) }",
        "education (compared to { pl_pct1(sh1_dom$v) },",
        "{ pl_pct1(sh2_dom$v) }, { pl_pct1(sh3_dom$v) }, and",
        "{ pl_pct1(sh4_dom$v) }, respectively, for native-borns)."
      ))
    }
    
    # data_all_t1 <- filter(data_all, t == t1)
    # data_all_iso_t1 <- filter(data_all_iso, t == t1)
    # 
    # data_dom_basic_t1 <- data_all_t1 |> 
    #   summarise(v = mean(v), .by = c(geo, educ, var, t)) |> 
    #   filter(var == "Native-born" & educ == "Basic or less") |> pull(v)
    # data_imm_basic_t1 <- data_all_t1 |> 
    #   summarise(v = mean(v), .by = c(geo, educ, var, t)) |> 
    #   filter(var == "Foreign-born" & educ == "Basic or less") |> pull(v)
    # 
    # data_dom_inter_t1 <- data_all_t1 |> 
    #   summarise(v = mean(v), .by = c(geo, educ, var, t)) |> 
    #   filter(var == "Native-born" & educ == "Intermediate") |> pull(v)
    # data_imm_inter_t1 <- data_all_t1 |> 
    #   summarise(v = mean(v), .by = c(geo, educ, var, t)) |> 
    #   filter(var == "Foreign-born" & educ == "Intermediate") |> pull(v)
    # 
    # data_dom_adv_t1 <- data_all_t1 |> 
    #   summarise(v = mean(v), .by = c(geo, educ, var, t)) |> 
    #   filter(var == "Native-born" & educ == "Advanced") |> pull(v)
    # data_imm_adv_t1 <- data_all_t1 |> 
    #   summarise(v = mean(v), .by = c(geo, educ, var, t)) |> 
    #   filter(var == "Foreign-born" & educ == "Advanced") |> pull(v)
    # 
    # data_iso_dom_basic_t1 <- data_all_iso_t1 |> 
    #   summarise(v = mean(v), .by = c(geo, educ, var, t)) |> 
    #   filter(var == "Native-born" & educ == "Basic or less") |> pull(v)
    # data_iso_imm_basic_t1 <- data_all_iso_t1 |> 
    #   summarise(v = mean(v), .by = c(geo, educ, var, t)) |> 
    #   filter(var == "Foreign-born" & educ == "Basic or less") |> pull(v)
    # 
    # data_iso_dom_inter_t1 <- data_all_iso_t1 |> 
    #   summarise(v = mean(v), .by = c(geo, educ, var, t)) |> 
    #   filter(var == "Native-born" & educ == "Intermediate") |> pull(v)
    # data_iso_imm_inter_t1 <- data_all_iso_t1 |> 
    #   summarise(v = mean(v), .by = c(geo, educ, var, t)) |> 
    #   filter(var == "Foreign-born" & educ == "Intermediate") |> pull(v)
    # 
    # data_iso_dom_adv_t1 <- data_all_iso_t1 |> 
    #   summarise(v = mean(v), .by = c(geo, educ, var, t)) |> 
    #   filter(var == "Native-born" & educ == "Advanced") |> pull(v)
    # data_iso_imm_adv_t1 <- data_all_iso_t1 |> 
    #   summarise(v = mean(v), .by = c(geo, educ, var, t)) |> 
    #   filter(var == "Foreign-born" & educ == "Advanced") |> pull(v)
    
    
    # sex_t1 <- data_all_t1 |> 
    #   summarise(v = mean(v), .by = c(geo, sex, var, t))
    # data_dom_f_t1 <- sex_t1 |> 
    #   filter(var == "Native-born" & sex == "Female") |> pull(v)
    # data_dom_m_t1 <- sex_t1 |> 
    #   filter(var == "Native-born" & sex == "Male") |> pull(v)
    # data_imm_f_t1 <- sex_t1 |> 
    #   filter(var == "Foreign-born" & sex == "Female") |> pull(v)
    # data_imm_m_t1 <- sex_t1 |> 
    #   filter(var == "Foreign-born" & sex == "Male") |> pull(v)
    # 
    # sex_iso_t1 <- data_all_iso_t1 |> 
    #   summarise(v = mean(v), .by = c(geo, sex, var, t))
    # data_dom_iso_f_t1 <- sex_iso_t1 |> 
    #   filter(var == "Native-born" & sex == "Female") |> pull(v)
    # data_dom_iso_m_t1 <- sex_iso_t1 |> 
    #   filter(var == "Native-born" & sex == "Male") |> pull(v)
    # data_imm_iso_f_t1 <- sex_iso_t1 |> 
    #   filter(var == "Foreign-born" & sex == "Female") |> pull(v)
    # data_imm_iso_m_t1 <- sex_iso_t1 |> 
    #   filter(var == "Foreign-born" & sex == "Male") |> pull(v)
    # 
    # comparison <- "more"
    # if (data_imm_iso_m_t1 > data_imm_iso_f_t1) comparison <- "less"
    
    data_sex <- filter(data_all_iso, sex != "Total" & educ == "Total")
    sex_dom <- filter(data_sex, str_detect(birth, "Native"))
    sex_for <- filter(data_sex, str_detect(birth, "Foreign"))
    sex_dom_m <- filter(sex_dom, sex == "Male") |> pull(v)
    sex_dom_f <- filter(sex_dom, sex == "Female") |> pull(v)
    sex_for_m <- filter(sex_for, sex == "Male") |> pull(v)
    sex_for_f <- filter(sex_for, sex == "Female") |> pull(v)
    
    sex_text <- str_glue(paste(
      "#b[{ pl_pct1(sex_for_f) }] of foreign-born women and",
      "#b[{ pl_pct1(sex_for_m) }] of foreign-born men were unemployed",
      "(compared to { pl_pct1(sex_dom_f) } and { pl_pct1(sex_dom_m) },",
      "respectively, for native-borns)."
    ))
    
    caption <- str_glue(paste(
      "- {educ_text}\n",
      "- {sex_text}"
    ))
    # caption <- str_glue(paste(
    #   " - Unemployment rates among foreign-borns in {name} were",
    #   "#b[{pl_pct1(data_iso_imm_basic_t1)}]",
    #   "{get_pct(data_imm_basic_t1, data_iso_imm_basic_t1)} for those with",
    #   "basic or less education, #b[{pl_pct1(data_iso_imm_inter_t1)}]",
    #   "{get_pct(data_imm_inter_t1, data_iso_imm_inter_t1)} for those with",
    #   "intermediate education and #b[{pl_pct1(data_iso_imm_adv_t1)}]",
    #   "{get_pct(data_imm_adv_t1, data_iso_imm_adv_t1)} for those with advanced",
    #   "education in {t1} (compared to #b[{pl_pct1(data_iso_dom_basic_t1)}]",
    #   "{get_pct(data_dom_basic_t1, data_iso_dom_basic_t1)},",
    #   "#b[{pl_pct1(data_iso_dom_inter_t1)}]",
    #   "{get_pct(data_dom_inter_t1, data_iso_dom_inter_t1)} and",
    #   "#b[{pl_pct1(data_iso_dom_adv_t1)}]",
    #   "{get_pct(data_dom_adv_t1, data_iso_dom_adv_t1)}, respectively, for the",
    #   "native-borns).\n",
    #   " - Foreign-born women were overall #b[{comparison}] unemployed",
    #   "(#b[{pl_pct1(data_imm_iso_f_t1)}]",
    #   "{get_pct(data_imm_f_t1, data_imm_iso_f_t1)}) than men",
    #   "({pl_pct1(data_imm_iso_m_t1)}) (compared to",
    #   "{pl_pct1(data_dom_iso_f_t1)} and {pl_pct1(data_dom_iso_m_t1)} for",
    #   "native-born women and men)."
    # ))
  }
  
  return(caption)
}




# Migrant stocks ----------------------------------------------------------

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
  
  # - The number of immigrants has #b[increased/decreased] by an average #b[X%] 
  # per year over the past 30 years (compared to an average annual 
  # increase/decrease in the total population size of Germany by X% over the 
  # same period).
  
  return(caption)
}


# Row 2 -------------------------------------------------------------------

caption_immpyr <- function(iso) {
  
  name <- namer(iso)
  
  # t1 <- max(undesa_stocks$t)
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
  
  if (nrow(drop_na(data_iso)) == 0) {
    
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
      "- Among immigrants in {name} in {t1}, #b[{ pl_pct(data_imm_child_iso) }]",
      "{ get_pct(data_imm_child, data_imm_child_iso) } were children (0-14 years),",
      "#b[{ pl_pct(data_imm_work_iso) }] { get_pct(data_imm_work, data_imm_work_iso) }",
      "were working-age (15-64) and",
      "#b[{ pl_pct(data_imm_elder_iso) }] { get_pct(data_imm_elder, data_imm_elder_iso) }",
      "were elderly (65+)",
      "(compared to { pl_pct(data_gen_child_iso) } { get_pct(data_gen_work, data_gen_work_iso) },",
      "{ pl_pct(data_gen_work_iso) } { get_pct(data_gen_work, data_gen_work_iso) } and",
      "{ pl_pct(data_gen_elder_iso) } { get_pct(data_gen_elder, data_gen_elder_iso) }",
      "for the general population).\n",
      "- #b[{ pl_pct(data_imm_f_iso) }] { get_pct(data_imm_f, data_imm_f_iso) }",
      "of immigrants were female (compared to the { pl_pct(data_gen_f_iso) }",
      "{ get_pct(data_gen_f, data_gen_f_iso) } for the general population)."
    ))
  }
  
  return(caption)
}

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
  
  if (nrow(data_iso) < 2) {
    
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
      "- Among immigrants in {name}, there were #b[{ pl(data_imm_iso) }]",
      "{ get_pct(data_imm, data_imm_iso) } dependents (younger than 15, older",
      "than 64) for every 100 working-age persons (compared to",
      "{ pl(data_gen_iso) } { get_pct(data_gen, data_gen_iso) } dependents",
      "for the general population)."
    ))
    # caption <- str_glue(paste(
    #   " - Among immigrants in {name}, there were #b[{ pl(data_imm_iso) }]",
    #   "{ get_pct(data_imm, data_imm_iso) } dependents (younger than 15, older",
    #   "than 64) for every 100 working-age persons in {t1}.\n",
    #   " - This is {comparison} the ratio for the general population of {name}",
    #   "which was {pl(data_gen_iso)} {get_pct(data_gen, data_gen_iso)}",
    #   "dependents."
    # ))
  }
  
  return(caption)
}





# Row 4 -------------------------------------------------------------------

caption_emp_sector <- function(iso) {
  
  name <- namer(iso)
  
  labels <- tribble(
    ~group, ~lab,
    "A",    "Agriculture",
    "B",    "Mining",
    "C",    "Manufacturing",
    "D-E",  "Utilities",
    "F",    "Construction",
    "G",    "Wholesale and retail trade",
    "H",    "Transportation",
    "I",    "Accommodation and food services",
    "J",    "Information and communication",
    "K",    "Finance",
    "L",    "Real estate",
    "M",    "Professional activities",
    "N",    "Administrative activities",
    "O",    "Public administration",
    "P",    "Education",
    "Q",    "Health",
    "R",    "Arts and recreation",
    "S",    "Other services",
    "T-X",  "Others"
  )
  
  data_all <- read_csv("data/Employment_ActivityPlaceofbirth_rev4.csv") |> 
    rename(
      geo = Country, 
      sector = Activity,
      birth = PlaceOfBirth, 
      t = Year, 
      n = Value,
      sd = Uncertainty
    ) |> 
    mutate(
      n = 1000 * n,
      group = str_extract(sector, "[A-Z](?=\\.)"),
      group = case_when(
        group %in% c("D", "E") ~ "D-E",
        group %in% c("T", "U", "X") ~ "T-X",
        .default = group
      )
    ) |> 
    summarise(n = sum(n), sd = sum(sd), .by = c(geo, birth, t, group))
  
  data_all_iso <- filter(data_all, geo == iso) |> 
    filter(t == max(t))
  
  if (nrow(data_all_iso) == 0) {
    
    caption <- str_glue(
      "Eurostat has no information on employment by sector for {name}."
    )
    
  } else {
    
    t1 <- data_all_iso$t[1]
    
    data_iso <- data_all_iso |> 
      left_join(labels, by = "group") |> 
      mutate(
        lab = tolower(lab), 
        v = 100 * n / sum(n),
        .by = c(geo, birth, t)
      )
    
    data_iso_tot <- data_iso |> 
      summarise(n = sum(n), .by = c(geo, group, lab)) |> 
      mutate(v = 100 * n / sum(n)) |> 
      arrange(desc(n))
    
    if (nrow(filter(data_iso, birth == "Foreign-born")) > 0) {
      
      data_iso_v <- slice_head(data_iso_tot, n = 5) |> 
        left_join(
          data_iso |> 
            filter(group %in% data_iso_tot$group[1:5]) |> 
            pivot_wider(id_cols = lab, names_from = birth, values_from = v) |> 
            arrange(match(lab, data_iso_tot$lab)),
          by = "lab"
        ) |> 
        rename(tot = v, dom = `Native-born`, imm = `Foreign-born`)
      
      data_iso_n <- data_iso_tot |> 
        left_join(
          data_iso |> 
            pivot_wider(id_cols = lab, names_from = birth, values_from = n),
          by = "lab"
        ) |> 
        rename(tot = v, dom = `Native-born`, imm = `Foreign-born`) |> 
        mutate(imm_sh = 100 * imm / n) |> 
        arrange(desc(imm_sh)) |> 
        filter(lab != "others")
      
      caption <- str_glue(paste(
        " - In {t1}, the five largest sectors in {name} by employment were",
        "#b[{ data_iso_v$lab[1] }] ({ pl_pct(data_iso_v$tot[1]) } of total",
        "employment, { pl_pct(data_iso_v$imm[1]) } of foreign-born",
        "employment), #b[{ data_iso_v$lab[2] }]",
        "({ pl_pct(data_iso_v$tot[2]) } and { pl_pct(data_iso_v$imm[2]) }),",
        "#b[{ data_iso_v$lab[3] }] ({ pl_pct(data_iso_v$tot[3]) } and",
        "{ pl_pct(data_iso_v$imm[3]) }),",
        "#b[{ data_iso_v$lab[4] }] ({ pl_pct(data_iso_v$tot[4]) } and",
        "{ pl_pct(data_iso_v$imm[4]) }) and",
        "#b[{ data_iso_v$lab[5] }] ({ pl_pct(data_iso_v$tot[5]) } and",
        "{ pl_pct(data_iso_v$imm[5]) }).\n",
        "#colbreak()\n",
        " - The five sectors with the highest proportions of foreign-borns",
        "among all persons employed in the sector were",
        "#b[{ data_iso_n$lab[1] }] ({ pl_pct(data_iso_n$imm_sh[1]) }),",
        "#b[{ data_iso_n$lab[2] }] ({ pl_pct(data_iso_n$imm_sh[2]) }),",
        "#b[{ data_iso_n$lab[3] }] ({ pl_pct(data_iso_n$imm_sh[3]) }),",
        "#b[{ data_iso_n$lab[4] }] ({ pl_pct(data_iso_n$imm_sh[4]) }) and",
        "#b[{ data_iso_n$lab[5] }] ({ pl_pct(data_iso_n$imm_sh[5]) })."
      ))
      
    } else {
      
      data_iso_v <- slice_head(data_iso_tot, n = 5) |> 
        left_join(
          data_iso |> 
            filter(group %in% data_iso_tot$group[1:5]) |> 
            pivot_wider(id_cols = lab, names_from = birth, values_from = v) |> 
            arrange(match(lab, data_iso_tot$lab)),
          by = "lab"
        ) |> 
        rename(tot = v, dom = `Native-born`)
      
      data_iso_n <- data_iso_tot |> 
        left_join(
          data_iso |> 
            pivot_wider(id_cols = lab, names_from = birth, values_from = n),
          by = "lab"
        ) |> 
        rename(tot = v, dom = `Native-born`) |> 
        filter(lab != "others")
      
      caption <- str_glue(paste(
        "In {t1}, the five largest sectors in {name} by employment were",
        "#b[{ data_iso_v$lab[1] }] ({ pl_pct(data_iso_v$tot[1]) } of total",
        "employment), #b[{ data_iso_v$lab[2] }]",
        "({ pl_pct(data_iso_v$tot[2]) }),",
        "#b[{ data_iso_v$lab[3] }] ({ pl_pct(data_iso_v$tot[3]) }),",
        "#b[{ data_iso_v$lab[4] }] ({ pl_pct(data_iso_v$tot[4]) }) and",
        "#b[{ data_iso_v$lab[5] }] ({ pl_pct(data_iso_v$tot[5]) })."
      ))
      
    }
    
  }
  
  return(caption)
}


# Row 5 -------------------------------------------------------------------

caption_emp_occup <- function(iso) {
  
  name <- namer(iso)
  
  sector_labels <- tribble(
    ~sector, ~lab,
    "A",     "Agriculture",
    "B",     "Mining",
    "C",     "Manufacturing",
    "D",     "Utilities",
    "E",     "Water supply",
    "F",     "Construction",
    "G",     "Wholesale and retail trade",
    "H",     "Transportation",
    "I",     "Accommodation and food services",
    "J",     "Information and communication",
    "K",     "Finance",
    "L",     "Real estate",
    "M",     "Professional activities",
    "N",     "Administrative activities",
    "O",     "Public administration",
    "P",     "Education",
    "Q",     "Health",
    "R",     "Arts and recreation",
    "S",     "Other services",
  )
  
  occup_labels <- tribble(
    ~occup, ~occup_lab,
    "0",    "Armed forces occupations", 
    "1",    "Managers", 
    "2",    "Professionals", 
    "3",    "Technicians and associate professionals", 
    "4",    "Clerical support workers", 
    "5",    "Service and sales workers", 
    "6",    "Skilled agricultural, forestry and fishery workers", 
    "7",    "Craft and related trades workers", 
    "8",    "Plant and machine operators, and assemblers", 
    "9",    "Elementary occupations", 
    "X",    "Not elsewhere classified",
    "S1",   "Low-skilled",
    "S2",   "Medium-skilled",
    "S3",   "High-skilled",
    "SX",   "Unknown",
    "XX",   "Unknown"
  )
  
  data_all <- read_csv("data/Employment_ActivityOccupation_rev4.csv") |> 
    rename(
      geo = Country, 
      sector = Activity,
      occup = Occupation,
      t = Year, 
      n = Value,
      sd = Uncertainty
    ) |> 
    mutate(
      sector = str_extract(sector, "[A-Z](?=\\.)"),
      occup = case_when(
        occup == "(Skill level): Skill level 1 ~ low" ~ "S1",
        occup == "(Skill level): Skill level 2 ~ medium" ~ "S2",
        occup == "(Skill level): Skill levels 3 and 4 ~ high" ~ "S3",
        occup == "(Skill level): Not elsewhere classified" ~ "SX",
        occup == "Unknown" ~ "XX",
        .default = str_extract(occup, "[0-9X](?=\\.)")
      ),
      skill = case_when(
        occup %in% c("S1", 1:3) ~ "high",
        occup %in% c("S2", 4:8) ~ "medium",
        occup %in% c("S3", 9) ~ "low",
      )
    )
  
  data_all_iso <- filter(data_all, geo == iso) |> filter(t == max(t))
  
  if (nrow(data_all_iso) == 0) {
    
    caption <- str_glue(
      "Eurostat has no information on occupations by sector for {name}."
    )
    
  } else {
    
    t1 <- data_all_iso$t[1]
    occup_disagg <- unique(data_all_iso$occup) |> length()
    
    data_iso <- data_all_iso |> 
      left_join(sector_labels, by = "sector") |> 
      left_join(occup_labels, by = "occup") |> 
      rename(sector_lab = lab) |> 
      mutate(sector_lab = tolower(sector_lab), occup_lab = tolower(occup_lab))
    
    data_iso_occup <- data_iso |> 
      summarise(n = sum(n), .by = c(geo, occup_lab)) |> 
      arrange(desc(n)) |> 
      mutate(v = 100 * n / sum(n))
    
    if (occup_disagg <= 4) {
      
      data_iso_occup_low <- data_iso_occup |> 
        filter(str_detect(occup_lab, "low")) |> pull(v)
      data_iso_occup_med <- data_iso_occup |> 
        filter(str_detect(occup_lab, "medium")) |> pull(v)
      data_iso_occup_high <- data_iso_occup |> 
        filter(str_detect(occup_lab, "high")) |> pull(v)
      
      occup_text <- str_glue(paste(
        "Across sectors in {name} in {t1}, #b[{ pl_pct(data_iso_occup_low) }]",
        "were low-skilled, #b[{ pl_pct(data_iso_occup_med) }] were",
        "medium-skilled and #b[{ pl_pct(data_iso_occup_high) }] were",
        "high-skilled. The rest had unknown skill levels."
      ))
    }
    
    if (occup_disagg > 4) {
      
      occup_text <- str_glue(paste(
        "Across sectors in {name} in {t1}, the top five occupations were",
        "#b[{ data_iso_occup$occup_lab[1] }] ({ pl_pct(data_iso_occup$v[1]) }),",
        "#b[{ data_iso_occup$occup_lab[2] }] ({ pl_pct(data_iso_occup$v[2]) }),",
        "#b[{ data_iso_occup$occup_lab[3] }] ({ pl_pct(data_iso_occup$v[3]) }),",
        "#b[{ data_iso_occup$occup_lab[4] }] ({ pl_pct(data_iso_occup$v[4]) })",
        "and #b[{ data_iso_occup$occup_lab[5] }]",
        "({ pl_pct(data_iso_occup$v[5]) })."
      ))
    }
    
    data_skill <- data_all_iso |> 
      summarise(n = sum(n), .by = skill) |> 
      mutate(v = 100 * n / sum(n, na.rm = TRUE))
    skill_high <- filter(data_skill, skill == "high")$v
    skill_med <- filter(data_skill, skill == "medium")$v
    skill_low <- filter(data_skill, skill == "low")$v
    
    skill_text <- str_glue(paste(
      "#b[{ pl_pct1(skill_low) }], #b[{ pl_pct1(skill_med) }] and",
      "#b[{ pl_pct1(skill_high) }] of persons worked in low-, medium- and",
      "high-skilled occupations, respectively."
    ))
    
    caption <- str_glue(
      "- {occup_text}\n",
      "#colbreak()\n",
      "- {skill_text}"
    )
  }
  
  return(caption)
}



# Row 6 -------------------------------------------------------------------

caption_vacant <- function(iso) {
  
  name <- namer(iso)
  
  labels <- tribble(
    ~group, ~lab,
    "A",    "Agriculture",
    "B",    "Mining",
    "C",    "Manufacturing",
    "D-E",  "Utilities",
    "F",    "Construction",
    "G",    "Wholesale and retail trade",
    "H",    "Transportation",
    "I",    "Accommodation and food services",
    "J",    "Information and\ncommunication",
    "K",    "Finance",
    "L",    "Real estate",
    "M",    "Professional activities",
    "N",    "Administrative activities",
    "O",    "Public administration",
    "P",    "Education",
    "Q",    "Health",
    "R",    "Arts and recreation",
    "S",    "Other services",
    "B-E",  "Mining, manufacturing and utilities",
    "B-F",  "Mining, manufacturing, construction and utilities",
    "G-I",  "Business services",
    "G-N",  "Business services",
    "M-N",  "Professional and administrative services",
    "O-Q",  "Personal services",
    "O-S",  "Personal services",
    "R-S",  "Other services"
  )
  
  data_all <- readxl::read_excel("data/Vacancies_Activity.xlsx") |> 
    rename(
      geo = Country, 
      sector = Activity,
      t = LatestYear, 
      n = Value
    ) |> 
    mutate(
      sector = str_replace(sector, "_", "-"),
      sector = case_when(
        sector %in% c("D", "E") ~ "D-E",
        .default = sector
      )
    ) |> 
    summarise(n = sum(n), .by = c(geo, sector, t))
  
  data_all_iso <- filter(data_all, geo == iso)
  
  if (nrow(data_all_iso) == 0) {
    
    caption <- str_glue(
      "Eurostat has no information on vacancies by sector for {name}."
    )
    
  } else {
    
    t1 <- data_all_iso$t[1]
    sectors <- length(unique(data_all_iso$sector))
    
    data_all_iso_labs <- data_all_iso |> 
      left_join(labels, by = c("sector" = "group")) |> 
      arrange(desc(n)) |> 
      mutate(v = 100 * n / sum(n), lab = tolower(lab))
    
    if (sectors >= 5) {
      
      data_iso <- data_all_iso_labs |> slice_head(n = 5)
      
      caption <- str_glue(paste(
        "In {t1}, the five largest sectors in need of labour in {name} were",
        "#b[{ data_iso$lab[1] }] ({ pl_pct(data_iso$v[1]) } of all",
        "vacancies), #b[{ data_iso$lab[2] }] ({ pl_pct(data_iso$v[2]) }),",
        "#b[{ data_iso$lab[3] }] ({ pl_pct(data_iso$v[3]) }),",
        "#b[{ data_iso$lab[4] }] ({ pl_pct(data_iso$v[4]) }) and",
        "#b[{ data_iso$lab[5] }] ({ pl_pct(data_iso$v[5]) })."
      ))
    
    } else {
      
      data_iso <- data_all_iso_labs
      
      caption <- str_glue(paste(
        "In {t1}, the five largest sectors in need of labour in {name} were",
        "#b[{ data_iso$lab[1] }] ({ pl_pct(data_iso$v[1]) } of all",
        "vacancies), #b[{ data_iso$lab[2] }] ({ pl_pct(data_iso$v[2]) })",
        "and #b[{ data_iso$lab[3] }] ({ pl_pct(data_iso$v[3]) })."
      ))
      
    }
  }
  
  return(caption)
}


# Row 7 -------------------------------------------------------------------




# Row 8 -------------------------------------------------------------------

caption_emp_sh2 <- function(iso) {
  
  name <- namer(iso)
  
  sector_labels <- tribble(
    ~sector, ~lab,
    "A",     "Agriculture",
    "B",     "Mining",
    "C",     "Manufacturing",
    "D",     "Utilities",
    "E",     "Water supply",
    "F",     "Construction",
    "G",     "Wholesale and retail trade",
    "H",     "Transportation",
    "I",     "Accommodation and food services",
    "J",     "Information and communication",
    "K",     "Finance",
    "L",     "Real estate",
    "M",     "Professional activities",
    "N",     "Administrative activities",
    "O",     "Public administration",
    "P",     "Education",
    "Q",     "Health",
    "R",     "Arts and recreation",
    "S",     "Other services",
  )
  
  occup_labels <- tribble(
    ~occup, ~lab,
    "0",    "Armed forces occupations", 
    "1",    "Managers", 
    "2",    "Professionals", 
    "3",    "Technicians and associate professionals", 
    "4",    "Clerical support workers", 
    "5",    "Service and sales workers", 
    "6",    "Skilled agricultural, forestry and fishery workers", 
    "7",    "Craft and related trades workers", 
    "8",    "Plant and machine operators, and assemblers", 
    "9",    "Elementary occupations", 
    "X",    "Not elsewhere classified"
  )
  
  data_all <- readxl::read_excel(
    "data/Employment_ActivityOccupation_rev4.xlsx"
  ) |> 
    rename(
      geo = Country, 
      sector = Activity,
      occup = Occupation,
      t = Year, 
      n = Value
    ) |> 
    mutate(
      n = 1000 * n,
      sector = str_extract(sector, "[A-Z](?=\\.)"),
      occup = str_extract(occup, "[0-9X](?=\\.)"),
    ) |> 
    filter(t == max(t), .by = geo)
  
  data_sectors <- summarise(data_all, n = sum(n), .by = c(geo, sector, t))
  data_sectors_iso <- filter(data_sectors, geo == iso)
  
  if (nrow(data_sectors_iso) == 0) {
    
    caption <- str_glue("No data for {name}.")
    
  } else {
    
    # Select sectors
    top_sectors <- data_sectors_iso |> 
      arrange(desc(n)) |> 
      slice_head(n = 5) |> 
      pull(sector)
    sector_labs <- filter(sector_labels, sector %in% top_sectors) |> 
      pull(lab) |> 
      tolower()
    
    # Select comparators
    comp_list <- filter(data_sectors, geo != iso & sector %in% top_sectors) |> 
      group_by(sector) |> 
      arrange(desc(n), by_group = TRUE) |> 
      ungroup() |> 
      slice_head(n = 5, by = sector) |> 
      mutate(name = countryname(geo)) |> 
      left_join(sector_labels, by = "sector")

    df <- left_join(
      comp_list |> select(geo, sector, t),
      data_all |> select(geo, sector, occup, t, n),
      by = c("geo", "sector", "t")
    ) |> 
      left_join(occup_labels, by = "occup") |> 
      group_by(geo, sector) |> 
      mutate(
        lab = tolower(lab), 
        name = countryname(geo), 
        v = 100 * n / sum(n)
      ) |> 
      arrange(desc(v), by_group = TRUE) |> 
      ungroup()
    
    overview <- str_glue(paste(
      "The five largest sectors in {name} by employment are",
      "#b[{ sector_labs[1] }], #b[{ sector_labs[2] }], #b[{ sector_labs[3] }],",
      "#b[{ sector_labs[4] }] and #b[{ sector_labs[5] }]."
    ))
    
    sector1 <- filter(comp_list, sector == top_sectors[1])
    sector1_occup <- filter(df, sector == top_sectors[1])
    
    sector1_text <- str_glue(paste(
      "The largest labor forces employed in { sector_labs[1] } are in",
      "#b[{ sector1$name[1] }], #b[{ sector1$name[2] }] and",
      "#b[{ sector1$name[3] }].",
      
      "In { sector1$name[1] }, occupations are mainly",
      "#b[{ filter(sector1_occup, name == sector1$name[1])$lab[1] }]",
      "({ pl_pct(filter(sector1_occup, name == sector1$name[1])$v[1]) }),",
      "#b[{ filter(sector1_occup, name == sector1$name[1])$lab[2] }]",
      "({ pl_pct(filter(sector1_occup, name == sector1$name[1])$v[2]) }) and",
      "#b[{ filter(sector1_occup, name == sector1$name[1])$lab[3] }]",
      "({ pl_pct(filter(sector1_occup, name == sector1$name[1])$v[3]) }).",
      
      "In { sector1$name[2] }, occupations are mainly",
      "#b[{ filter(sector1_occup, name == sector1$name[2])$lab[1] }]",
      "({ pl_pct(filter(sector1_occup, name == sector1$name[1])$v[1]) }),",
      "#b[{ filter(sector1_occup, name == sector1$name[2])$lab[2] }]",
      "({ pl_pct(filter(sector1_occup, name == sector1$name[1])$v[2]) }) and",
      "#b[{ filter(sector1_occup, name == sector1$name[2])$lab[3] }]",
      "({ pl_pct(filter(sector1_occup, name == sector1$name[1])$v[3]) }).",
      
      "In { sector1$name[2] }, occupations are mainly",
      "#b[{ filter(sector1_occup, name == sector1$name[3])$lab[1] }]",
      "({ pl_pct(filter(sector1_occup, name == sector1$name[1])$v[1]) }),",
      "#b[{ filter(sector1_occup, name == sector1$name[3])$lab[2] }]",
      "({ pl_pct(filter(sector1_occup, name == sector1$name[1])$v[2]) }) and",
      "#b[{ filter(sector1_occup, name == sector1$name[3])$lab[3] }]",
      "({ pl_pct(filter(sector1_occup, name == sector1$name[1])$v[3]) })."
    ))
    
    sector2 <- filter(comp_list, sector == top_sectors[2])
    sector2_occup <- filter(df, sector == top_sectors[2])
    
    sector2_text <- str_glue(paste(
      "The largest labor forces employed in { sector_labs[2] } are in",
      "#b[{ sector2$name[1] }], #b[{ sector2$name[2] }] and",
      "#b[{ sector2$name[3] }].",
      
      "In { sector2$name[1] }, occupations are mainly",
      "#b[{ filter(sector2_occup, name == sector2$name[1])$lab[1] }]",
      "({ pl_pct(filter(sector2_occup, name == sector2$name[1])$v[1]) }),",
      "#b[{ filter(sector2_occup, name == sector2$name[1])$lab[2] }]",
      "({ pl_pct(filter(sector2_occup, name == sector2$name[1])$v[2]) }) and",
      "#b[{ filter(sector2_occup, name == sector2$name[1])$lab[3] }]",
      "({ pl_pct(filter(sector2_occup, name == sector2$name[1])$v[3]) }).",
      
      "In { sector2$name[2] }, occupations are mainly",
      "#b[{ filter(sector2_occup, name == sector2$name[2])$lab[1] }]",
      "({ pl_pct(filter(sector2_occup, name == sector2$name[1])$v[1]) }),",
      "#b[{ filter(sector2_occup, name == sector2$name[2])$lab[2] }]",
      "({ pl_pct(filter(sector2_occup, name == sector2$name[1])$v[2]) }) and",
      "#b[{ filter(sector2_occup, name == sector2$name[2])$lab[3] }]",
      "({ pl_pct(filter(sector2_occup, name == sector2$name[1])$v[3]) }).",
      
      "In { sector2$name[2] }, occupations are mainly",
      "#b[{ filter(sector2_occup, name == sector2$name[3])$lab[1] }]",
      "({ pl_pct(filter(sector2_occup, name == sector2$name[1])$v[1]) }),",
      "#b[{ filter(sector2_occup, name == sector2$name[3])$lab[2] }]",
      "({ pl_pct(filter(sector2_occup, name == sector2$name[1])$v[2]) }) and",
      "#b[{ filter(sector2_occup, name == sector2$name[3])$lab[3] }]",
      "({ pl_pct(filter(sector2_occup, name == sector2$name[1])$v[3]) })."
    ))
    
    sector3 <- filter(comp_list, sector == top_sectors[3])
    sector3_occup <- filter(df, sector == top_sectors[3])
    
    sector3_text <- str_glue(paste(
      "The largest labor forces employed in { sector_labs[3] } are in",
      "#b[{ sector3$name[1] }], #b[{ sector3$name[2] }] and",
      "#b[{ sector3$name[3] }].",
      
      "In { sector3$name[1] }, occupations are mainly",
      "#b[{ filter(sector3_occup, name == sector3$name[1])$lab[1] }]",
      "({ pl_pct(filter(sector3_occup, name == sector3$name[1])$v[1]) }),",
      "#b[{ filter(sector3_occup, name == sector3$name[1])$lab[2] }]",
      "({ pl_pct(filter(sector3_occup, name == sector3$name[1])$v[2]) }) and",
      "#b[{ filter(sector3_occup, name == sector3$name[1])$lab[3] }]",
      "({ pl_pct(filter(sector3_occup, name == sector3$name[1])$v[3]) }).",
      
      "In { sector3$name[2] }, occupations are mainly",
      "#b[{ filter(sector3_occup, name == sector3$name[2])$lab[1] }]",
      "({ pl_pct(filter(sector3_occup, name == sector3$name[1])$v[1]) }),",
      "#b[{ filter(sector3_occup, name == sector3$name[2])$lab[2] }]",
      "({ pl_pct(filter(sector3_occup, name == sector3$name[1])$v[2]) }) and",
      "#b[{ filter(sector3_occup, name == sector3$name[2])$lab[3] }]",
      "({ pl_pct(filter(sector3_occup, name == sector3$name[1])$v[3]) }).",
      
      "In { sector3$name[2] }, occupations are mainly",
      "#b[{ filter(sector3_occup, name == sector3$name[3])$lab[1] }]",
      "({ pl_pct(filter(sector3_occup, name == sector3$name[1])$v[1]) }),",
      "#b[{ filter(sector3_occup, name == sector3$name[3])$lab[2] }]",
      "({ pl_pct(filter(sector3_occup, name == sector3$name[1])$v[2]) }) and",
      "#b[{ filter(sector3_occup, name == sector3$name[3])$lab[3] }]",
      "({ pl_pct(filter(sector3_occup, name == sector3$name[1])$v[3]) })."
    ))
    
    caption <- str_glue(paste(
      " - {overview}\n",
      " - {sector1_text}\n",
      "#colbreak()\n",
      " - {sector2_text}\n",
      " - {sector3_text}"
    ))
  }
  
  return(caption)
}















# Employment shares -------------------------------------------------------

caption_emp_sh <- function(iso) {
  
  name <- namer(iso)
  
  sector_labels <- tribble(
    ~sector, ~lab,
    "A",     "Agriculture",
    "B",     "Mining",
    "C",     "Manufacturing",
    "D",     "Utilities",
    "E",     "Water supply",
    "F",     "Construction",
    "G",     "Wholesale and retail trade",
    "H",     "Transportation",
    "I",     "Accommodation and food services",
    "J",     "Information and communication",
    "K",     "Finance",
    "L",     "Real estate",
    "M",     "Professional activities",
    "N",     "Administrative activities",
    "O",     "Public administration",
    "P",     "Education",
    "Q",     "Health",
    "R",     "Arts and recreation",
    "S",     "Other services",
  )
  
  occup_labels <- tribble(
    ~occup, ~lab,
    "0",    "Armed forces occupations", 
    "1",    "Managers", 
    "2",    "Professionals", 
    "3",    "Technicians and associate professionals", 
    "4",    "Clerical support workers", 
    "5",    "Service and sales workers", 
    "6",    "Skilled agricultural, forestry and fishery workers", 
    "7",    "Craft and related trades workers", 
    "8",    "Plant and machine operators, and assemblers", 
    "9",    "Elementary occupations", 
    "X",    "Not elsewhere classified"
  )
  
  data <- readxl::read_excel("data/Employment_ActivityOccupation_rev4.xlsx") |> 
    rename(
      geo = Country, 
      sector = Activity,
      occup = Occupation,
      t = Year, 
      n = Value,
      sd = Uncertainty
    ) |> 
    mutate(
      sector = str_extract(sector, "[A-Z](?=\\.)"),
      occup = str_extract(occup, "[0-9X](?=\\.)"),
      v = n / sum(n),
      .by = c(geo, t)
    ) |> 
    filter(t == max(t))
  
  df_hero <- filter(data, geo == iso)
  
  # Select sectors
  
  top_sectors <- df_hero |> 
    summarise(n = sum(n), .by = c(geo, sector)) |> 
    arrange(desc(n)) |> 
    mutate(rank = 1:n())
  sectors_list <- top_sectors$sector[1:6]
  
  sector_labels_select <- filter(sector_labels, sector %in% sectors_list)
  sectors_text <- str_glue(paste(
    '{paste(sector_labels_select$lab[1:5], collapse = ", ")},',
    "and {sector_labels_select$lab[6]}"
  )) |> tolower()
  
  # Select comparators
  
  # t1 <- max(undesa_stocks$t)
  t1 <- 2020
  immig <- undesa_stocks |> 
    left_join(countrynames, by = c("from" = "iso3")) |> 
    filter(t == t1 & geo == hero) |> 
    summarise(n = sum(n), .by = c(geo, from, region, t)) |> 
    mutate(region = case_when(is.na(region) ~ "Unknown", .default = region)) |> 
    rename(hero = geo, nat = from) |> 
    arrange(desc(n))
  
  immig_list_provisional <- filter(immig, nat != "OOO") |> pull(nat)
  immig_list <- c()
  count <- 0
  
  for (i in immig_list_provisional) {
    df_i <- filter(data, geo == i)
    if (nrow(df_i) > 0) {
      immig_list <- c(immig_list, i)
      count <- count + 1
    }
    if (count == 5) break
  }
  
  df_world <- data |> 
    summarise(n = sum(n), .by = c(sector, occup)) |> 
    filter(sector %in% sectors_list) |> 
    mutate(geo = "World", v = n / sum(n))
  df <- filter(data, geo %in% immig_list & sector %in% sectors_list) |> 
    bind_rows(df_world) |> 
    left_join(sector_labels, by = "sector") |> 
    mutate(
      sector_lab = factor(lab, levels = rev(sector_labels_select$lab))
    ) |> 
    complete(
      geo,
      sector_lab = sector_labels_select$lab,
      occup = c(0:9, "X"),
      fill = list(v = 0)
    )
  
  country_labels <- tibble(iso = immig_list) |> 
    mutate(name = countryname(iso)) |> 
    bind_rows(tibble(iso = "World", name = "World"))
  
  
  caption <- str_glue(paste(
    "- The largest sectors in {name} by share of employment are",
    "{sectors_text}"
  ))
  
  return(caption)
}


# Net migration -----------------------------------------------------------

caption_nmig <- function(iso) {
  
  name <- namer(iso)
  
  nmig <- filter(wdi, var == "nmig") |> rename(nmig = v) |> select(-var)
  pop <- filter(wdi, var == "pop") |> rename(pop = v) |> select(-var)
  data <- full_join(nmig, pop, by = c("geo", "t")) |> 
    mutate(v = 1000 * nmig / pop)
  data_iso <- filter(data, geo == iso)
  
  map <- paste(
    "The map shows where movements have taken place. Blue represents areas of",
    "net in-migration while red represents areas of net out-immigration."
  )
  
  if (nrow(drop_na(data_iso, v)) == 0) {
    
    caption <- str_glue(paste(
      "The World Bank has no information on net migration for {name}.",
      "#colbreak()",
      "{map}"
    ))
    
  } else {
    
    t1 <- max(data_iso$t) |> as.numeric()
    t0 <- t1 - 9
    
    tot <- filter(data, t >= t0) |> 
      summarise(nmig = sum(nmig), .by = geo) |> 
      pull(nmig)
    tot_iso <- filter(data_iso, geo == iso & t >= t0) |> 
      summarise(nmig = sum(nmig), .by = geo) |> 
      pull(nmig)
    
    avg <- filter(data, t >= t0) |> 
      summarise(v = mean(v), .by = geo) |> 
      pull(v)
    avg_iso <- filter(data, geo == iso & t >= t0) |> 
      summarise(v = mean(v), .by = geo) |> 
      pull(v)
    
    # avg_iso_avg <- mean(data_iso$v)
    
    # data_iso_t1 <- filter(data_iso, t == t1)$v
    # data_t1 <- filter(data, t == t1)$v
    
    verb <- "entered"
    if (tot_iso < 0) verb <- "left"
    
    caption <- str_glue(paste(
      "According to the World Bank, a cumulative net total of",
      "#b[{pl(abs(tot_iso))}] {get_pct(tot, tot_iso)} migrants have #b[{verb}]",
      "{name} over {t0}–{t1}, or an average #b[{pl(avg_iso, d = 1)}]",
      "{get_pct(avg, avg_iso)} migrants for every 1,000 population per year.",
      "#colbreak()",
      "{map}"
    ))
  }
  
  return(caption)
}





# Refugees ----------------------------------------------------------------

caption_refug_orig <- function(iso) {
  
  name <- namer(iso)
  
  data <- left_join(unhcr, countrynames, by = c("geo" = "iso3")) |> 
    filter(group == "refugee") |> 
    summarise(n = sum(n), .by = c(geo, t, from, region))
  data_iso <- filter(data, from == iso) |> 
    summarise(n = sum(n), .by = c(from, t))
  
  if (nrow(data_iso) == 0) {
    
    caption <- str_glue(
      "UNHCR has no information on refugees originating from {name}.\n\n"
    )
    
  } else {
    
    data_iso <- data_iso |> 
      complete(t = seq(min(data_iso$t), max(data_iso$t)), fill = list(n = 0))
    data_reg <- filter(data, from == iso) |> 
      summarise(n = sum(n), .by = c(region, t))
    
    # t0 <- min(data_iso$t) |> as.numeric()
    t1 <- max(data_iso$t) |> as.numeric()
    t0 <- t1 - 1
    
    data_t0t1 <- filter(unhcr, group == "refugee" & t %in% c(t0, t1)) |> 
      summarise(n = sum(n), .by = c(from, t)) |> 
      mutate(t = case_when(t == t0 ~ "col_t0", t == t1 ~ "col_t1")) |> 
      pivot_wider(names_from = t, values_from = n) |> 
      drop_na() |> 
      mutate(change = 100 * ((col_t1 / col_t0)^(1 / (t1 - t0)) - 1))
    
    data_t0 <- filter(unhcr, group == "refugee" & t == t0) |> 
      summarise(n = sum(n), .by = from)
    data_t1 <- filter(unhcr, group == "refugee" & t == t1) |> 
      summarise(n = sum(n), .by = from)
    
    data_iso_t0 <- filter(data_iso, t == t0) |> pull(n)
    data_iso_t1 <- filter(data_iso, t == t1) |> pull(n)
    
    # Shares of population
    pop_t1 <- filter(wdi, var == "pop" & t == t1) |> rename(pop = v)
    share_t1 <- left_join(data_t1, pop_t1, by = c("from" = "geo")) |> 
      mutate(v = 100 * n / pop) |> 
      drop_na()
    share_iso_t1 <- filter(share_t1, from == iso) |> pull(v)
    
    # 1 year change 
    change <- data_t0t1$change
    change_iso <- 100 * ((data_iso_t1 / data_iso_t0)^(1 / (t1 - t0)) - 1)
    change_iso_lab <- pl_pct1(change_iso)
    if (change_iso == Inf) change_iso_lab <- ""
    if (change_iso > 0) change_iso_lab <- paste0("+", pl_pct1(change_iso))
    
    change_text <- str_glue(paste(
      "This was a #b[{change_iso_lab}] {get_pct(change, change_iso)}",
      "change from the previous year."
    ))
    
    if (data_iso_t1 > 0 & data_iso_t0 == 0) {
      change_text <- "There were no recorded refugees the previous year."
    }
    
    reg_all <- filter(data_reg, t == t1) |> 
      mutate(
        share = 100 * n / sum(n), 
        lab = case_when(
          share == 100 ~ "100%",
          share >= 99.5 ~ ">99%",  # Share of region is never 100%
          .default = prettylabel(share, pct = TRUE)
        )
      ) |> 
      arrange(desc(share)) |> 
      mutate(
        csum = cumsum(share),
        region = case_when(
          region == "Americas" ~ "the #b[Americas]",
          .default = paste0("#b[", region, "]")
        )
      )
    
    # List down regions until cumulative share is at least 75%
    cum <- 0
    row <- 1
    reg <- tibble()
    while (cum <= 75) {
      reg <- bind_rows(reg, reg_all[row, ])
      cum <- cum + reg_all[row, ]$csum
      row <- row + 1
    }
    
    mostly <- "Refugees are mostly hosted in"
    all <- "Refugees are hosted in"
    if (nrow(reg) == 1) {
      if (reg$share == 100) {
        regions <- str_glue("{all} { reg$region[1] } ({ reg$lab[1] })")
      } else {
        regions <- str_glue("{mostly} { reg$region[1] } ({ reg$lab[1] })")
      }
    }
    if (nrow(reg) == 2) {
      regions <- str_glue(paste(
        "{mostly} { reg$region[1] } ({ reg$lab[1] }) and", 
        "{ reg$region[2] } ({ reg$lab[2] })"
      ))
    }
    if (nrow(reg) == 3) {
      regions <- str_glue(paste(
        "{mostly} { reg$region[1] } ({ reg$lab[1] }),", 
        "{ reg$region[2] } ({ reg$lab[2] }), and",
        "{ reg$region[3] } ({ reg$lab[3] })"
      ))
    }
    if (nrow(reg) == 4) {
      regions <- str_glue(paste(
        "{mostly} { reg$region[1] } ({ reg$lab[1] }),", 
        "{ reg$region[2] } ({ reg$lab[2] }),", 
        "{ reg$region[3] } ({ reg$lab[3] }), and",
        "{ reg$region[4] } ({ reg$lab[4] })"
      ))
    }
    if (nrow(reg) == 5) {
      regions <- str_glue(paste(
        "{mostly} { reg$region[1] } ({ reg$lab[1] }),", 
        "{ reg$region[2] } ({ reg$lab[2] }),", 
        "{ reg$region[3] } ({ reg$lab[3] }),", 
        "{ reg$region[4] } ({ reg$lab[4] }), and",
        "{ reg$region[5] } ({ reg$lab[5] })"
      ))
    }
    
    caption <- str_glue(paste(
      " - By UNHCR estimates, refugees from {name} numbered",
      "#b[{pl(data_iso_t1)}] {get_pct(data_t1$n, data_iso_t1)} as of {t1},",
      "equivalent to #b[{pl_pct(share_iso_t1)}]",
      "{get_pct(share_t1$v, share_iso_t1)} of its population. {change_text}\n",
      " - {regions}."
    ))
  }
  
  return(caption)
}


caption_refug_host <- function(iso) {
  
  name <- namer(iso)
  
  data <- left_join(unhcr, countrynames, by = c("from" = "iso3")) |> 
    filter(group == "refugee") |> 
    summarise(n = sum(n), .by = c(geo, t, from, region)) |> 
    mutate(region = case_when(is.na(region) ~ "Unknown", .default = region))
  data_iso <- filter(data, geo == iso) |> 
    summarise(n = sum(n), .by = c(geo, t))
  
  if (nrow(data_iso) == 0) {
    
    caption <- str_glue(
      "UNHCR has no information on refugees hosted in {name}."
    )
    
  } else {
    
    data_reg <- filter(data, geo == iso) |> 
      summarise(n = sum(n), .by = c(region, t))
    
    t1 <- max(data_iso$t) |> as.numeric()
    t0 <- t1 - 1
    # t0 <- min(data_iso$t) |> as.numeric()
    
    data_t0t1 <- filter(unhcr, group == "refugee" & t %in% c(t0, t1)) |> 
      summarise(n = sum(n), .by = c(geo, t)) |> 
      mutate(t = case_when(t == t0 ~ "col_t0", t == t1 ~ "col_t1")) |> 
      pivot_wider(names_from = t, values_from = n) |> 
      drop_na() |> 
      mutate(change = 100 * ((col_t1 / col_t0)^(1 / (t1 - t0)) - 1))
    
    data_t0 <- filter(unhcr, group == "refugee" & t == t0) |> 
      summarise(n = sum(n), .by = geo)
    data_t1 <- filter(unhcr, group == "refugee" & t == t1) |> 
      summarise(n = sum(n), .by = geo)

    data_iso_t0 <- filter(data_iso, t == t0) |> pull(n)
    data_iso_t1 <- filter(data_iso, t == t1) |> pull(n)
    
    # Shares of population
    pop_t1 <- filter(wdi, var == "pop" & t == t1) |> rename(pop = v)
    share_t1 <- left_join(data_t1, pop_t1, by = "geo") |> 
      mutate(v = 100 * n / pop) |> 
      drop_na()
    share_iso_t1 <- filter(share_t1, geo == iso) |> pull(v)
    
    # CAGR from earliest to latest
    change <- data_t0t1$change
    # change <- 100 * ((data_t1$n / data_t0$n)^(1 / (t1 - t0)) - 1)
    change_iso <- 100 * ((data_iso_t1 / data_iso_t0)^(1 / (t1 - t0)) - 1)
    change_iso_lab <- pl_pct1(change_iso)
    if (change_iso > 0) change_iso_lab <- paste0("+", pl_pct1(change_iso))
    
    reg_all <- filter(data_reg, t == t1) |> 
      mutate(
        share = 100 * n / sum(n), 
        lab = case_when(
          share >= 99.5 ~ ">99%",  # Share of region is never 100%
          .default = prettylabel(share, pct = TRUE)
        )
      )
    
    reg_unknown <- filter(reg_all, region == "Unknown")
    
    unknown_share <- reg_unknown$share
    if (nrow(reg_unknown) == 0) unknown_share <- 0
    
    if (100 - unknown_share >= 75) {
      
      reg_all <- filter(reg_all, region != "Unknown") |> 
        arrange(desc(share)) |> 
        mutate(
          csum = cumsum(share),
          region = case_when(
            region == "Americas" ~ "the #b[Americas]",
            .default = paste0("#b[", region, "]")
          ))
      
      cum <- 0
      row <- 1
      reg <- tibble()
      while (cum <= 75) {
        reg <- bind_rows(reg, reg_all[row, ])
        cum <- cum + reg_all[row, ]$csum
        row <- row + 1
      }
      
      if (nrow(reg) == 1) {
        regions <- str_glue("{ reg$region[1] } ({ reg$lab[1] })")
      }
      if (nrow(reg) == 2) {
        regions <- str_glue(paste(
          "{ reg$region[1] } ({ reg$lab[1] }) and", 
          "{ reg$region[2] } ({ reg$lab[2] })"
        ))
      }
      if (nrow(reg) == 3) {
        regions <- str_glue(paste(
          "{ reg$region[1] } ({ reg$lab[1] }),", 
          "{ reg$region[2] } ({ reg$lab[2] }), and",
          "{ reg$region[3] } ({ reg$lab[3] })"
        ))
      }
      if (nrow(reg) == 4) {
        regions <- str_glue(paste(
          "{ reg$region[1] } ({ reg$lab[1] }),", 
          "{ reg$region[2] } ({ reg$lab[2] }),", 
          "{ reg$region[3] } ({ reg$lab[3] }), and",
          "{ reg$region[4] } ({ reg$lab[4] })"
        ))
      }
      if (nrow(reg) == 5) {
        regions <- str_glue(paste(
          "{ reg$region[1] } ({ reg$lab[1] }),", 
          "{ reg$region[2] } ({ reg$lab[2] }),", 
          "{ reg$region[3] } ({ reg$lab[3] }),", 
          "{ reg$region[4] } ({ reg$lab[4] }), and",
          "{ reg$region[5] } ({ reg$lab[5] })"
        ))
      }
      
      unknown <- ""
      if (nrow(reg_unknown) > 0) {
        if (reg_unknown$share > 4.5) {
          unknown <- str_glue("Another {reg_unknown$lab} had unknown origins.")
        }
      }
      
      regions_text <- str_glue(
        "Hosted refugees mostly came from {regions}. {unknown}"
      )
      
    } else {
      
      regions_text <- str_glue(
        "Hosted refugees mostly ({reg_unknown$lab}) had unknown origins."
      )
    }
    
    caption <- str_glue(paste(
      " - By UNHCR estimates, refugees hosted in {name} numbered",
      "#b[{pl(data_iso_t1)}] {get_pct(data_t1$n, data_iso_t1)} as of {t1},", 
      "equivalent to #b[{pl_pct(share_iso_t1)}]",
      "{get_pct(share_t1$v, share_iso_t1)} of its population. This was a", 
      "#b[{change_iso_lab}] {get_pct(change, change_iso)} change from the",
      "previous year.\n",
      " - {regions_text}"
    ))
  }
  
  return(caption)
}






# Internal displacements --------------------------------------------------

caption_idp <- function(iso) {
  
  name <- namer(iso)
  
  data <- filter(idmc_flows, t >= max(t) - 9 & n > 0)
  data_iso <- filter(data, geo == iso)
  t0 <- min(data$t) |> as.numeric()
  t1 <- max(data$t) |> as.numeric()
  
  if (nrow(data_iso) == 0) {
    
    caption <- str_glue(paste(
      " - Over {t0}–{t1}, IDMC reports no displacements caused by",
      "natural disasters in {name}.\n",
      " - There were also no reported displacements caused by conflicts."
    ))
    
    return(caption)
  }
  
  data_agg <- summarise(data, n = sum(n), .by = c(geo, cause))
  data_iso_agg <- summarise(data_iso, n = sum(n), .by = c(geo, cause))
  
  data_agg_conflict <- filter(data_agg, cause == "conflict")
  data_iso_agg_conflict <- filter(data_iso_agg, cause == "conflict")
  
  if (nrow(data_iso_agg_conflict) == 0) {
    conflict <- str_glue(
      "the IDMC reports no displacements caused by conflicts"
    )
  } else {
    conflict <- str_glue(paste(
      "conflicts caused a total of #b[{pl(data_iso_agg_conflict$n)}]",
      "{get_pct(data_agg_conflict$n, data_iso_agg_conflict$n)}",
      "displacements"
    ))
  }
  
  data_iso_agg_disaster <- filter(data_iso_agg, cause != "conflict")
  
  if (nrow(data_iso_agg_disaster) == 0) {
    disaster <- str_glue(
      "Over {t0}–{t1}, IDMC reports no displacements caused by natural",
      "disasters in {name}"
    )
  } else {
    
    data_iso_agg_top3 <- filter(data_iso_agg, cause != "conflict") |> 
      arrange(desc(n)) |> 
      mutate(
        rank = 1:n(),
        cause_lab = case_when(
          cause == "volcanic activity" ~ "volcanic activities",
          .default = paste0(cause, "s")
        )
      ) |> 
      filter(rank <= 3)
    
    data_agg_top1 <- filter(data_agg, cause == data_iso_agg_top3$cause[1])
    data_agg_top2 <- filter(data_agg, cause == data_iso_agg_top3$cause[2])
    data_agg_top3 <- filter(data_agg, cause == data_iso_agg_top3$cause[3])
    
    if (nrow(data_iso_agg_top3) == 1) {
      natdis <- str_glue(paste(
        "#b[{data_iso_agg_top3$cause_lab[1]}] caused a total of",
        "#b[{pl(data_iso_agg_top3$n[1])}]", 
        "{get_pct(data_agg_top1$n, data_iso_agg_top3$n[1])} displacements"
      ))
    }
    if (nrow(data_iso_agg_top3) == 2) {
      natdis <- str_glue(paste(
        "#b[{data_iso_agg_top3$cause_lab[1]}] caused a total of",
        "#b[{pl(data_iso_agg_top3$n[1])}]", 
        "{get_pct(data_agg_top1$n, data_iso_agg_top3$n[1])} displacements and",
        "#b[{data_iso_agg_top3$cause_lab[2]}] caused",
        "#b[{pl(data_iso_agg_top3$n[2])}]", 
        "{get_pct(data_agg_top1$n, data_iso_agg_top3$n[2])} displacements"
      ))
    }
    if (nrow(data_iso_agg_top3) == 3) {
      natdis <- str_glue(paste(
        "#b[{data_iso_agg_top3$cause_lab[1]}] caused a total of",
        "#b[{pl(data_iso_agg_top3$n[1])}]", 
        "{get_pct(data_agg_top1$n, data_iso_agg_top3$n[1])} displacements,",
        "#b[{data_iso_agg_top3$cause_lab[2]}] caused",
        "#b[{pl(data_iso_agg_top3$n[2])}]", 
        "{get_pct(data_agg_top1$n, data_iso_agg_top3$n[2])} displacements, and",
        "#b[{data_iso_agg_top3$cause_lab[3]}] caused",
        "#b[{pl(data_iso_agg_top3$n[3])}]", 
        "{get_pct(data_agg_top1$n, data_iso_agg_top3$n[3])} displacements"
      ))
    }
    
    disaster <- str_glue("Over {t0}–{t1}, the IDMC reports that {natdis}")
  }
  
  caption <- str_glue(paste(
    " - {disaster}.\n",
    " - Over the same period, {conflict}."
  ))
  
  return(caption)
}


# Missing migrants --------------------------------------------------------

caption_mmp <- function(iso) {
  
  name <- namer(iso)
  
  data <- summarise(iom_mmp, n = sum(dead), .by = geo)
  data_iso <- filter(data, geo == iso)
  
  t0 <- min(iom_mmp$t, na.rm = TRUE) |> year()
  t1 <- max(iom_mmp$t, na.rm = TRUE) |> year()
  
  if (nrow(data_iso) == 0) {
    
    caption <- str_glue(paste(
      " - In the territory of {name} over {t0}–{t1}, IOM recorded no deaths or",
      "disappearances of people in the act of international migration.\n",
      " - Note that given severe data limitations, this does not imply that no",
      "migrant deaths occurred in this territory during this period."
    ))
    
  } else {
    
    data_iso_cause <- filter(iom_mmp, geo == iso) |> 
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
      " - {cause}"
    ))
  }
  
  return(caption)
}


# Remittances -------------------------------------------------------------

caption_remin <- function(iso) {
  
  name <- namer(iso)
  
  data <- filter(wdi, var == "remin" & v > 0) |> 
    left_join(filter(wdi, var == "gdp"), by = c("geo", "t")) |> 
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
  
  data <- filter(wdi, var == "remout" & v > 0) |> 
    left_join(filter(wdi, var == "gdp"), by = c("geo", "t")) |> 
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
  
  data <- filter(wdi, var == "fdiin" & v > 0) |> 
    left_join(filter(wdi, var == "gdp"), by = c("geo", "t")) |> 
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
      drop_na() |> 
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
    if (t0 == t1) period <- str_glue("In {t1}")
    
    data_t1_latest <- filter(data, t == t1_latest) |> 
      pull(fdiin)
    data_iso_t1_latest <- filter(data_iso_latest, t == t1_latest) |> 
      pull(fdiin)
    
    caption <- str_glue(paste(
      " - The World Bank reports that inbound FDI to {name} amounted to",
      "#b[{pl_usd(data_iso_t1_latest)}]",
      "{get_pct(data_t1_latest, data_iso_t1_latest)} in {t1_latest}. {period},",
      "these were #b[{pl_pct(sh_iso_t0_t1)}] {get_pct(sh_t0_t1, sh_iso_t0_t1)}",
      "of GDP."
    ))
  }
  
  return(caption)
}

caption_fdiout <- function(iso) {
  
  name <- namer(iso)
  
  data <- filter(wdi, var == "fdiout" & v > 0) |> 
    full_join(filter(wdi, var == "gdp"), by = c("geo", "t")) |> 
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
      drop_na() |> 
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
    if (t0 == t1) period <- str_glue("In {t1}")
    
    data_t1_latest <- filter(data, t == t1_latest) |> 
      pull(fdiout)
    data_iso_t1_latest <- filter(data_iso_latest, t == t1_latest) |> 
      pull(fdiout)
    
    caption <- str_glue(paste(
      " - Outbound FDI, meanwhile, amounted to",
      "#b[{pl_usd(data_iso_t1_latest)}]",
      "{get_pct(data_t1_latest, data_iso_t1_latest)} in {t1_latest}. {period},",
      "these were #b[{pl_pct(sh_iso_t0_t1)}] {get_pct(sh_t0_t1, sh_iso_t0_t1)}",
      "of GDP."
    ))
  }
  
  return(caption)
}


# Population --------------------------------------------------------------

caption_pop <- function(iso) {
  
  name <- namer(iso)
  
  data <- filter(wdi, var == "pop")
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
      "{get_pct(data_t1, data_iso_t1)} as of {t1}. The world median was",
      "{pl(med_t1)}."
    ))
  }
  
  return(caption)
}


# Birth rate --------------------------------------------------------------

caption_birth <- function(iso) {
  
  name <- namer(iso)
  
  data <- filter(wdi, var == "birth")
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
      "world median was {pl(med_t1)}."
    ))
  }
  
  return(caption)
}





# Population pyramid ------------------------------------------------------

caption_pyr <- function(iso) {
  
  name <- namer(iso)
  
  t1 <- max(undesa_wpp$t)
  
  med <- filter(undesa_wpp, t == max(t)) |> 
    summarise(med = weighted.median(age, n), .by = geo)
  med_iso <- filter(med, geo == iso) |> pull(med)
  
  age <- filter(undesa_wpp, t == max(t)) |> 
    mutate(age = case_when(
      age %in% 0:14 ~ "children", 
      age %in% 15:64 ~ "working", 
      .default = "elderly" 
    )) |> 
    summarise(n = sum(n), .by = c(geo, age)) |> 
    mutate(v = 100 * n / sum(n), .by = geo)
  
  sex <- filter(undesa_wpp, t == max(t)) |> 
    summarise(n = sum(n), .by = c(geo, sex)) |> 
    mutate(v = 100 * n / sum(n), .by = geo) |> 
    filter(sex == "female")
  
  age_iso <- filter(age, geo == iso)
  sex_iso <- filter(sex, geo == iso)
  
  if (nrow(age_iso) == 0) {
    
    caption <- str_glue(
      "UN DESA has no demographic information on the population of {name}."
    )
    
  } else {
    
    age_child <- filter(age, age == "children")
    age_work <- filter(age, age == "working")
    age_elder <- filter(age, age == "elderly")
    
    age_child_iso <- filter(age_child, geo == iso) |> pull(v)
    age_work_iso <- filter(age_work, geo == iso) |> pull(v)
    age_elder_iso <- filter(age_elder, geo == iso) |> pull(v)
    
    sex_iso <- filter(sex, geo == iso) |> pull(v)
    
    caption <- str_glue(paste(
      " - UN DESA estimates that the {t1} population of {name} comprised of",
      "#b[{pl_pct(age_child_iso)}] {get_pct(age_child$v, age_child_iso)}",
      "children (0-14), #b[{pl_pct(age_work_iso)}]",
      "{get_pct(age_work$v, age_work_iso)} working age (15-64), and",
      "#b[{pl_pct(age_elder_iso)}] {get_pct(age_elder$v, age_elder_iso)}",
      "elderly (65+). The median age was #b[{pl(med_iso)}]",
      "{get_pct(med$med, med_iso)}.\n",
      " - #b[{pl_pct(sex_iso)}] {get_pct(sex$v, sex_iso)} of the population",
      "were female in {t1}.\n",
      " - The map shows where populations are concentrated."
    ))
  }
  
  return(caption)
}


# Inflation rate ----------------------------------------------------------

caption_inf <- function(iso) {
  
  name <- namer(iso)
  
  data <- filter(wdi, var == "inf")
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
    
    change <- round(data_iso_t1_latest - data_iso_tL_latest, digits = 1)
    if (change == 0) verb <- "unchanged"
    if (change > 0) verb <- "up"
    if (change < 0) verb <- "down"
    
    caption <- str_glue(paste(
      "The World Bank reports that the inflation rate in {name} was",
      "#b[{pl_pct1(data_iso_t1_latest)}]",
      "{get_pct(data_t1_latest, data_iso_t1_latest)} in {t1_latest},",
      "#b[{verb}] from {pl_pct1(data_iso_tL_latest)} the previous year. The",
      "world median was {pl_pct1(med_t1)}."
    ))
  }
  
  return(caption)
}


# Societal division -------------------------------------------------------

caption_grieve <- function(iso) {
  
  name <- namer(iso)
  
  data <- filter(wdi, var == "grieve")
  data_iso <- filter(data, geo == iso)
  
  intro <- paste(
    "The Group Grievance Index measures societal division on a scale from 0 to",
    "10, with 10 indicating the most division. It is a subcomponent of the",
    "Fund for Peace's Fragile States Index."
  )
  
  if (nrow(data_iso) == 0) {
    
    caption <- str_glue(paste(
      " - {intro}\n",
      " - The Fragile States Index does not cover {name}."
    ))
    
  } else {
    
    t0 <- min(data_iso$t) |> as.numeric()
    t1 <- max(data_iso$t) |> as.numeric()
    
    data_t1 <- filter(data, t == t1) |> pull(v)
    med_t1 <- median(data_t1, na.rm = TRUE)
    data_iso_t1 <- filter(data_iso, t == t1) |> pull(v)
    
    comparison <- str_glue(
      "This is in line with the world median of {pl1(med_t1)}."
    )
    if (data_iso_t1 > med_t1 + .1) {
      comparison <- str_glue(paste(
        "Compared to the world median of {pl1(med_t1)}, this indicates",
        "#b[relatively high] societal division."
      ))
    }
    if (data_iso_t1 < med_t1 - .1) {
      comparison <- str_glue(paste(
        "Compared to the world median of {pl1(med_t1)}, this indicates",
        "#b[relatively low] societal division."
      ))
    }

    caption <- str_glue(paste(
      " - {intro}\n",
      " - In {t1}, {name} had a Group Grievance Index of",
      "#b[{pl1(data_iso_t1)}] {get_pct(data_t1, data_iso_t1)}. {comparison}"
    ))
  }
  
  return(caption)
}


# Political stability -----------------------------------------------------

caption_polstb <- function(iso) {
  
  name <- namer(iso)
  
  data <- filter(wdi, var == "polstb")
  data_iso <- filter(data, geo == iso)
  
  intro <- paste(
    "This measures perceptions of the likelihood of political instability and",
    "violence, expressed as standard deviations from the mean (more negative",
    "means greater likelihood of instability)."
  )
  
  if (nrow(data_iso) == 0) {
    
    caption <- str_glue(paste(
      " - {intro}\n",
      " - The World Governance wdi do not cover {name}."
    ))
    
  } else {
    
    t0 <- min(data_iso$t) |> as.numeric()
    t1 <- max(data_iso$t) |> as.numeric()
    
    data_t1 <- filter(data, t == t1) |> pull(v)
    data_iso_t1 <- filter(data_iso, t == t1) |> pull(v)
    
    data_iso_t1_lab <- pl1(data_iso_t1)
    adj <- ""
    if(data_iso_t1 > 0) data_iso_t1_lab <- paste0("+", data_iso_t1_lab)
    if (data_iso_t1 <= -.045) adj <- "above-"
    if (data_iso_t1 >= .045) adj <- "below-"

    caption <- str_glue(paste(
      " - {intro}\n",
      " - In {t1}, {name} scored #b[{data_iso_t1_lab}]",
      "{get_pct(data_t1, data_iso_t1)} points, indicating #b[{adj}average]",
      "perceived likelihood of political instability."
    ))
  }
  
  return(caption)
}


# Affected by disasters ---------------------------------------------------

caption_emdat <- function(iso) {
  
  name <- namer(iso)
  
  data <- filter(emdat_disaster, t >= max(t) - 9) |> 
    drop_na(affected) |> 
    select(geo, type, t, n = affected)
  data_iso <- filter(data, geo == iso)
  
  t0 <- min(data$t) |> as.numeric()
  t1 <- max(data$t) |> as.numeric()
  
  if (nrow(data_iso) == 0) {
    
    t0 <- min(emdat_disaster$t)
    t1 <- max(emdat_disaster$t)
    
    caption <- str_glue(paste(
      "EM-DAT does not have data for people affected by disasters in {name}",
      "during {t0}–{t1}."
    ))
    
  } else {
    
    pop <- filter(wdi, var == "pop") |> select(geo, t, pop = v)
    agg <- data |> 
      summarise(n = sum(n), .by = c(geo, t)) |> 
      left_join(pop, by = c("geo", "t")) |> 
      mutate(pc = 10000 * n / pop) |> 
      summarise(
        n = sum(n, na.rm = TRUE), 
        pc = mean(pc, na.rm = TRUE), 
        .by = geo
      )
    agg_iso <- filter(agg, geo == iso)
    
    total <- str_glue(paste(
      "Over {t0}–{t1}, EM-DAT reports a total of #b[{pl(agg_iso$n)}]",
      "{get_pct(agg$n, agg_iso$n)} people affected by disasters in {name},",
      "amounting to #b[{pl(agg_iso$pc)}] {get_pct(agg$pc, agg_iso$pc)} people",
      "per 10,000 population a year on average"
    ))
    
    type <- data |> 
      left_join(pop, by = c("geo", "t")) |> 
      mutate(pc = 10000 * n / pop) |> 
      summarise(
        n = sum(n, na.rm = TRUE), 
        pc = mean(pc, na.rm = TRUE), 
        .by = c(geo, type)
      ) |> 
      mutate(sh = 100 * n / sum(n), .by = c(geo))
    
    type_iso <- filter(type, geo == iso) |> 
      arrange(desc(n)) |> 
      slice_head(n = 3)
    
    type_top1 <- filter(type, type == type_iso$type[1]) |> pull(pc)
    type_top2 <- filter(type, type == type_iso$type[2]) |> pull(pc)
    type_top3 <- filter(type, type == type_iso$type[3]) |> pull(pc)
    
    type_iso <- type_iso |> 
      mutate(type = case_when(
        type == "volanic activity" ~ "#b[volanic activities]", 
        .default = paste0("#b[", type, "s]")
      ))
    
    if (nrow(type_iso) == 1) {
      cause <- str_glue(paste(
        "The top cause were #b[{type_iso$type[1]}] ({pl_pct(type_iso$sh[1])}),",
        "which affected an average of {pl(type_iso$pc[1])}",
        "{get_pct(type_top1, type_iso$sh[1])} people per 10,000 a year"
      ))
    }
    
    if (nrow(type_iso) == 2) {
      cause <- str_glue(paste(
        "The top causes were #b[{type_iso$type[1]}]",
        "({pl_pct(type_iso$sh[1])}), which affected an average of",
        "{pl(type_iso$pc[1])} {get_pct(type_top1, type_iso$sh[1])} people per",
        "10,000 a year, and #b[{type_iso$type[2]}] ({pl_pct(type_iso$sh[2])}),",
        "which affected {pl(type_iso$pc[2])}",
        "{get_pct(type_top2, type_iso$sh[2])} people per 10,000 a year"
      ))
    }
    
    if (nrow(type_iso) == 3) {
      cause <- str_glue(paste(
        "The top causes were #b[{type_iso$type[1]}]",
        "({pl_pct(type_iso$sh[1])}), which affected an average of",
        "{pl(type_iso$pc[1])} {get_pct(type_top1, type_iso$sh[1])} people per",
        "10,000 a year, #b[{type_iso$type[2]}] ({pl_pct(type_iso$sh[2])}),",
        "which affected {pl(type_iso$pc[2])}",
        "{get_pct(type_top2, type_iso$sh[2])} people per 10,000 a year, and",
        "#b[{type_iso$type[3]}] ({pl_pct(type_iso$sh[3])}), which affected",
        "{pl(type_iso$pc[3])} {get_pct(type_top3, type_iso$sh[3])} people per",
        "10,000 a year"
      ))
    }
    
    caption <- str_glue("{total}. {cause}.")
  }
  
  return(caption)
}

