

# Employment --------------------------------------------------------------

# Total
get_emp <- function(iso = NULL) {
  
  output <- list(data = NULL, range = NULL)
  
  t0 <- 2000
  t1 <- max(empsexeduc$t)
  
  order <- c("Native-born", "Foreign-born")
  
  dataset <- empsexeduc |> 
    filter(t >= t0, birth != "Unknown") |> 
    summarise(n = sum(n), .by = c(geo, birth, t)) |> 
    complete(geo, birth, t = t0:t1) |> 
    mutate(birth = factor(birth, levels = order))
  
  if (is.null(iso)) {
    
    output$data <- dataset
    output$range <- c(t0, t1) |> as.integer()
    
  } else {
    
    data_iso <- filter(dataset, geo == iso)
    output$data <- data_iso
    
    if (nrow(data_iso) > 0) {
      t_iso <- drop_na(data_iso)$t
      t0_iso <- max(2000, min(t_iso))
      t1_iso <- max(t_iso)
      output$range <- c(t0_iso, t1_iso) |> as.integer()
    }
  }
  
  return(output)
}

# By education
get_empeduc <- function(iso = NULL) {
  
  output <- list(data = NULL, range = NULL)
  
  t1 <- max(empsexeduc$t)
  
  order_birth <- c("Native-born", "Foreign-born")
  order_sex = c("Male", "Female")
  order_educ <- c(
    "Less than basic", 
    "Basic", 
    "Intermediate", 
    "Advanced", 
    "Unknown"
  )
  
  dataset <- empsexeduc |> 
    filter(t == max(t), .by = geo) |> 
    mutate(v = n / sum(n), .by = c(geo, t)) |> 
    filter(birth != "Unknown") |> 
    mutate(
      birth = factor(birth, levels = order_birth),
      sex = factor(sex, levels = order_sex),
      educ = factor(educ, levels = order_educ),
    )
  
  if (is.null(iso)) {
    output$data <- dataset
    output$range <- c(t1, t1) |> as.integer()
    
  } else {
    data_iso <- filter(dataset, geo == iso)
    output$data <- data_iso
    
    if (nrow(data_iso) > 0) {
      output$range <- c(data_iso$t[1], data_iso$t[1]) |> as.integer()
    }
  }
  
  return(output)
}


# Unemployment rate -------------------------------------------------------

# Aggregate
get_unemrate <- function(iso = NULL) {
  
  output <- list(data = NULL, range = NULL)
  
  t0 <- 2000
  t1 <- max(unemsexeduc$t)
  
  order <- c("Native-born", "Foreign-born")
  
  dataset <- unemsexeduc |> 
    filter(sex == "Total", educ == "Total", birth != "Unknown") |> 
    complete(geo, birth, t = t0:t1) |> 
    mutate(birth = factor(birth, levels = order))
  
  if (is.null(iso)) {
    
    output$data <- dataset
    output$range <- c(t0, t1) |> as.integer()
    
  } else {
    
    data_iso <- filter(dataset, geo == iso)
    output$data <- data_iso
    
    if (nrow(data_iso) > 0) {
      t_iso <- drop_na(data_iso)$t
      t0_iso <- max(2000, min(t_iso))
      t1_iso <- max(t_iso)
      output$range <- c(t0_iso, t1_iso) |> as.integer()
    }
  }
  
  return(output)
}

# By education
get_unemeduc <- function(iso = NULL) {
  
  output <- list(data = NULL, range = NULL)
  
  t1 <- max(unemsexeduc$t)
  
  order_birth <- c("Native-born", "Foreign-born")
  order_sex = c("Male", "Female", "Total")
  order_educ <- c(
    "Less than basic", 
    "Basic", 
    "Intermediate", 
    "Advanced", 
    "Unknown",
    "Total"
  )
  
  dataset <- unemsexeduc |> 
    filter(t == max(t), .by = geo) |> 
    filter(birth != "Unknown") |> 
    mutate(
      birth = factor(birth, levels = order_birth),
      sex = factor(sex, levels = order_sex),
      educ = factor(educ, levels = order_educ),
    )
  
  if (is.null(iso)) {
    output$data <- dataset
    output$range <- c(t1, t1) |> as.integer()
    
  } else {
    data_iso <- filter(dataset, geo == iso)
    output$data <- data_iso
    
    if (nrow(data_iso) > 0) {
      output$range <- c(data_iso$t[1], data_iso$t[1]) |> as.integer()
    }
  }
  
  return(output)
}


# Earnings ----------------------------------------------------------------

get_earnings <- function(iso = NULL) {
  
  output <- list(data = NULL, range = NULL)
  
  t0 <- 2000
  t1 <- max(earnings$t)
  
  order <- c("Native", "Foreign")
  
  dataset <- earnings |> 
    filter(t >= t0) |> 
    filter(status != "Total", sex == "Total", status != "Unknown") |> 
    mutate(status = factor(status, levels = order)) |> 
    select(-sex)
  
  if (is.null(iso)) {
    output$data <- dataset
    output$range <- c(t0, t1) |> as.integer()
    
  } else {
    data_iso <- filter(dataset, geo == iso) |> 
      complete(geo, type, status, t = t0:t1)
    output$data <- data_iso
    
    if (nrow(data_iso) > 0) {
      t_iso <- drop_na(data_iso)$t
      t0_iso <- max(2000, min(t_iso))
      t1_iso <- max(t_iso)
      output$range <- c(t0_iso, t1_iso) |> as.integer()
    }
  }
  
  return(output)
}


# Working hours -----------------------------------------------------------

get_hours <- function(iso = NULL) {
  
  output <- list(data = NULL, range = NULL)
  
  t0 <- 2000
  t1 <- max(hours$t)
  
  order <- c("Native", "Foreign")
  
  dataset <- hours |> 
    filter(t >= t0) |> 
    filter(status != "Total", sex == "Total", status != "Unknown") |> 
    mutate(status = factor(status, levels = order)) |> 
    select(-sex)
  
  if (is.null(iso)) {
    output$data <- dataset
    output$range <- c(t0, t1) |> as.integer()
    
  } else {
    data_iso <- filter(dataset, geo == iso) |> 
      complete(geo, type, status, t = t0:t1)
    output$data <- data_iso
    
    if (nrow(data_iso) > 0) {
      t_iso <- drop_na(data_iso)$t
      t0_iso <- max(2000, min(t_iso))
      t1_iso <- max(t_iso)
      output$range <- c(t0_iso, t1_iso) |> as.integer()
    }
  }
  
  return(output)
}


# Employment by sector and by place of birth ------------------------------

get_empsector <- function(iso = NULL) {
  
  output <- list(data = NULL, range = NULL)
  
  t1 <- max(empsector$t)
  
  order_sector <- sectors$code
  
  dataset <- empsector |> 
    filter(t == max(t), .by = geo) |> 
    mutate(
      sector = case_when(
        sector %in% c("D", "E") ~ "D-E",
        sector %in% c("T", "U", "X") ~ "T-X",
        .default = sector
      ),
      sector = factor(sector, levels = order_sector)
    ) |> 
    summarise(n = sum(n), .by = c(geo, t, birth, sector)) |> 
    pivot_wider(
      names_from = birth,
      values_from = n,
      values_fill = 0
    ) |> 
    mutate(total = `Native-born` + `Foreign-born` + Unknown) |> 
    mutate(v = `Foreign-born` / (`Native-born` + `Foreign-born` + Unknown)) |> 
    select(geo, t, sector, v)

  if (is.null(iso)) {
    output$data <- dataset
    output$range <- c(t1, t1) |> as.integer()
    
  } else {
    data_iso <- filter(dataset, geo == iso) |> 
      complete(geo, t, sector)
    output$data <- data_iso
    
    if (nrow(data_iso) > 0) {
      output$range <- c(data_iso$t[1], data_iso$t[1]) |> as.integer()
    }
  }
  
  return(output)
}


# Value added -------------------------------------------------------------

get_gva <- function(iso = NULL) {
  
  output <- list(data = NULL, range = NULL)
  
  dataset <- gva |> 
    filter(t %in% c(max(t) - 5, max(t)), .by = geo) |> 
    mutate(sector = factor(sector, levels = gva_sectors)) |> 
    complete(geo, sector, t, fill = list(v = 0))
  
  t0 <- min(dataset$t)
  t1 <- max(dataset$t)
  
  if (is.null(iso)) {
    output$data <- dataset
    output$range <- c(t0, t1) |> as.integer()
    
  } else {
    data_iso <- filter(dataset, geo == iso)
    output$data <- data_iso
    
    if (nrow(data_iso) > 0) {
      output$range <- c(t0, t1) |> as.integer()
    }
  }
  
  return(output)
}


# Exports -----------------------------------------------------------------

get_exports <- function(iso = NULL) {
  
  output <- list(data = NULL, range = NULL)
  
  dataset <- exports |> 
    filter(t %in% c(min(t), max(t)), .by = geo) |> 
    mutate(sector = factor(sector, levels = exports_sectors)) |> 
    complete(geo, sector, t, fill = list(v = 0))
  
  t0 <- min(dataset$t)
  t1 <- max(dataset$t)
  
  if (is.null(iso)) {
    output$data <- dataset
    output$range <- c(t0, t1) |> as.integer()
    
  } else {
    data_iso <- filter(dataset, geo == iso)
    output$data <- data_iso
    
    if (nrow(data_iso) > 0) {
      output$range <- c(t0, t1) |> as.integer()
    }
  }
  
  return(output)
}


# Sector/skill level ------------------------------------------------------

get_empoccup <- function(iso = NULL) {
  
  output <- list(data = NULL, range = NULL)
  
  dataset <- empoccup |> 
    filter(t == max(t), .by = geo) |> 
    mutate(
      sector = factor(sector, levels = sectors$code),
      skill = factor(skill, levels = c("high", "medium", "low", "unknown")),
      v = n / sum(n),
      .by = c(geo, t, sector)
    )

  t1 <- max(dataset$t)
  
  if (is.null(iso)) {
    output$data <- dataset
    output$range <- c(t1, t1) |> as.integer()
    
  } else {
    data_iso <- filter(dataset, geo == iso)
    output$data <- data_iso
    
    if (nrow(data_iso) > 0) {
      t1 <- max(data_iso$t)
      output$range <- c(t1, t1) |> as.integer()
    }
  }
  
  return(output)
}


get_unemoccup <- function(iso = NULL) {
  
  output <- list(data = NULL, range = NULL)
  
  if (!is.null(iso)) {

    dataset <- unemsector |> 
      filter(geo == iso)
    
    if (nrow(dataset) > 0) {
      
      dataset <- dataset |> 
        filter(t == max(t)) |> 
        mutate(
          sector = factor(sector, levels = sectors$code),
          v = n / sum(n),
          .by = t
        )
      
      t1 <- dataset$t[1]
      
      output$range <- c(t1, t1) |> as.integer()
    }
    
    output$data <- dataset
  }
  
  return(output)
}


# Employment by skill level -----------------------------------------------

get_empskill <- function(iso = NULL) {
  
  output <- list(data = NULL, range = NULL)
  
  emp_by_skill <- empsexoccup |> 
    left_join(isco_occupations, by = c("occup" = "code")) |> 
    filter(t == max(t), .by = geo) |> 
    summarise(n = sum(n), .by = c(geo, t, sex, skill)) |> 
    mutate(v = n / sum(n), .by = geo)
  
  if (!is.null(iso)) {
    
    # Aggregate globally
    emp_by_skill_world <- emp_by_skill |> 
      summarise(world_n = sum(n), .by = c(sex, skill)) |> 
      mutate(world_v = world_n / sum(world_n))
    
    # Aggregate by immigrant origin countries
    orig_shares <- gdidata::undesa_stocks_2020 |> 
      summarise(n = sum(n), .by = c(geo, t, from)) |> 
      filter(geo == iso, t == max(t)) |> 
      mutate(orig_sh = n / sum(n)) |> 
      select(geo = from, orig_n = n, orig_sh)
    
    emp_by_skill_orig <- emp_by_skill |> 
      left_join(orig_shares, by = "geo") |> 
      drop_na() |> 
      summarise(
        orig_n = weighted.mean(n, orig_sh, na.rm = TRUE), 
        orig_v = weighted.mean(v, orig_sh, na.rm = TRUE), 
        .by = c(sex, skill)
      )
    
    # Reference country
    emp_by_skill_iso <- emp_by_skill |> 
      filter(geo == iso)
    
    if (nrow(emp_by_skill_iso) > 0) {
      
      # Foreigners in reference country
      emp_by_skill_iso_for <- empsexoccupbirth |> 
        filter(birth == "Foreign", geo == iso) |> 
        filter(t == max(t)) |> 
        left_join(isco_occupations, by = c("occup" = "code")) |> 
        summarise(imm_n = sum(n), .by = c(sex, skill)) |> 
        mutate(imm_v = imm_n / sum(imm_n))
      
      t1 <- emp_by_skill_iso$t[1]
      output$range <- c(t1, t1) |> as.integer()
      
      data_iso <- emp_by_skill_iso |> 
        select(geo, t, sex, skill, iso_n = n, iso_v = v) |> 
        left_join(emp_by_skill_iso_for, by = c("sex", "skill")) |> 
        left_join(emp_by_skill_world, by = c("sex", "skill")) |> 
        left_join(emp_by_skill_orig, by = c("sex", "skill")) |> 
        pivot_longer(
          cols = -c(geo:skill), 
          names_to = c("var", ".value"),
          names_sep = "_"
        )
      
      output$data <- data_iso |> 
        mutate(
          sex = factor(sex, levels = c("Male", "Female")),
          var = factor(var)
        )
      
    } else {
      output$data <- emp_by_skill_iso
    }
    
  }
  
  return(output)
}


# Occupation by origin country --------------------------------------------

get_empcountry <- function(iso = NULL) {
  
  output <- list(data = NULL, range = NULL)
  
  emp_by_sectors <- empoccup |> 
    summarise(n = sum(n), .by = c(geo, t, sector)) |> 
    filter(t == max(t), .by = geo) |> 
    mutate(v = n / sum(n), .by = geo) |> 
    select(geo, t, sector, n, v)
  
  if (!is.null(iso)) {
    
    # Immigrant employment in reference country
    immemp_by_sectors <- empsector |> 
      filter(t == max(t), .by = geo) |> 
      filter(geo == iso, birth == "Foreign-born") |> 
      mutate(immig = n / sum(n)) |> 
      select(sector, immig)
    
    # Aggregate globally
    emp_by_sectors_world <- emp_by_sectors |> 
      summarise(n = sum(n), .by = sector) |> 
      mutate(world = n / sum(n)) |> 
      select(-n)
    
    # Aggregate by immigrant origin countries
    orig_shares <- gdidata::undesa_stocks_2020 |> 
      summarise(n = sum(n), .by = c(geo, t, from)) |> 
      filter(geo == iso, t == max(t)) |> 
      mutate(orig_sh = n / sum(n)) |> 
      select(geo = from, orig_sh)
    
    emp_by_sectors_orig <- emp_by_sectors |> 
      left_join(orig_shares, by = "geo") |> 
      drop_na() |> 
      summarise(orig = weighted.mean(v, orig_sh, na.rm = TRUE), .by = sector)
    
    # Reference country
    emp_by_sectors_iso <- emp_by_sectors |> 
      filter(geo == iso) |> 
      arrange(desc(n)) |> 
      slice_head(n = 6)
    
    if (nrow(emp_by_sectors_iso) > 0) {
      
      top_sectors <- emp_by_sectors_iso$sector
      
      t1 <- emp_by_sectors_iso$t[1]
      output$range <- c(t1, t1) |> as.integer()
      
      data_iso <- emp_by_sectors_iso |> 
        select(geo, t, sector, iso = v) |> 
        left_join(immemp_by_sectors, by = "sector") |> 
        left_join(emp_by_sectors_world, by = "sector") |> 
        left_join(emp_by_sectors_orig, by = "sector") |> 
        left_join(sectors, by = c("sector" = "code")) |> 
        pivot_longer(
          cols = c(iso, immig, orig, world), 
          names_to = "var", 
          values_to = "v"
        )
      
      output$data <- data_iso |> 
        mutate(
          sector = factor(sector, levels = top_sectors),
          var = factor(var)
        )
      
    } else {
      output$data <- emp_by_sectors_iso
    }
  }
  
  return(output)
}

