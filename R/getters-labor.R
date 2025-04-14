
get_emp <- function(iso = NULL) {
  
  output <- list(data = NULL, range = NULL)
  
  t0 <- 2000
  t1 <- max(emp$t)
  
  order <- c("Native-born", "Foreign-born", "Unknown")
  
  dataset <- emp |> 
    summarise(n = sum(n), .by = c(geo, birth, t)) |> 
    complete(geo, birth, t = t0:t1) |> 
    mutate(birth = factor(birth, levels = order))
  
  if (is.null(iso)) {
    
    output$data <- dataset
    output$range <- c(t0, t1) |> as.integer()
    
  } else {
    
    data_iso <- filter(dataset, geo == iso)
    
    if (nrow(data_iso) > 0) {
      
      output$data <- data_iso
      
      t_iso <- drop_na(data_iso)$t
      t0_iso <- max(2000, min(t_iso))
      t1_iso <- max(t_iso)
      output$range <- c(t0_iso, t1_iso) |> as.integer()
    }
  }
  
  return(output)
}

get_empeduc <- function(iso = NULL) {
  
  output <- list(data = NULL, range = NULL)
  
  t1 <- max(emp$t)
  
  order_birth <- c(
    "Native-born", 
    "Foreign-born"
  )
  
  order_sex = c(
    "Male",
    "Female"
  )
  
  order_educ <- c(
    "Less than basic", 
    "Basic", 
    "Intermediate", 
    "Advanced", 
    "Unknown"
  )
  
  dataset <- emp |> 
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
    
    if (nrow(data_iso) > 0) {
      output$data <- data_iso
      output$range <- c(data_iso$t[1], data_iso$t[1]) |> as.integer()
    }
  }
  
  return(output)
}
