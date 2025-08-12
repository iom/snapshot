
ranger <- function(data) c(min(data$t), max(data$t))

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
    
    others <- data_filter[!(data_filter$from %in% selected), ] |> 
      summarise(n = sum(n), .by = c(geo, t)) |> 
      mutate(from = "Others")
    
  } else if (others_count == 1) {
    
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





