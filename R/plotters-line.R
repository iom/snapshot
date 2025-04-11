
plot_empty <- function(title, 
                       source, 
                       time, 
                       basesize, 
                       font, 
                       msg = "No data") {
  
  k <- function(factor = 1) factor * basesize / .pt

  plot <- ggplot(data.frame(t = time), aes(x = .data$t)) +
    labs(title = title, caption = source) +
    apply_theme("line", basesize = basesize, font = font) +
    theme(
      axis.text = element_blank(),
      panel.background = element_rect(color = NA, fill = pal("unblues", 6)),
      plot.caption = element_text(
        margin = margin(t = k(), r = k(), l = k())
      ),
    )
  
  plot <- ggdraw(plot) +
    draw_label(
      msg,
      y = .5, 
      fontfamily = font, 
      color = pal("blues", 3), 
      size = k(3)
    )
  
  return(plot)
}


# Net migration -----------------------------------------------------------

plot_nmig <- function(hero,
                      basesize,
                      font,
                      title = paste0(
                        "Net migration, ",
                        plot_data("nmig", hero)$range |>
                          paste(collapse = "\u2013")
                      )) {
  
  k <- function(factor = 1) factor * basesize / .pt
  source <- "Source: World Bank."
  t0 <- plot_data("nmig",)$range[1]
  t1 <- plot_data("nmig")$range[2]
  data <- plot_data("nmig", hero)$data
  
  if (nrow(data) > 0) {
    
    axis <- set_axis(data$n, "Persons")
    
    plot <- ggplot(data, aes(x = .data$t, y = .data$n)) +
      geom_bar(stat = "identity", width = .7, fill = pal("blues", 2)) +
      geom_hline(
        yintercept = 0,
        color = pal("blues"),
        linewidth = k(.1)
      ) +
      labs(title = title, caption = source) +
      
      scale_x_continuous(
        breaks = seq(t0, t1, 10),
        expand = expansion(mult = c(.03, .03)),
        guide = guide_axis(minor.ticks = TRUE)
      ) +
      scale_y_continuous(
        name = axis$title,
        breaks = axis$breaks,
        labels = axis$labels
      ) +
      guides(
        color = guide_legend(nrow = 2),
        linetype =  guide_legend(nrow = 2)
      ) +
      
      apply_theme("bar-vertical", basesize = basesize, font = font) +
      theme(
        axis.title.y = element_text(
          size = basesize,
          margin = margin(r = k(2))
        ),
        axis.ticks.x = element_line(
          color = pal("blues"),
          linewidth = k(.05)
        ),
      )
    
  } else {
    
    plot <- plot_empty(title, source, t0:t1, basesize, font)
  }
  
  return(plot)
}


# Remittances -------------------------------------------------------------

plot_remt <- function(hero,
                      basesize,
                      font,
                      title = paste0(
                        "Remittances, ",
                        plot_data("remt", hero)$range |>
                          paste(collapse = "\u2013")
                      )) {
  
  k <- function(factor = 1) factor * basesize / .pt
  source <- "Source: World Bank."
  t0 <- plot_data("remt", hero)$range[1]
  t1 <- plot_data("remt", hero)$range[2]
  
  data <- plot_data("remt", hero)$data
  
  vars <- c(
    "remin" = pal("blues", 2),
    "remout" = pal("reds", 2)
  )
  
  plot_elements <- list(
    geom_line(
      aes(x = .data$t, y = .data$n, color = .data$var, group = .data$var),
      linewidth = k(.3),
      na.rm = TRUE
    ),
    labs(title = title, caption = source),
    scale_x_continuous(
      minor_breaks = seq(t0, t1, 5),
      expand = expansion(mult = c(.025, .150)),
      guide = guide_axis(minor.ticks = TRUE)
    ),
    scale_color_manual(
      label = c("Remittance received", "Remittance paid"),
      values = vars
    ),
    scale_fill_manual(values = vars),
    coord_cartesian(clip = "off"),
    apply_theme("line", basesize = basesize, font = font),
    theme(
      axis.ticks.x = element_line(color = pal("blues"), linewidth = k(.05)),
    )
  )
  
  if (nrow(data) > 0) {
    
    axis <- set_axis(data$n, "USD")
    
    endpts <- data |>
      filter(.data$t %in% c(max(.data$t), max(.data$t) - 1), .by = .data$var) |>
      drop_na() |>
      mutate(pt = 1:n(), .by = .data$var) |>
      filter(max(.data$pt) == 1, .by = .data$var)
    
    df <- complete(data, t = t0:t1, var = c("remin", "remout"))
    
    lab <- df |>
      drop_na(.data$n) |>
      filter(.data$t == max(.data$t), .by = .data$var) |>
      mutate(lab_n = prettylabel(.data$n, signif = 3, currency = "$"))
    
    plot <- ggplot(df) +
      geom_point(
        aes(x = .data$t, y = .data$n, color = .data$var), endpts,
        size = .5,
        shape = 15,
        show.legend = FALSE
      ) +
      geom_segment(
        aes(x = .data$t, y = .data$n), lab,
        xend = max(t1) + 1,
        color = "black",
        linetype = "11",
        linewidth = k(.1)
      ) +
      geom_label(
        aes(y = .data$n, fill = .data$var, label = .data$lab_n), lab,
        x = max(t1) + 1,
        color = "white",
        size = k(.9),
        fontface = "bold",
        family = font,
        hjust = 0,
        vjust = .5,
        label.r = unit(.05, "lines"),
        label.size = .1,
        show.legend = FALSE,
      ) +
      scale_y_continuous(
        name = axis$title,
        breaks = axis$breaks,
        labels = axis$labels
      ) +
      plot_elements +
      theme(axis.title.y = element_text(
        size = basesize,
        margin = margin(r = k(2))
      ))
    
  } else {
    
    df <- expand.grid(t = t0:t1, var = c("remin", "remout"), n = 0)
    
    plot <- ggplot(df) +
      plot_elements +
      scale_y_continuous(
        limits = c(1, 10),
        breaks = 1,
        expand = expansion(mult = 0)
      ) +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_rect(color = NA, fill = pal("unblues", 6)),
      )
    
    plot <- ggdraw(plot) +
      draw_label(
        "No data",
        y = .55,
        fontfamily = font,
        color = pal("blues", 3),
        size = k(3)
      )
  }
  
  return(plot)
}


# FDI ---------------------------------------------------------------------

plot_fdi <- function(hero,
                     basesize,
                     font,
                     title = paste0(
                       "Foreign direct investment, ",
                       plot_data("fdi", hero)$range |>
                         paste(collapse = "\u2013")
                     )) {
  
  k <- function(factor = 1) factor * basesize / .pt
  source <- "Source: World Bank."
  t0 <- plot_data("fdi", hero)$range[1]
  t1 <- plot_data("fdi", hero)$range[2]
  
  data <- plot_data("fdi", hero)$data
  
  vars <- c(
    "fdiin" = pal("blues", 2),
    "fdiout" = pal("reds", 2)
  )
  
  plot_elements <- list(
    geom_line(
      aes(x = .data$t, y = .data$n, color = .data$var, group = .data$var),
      linewidth = k(.3),
      na.rm = TRUE
    ),
    labs(title = title, caption = source),
    scale_x_continuous(
      minor_breaks = seq(t0, t1, 5),
      expand = expansion(mult = c(.025, .150)),
      guide = guide_axis(minor.ticks = TRUE)
    ),
    scale_color_manual(
      label = c("FDI inflow", "FDI outflow"),
      values = vars
    ),
    scale_fill_manual(values = vars),
    coord_cartesian(clip = "off"),
    apply_theme("line", basesize = basesize, font = font),
    theme(
      axis.ticks.x = element_line(color = pal("blues"), linewidth = k(.05)),
    )
  )
  
  if (nrow(data) > 0) {
    
    axis <- set_axis(data$n, "USD")
    
    endpts <- data |>
      filter(.data$t %in% c(max(.data$t), max(.data$t) - 1), .by = .data$var) |>
      drop_na() |>
      mutate(pt = 1:n(), .by = .data$var) |>
      filter(max(.data$pt) == 1, .by = .data$var)
    
    df <- complete(data, t = t0:t1, var = c("fdiin", "fdiout"))
    
    lab <- df |>
      drop_na(.data$n) |>
      filter(.data$t == max(.data$t), .by = .data$var) |>
      mutate(lab_n = prettylabel(.data$n, signif = 3, currency = "$"))
    
    plot <- ggplot(df) +
      geom_point(
        aes(x = .data$t, y = .data$n, color = .data$var), endpts,
        size = .5,
        shape = 15,
        show.legend = FALSE
      ) +
      geom_segment(
        aes(x = .data$t, y = .data$n), lab,
        xend = max(t1) + 1,
        color = "black",
        linetype = "11",
        linewidth = k(.1)
      ) +
      geom_label(
        aes(y = .data$n, fill = .data$var, label = .data$lab_n), lab,
        x = max(t1) + 1,
        color = "white",
        size = k(.9),
        fontface = "bold",
        family = font,
        hjust = 0,
        vjust = .5,
        label.r = unit(.05, "lines"),
        label.size = .1,
        show.legend = FALSE,
      ) +
      scale_y_continuous(
        name = axis$title,
        breaks = axis$breaks,
        labels = axis$labels
      ) +
      plot_elements +
      theme(axis.title.y = element_text(
        size = basesize,
        margin = margin(r = k(2))
      ))
    
  } else {
    
    df <- expand.grid(t = t0:t1, var = c("fdiin", "fdiout"), n = 0)
    
    plot <- ggplot(df) +
      plot_elements +
      scale_y_continuous(
        limits = c(1, 10),
        breaks = 1,
        expand = expansion(mult = 0)
      ) +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_rect(color = NA, fill = pal("unblues", 6)),
      )
    
    plot <- ggdraw(plot) +
      draw_label(
        "No data",
        y = .55,
        fontfamily = font,
        color = pal("blues", 3),
        size = k(3)
      )
  }
  
  return(plot)
}


# Population --------------------------------------------------------------

plot_pop <- function(hero,
                     basesize,
                     font,
                     title = paste0(
                       "Population,\n",
                       plot_data("pop", hero)$range |>
                         paste(collapse = "\u2013")
                     )) {
  
  k <- function(factor = 1) factor * basesize / .pt
  source <- "Source: UN DESA."
  name <- namer(hero)
  
  data <- plot_data("pop", hero)$data
  t0 <- plot_data("pop", hero)$range[1]
  t1 <- plot_data("pop", hero)$range[2]
  
  if (nrow(data) > 0) {
    
    axis <- set_axis(data$n, "Persons")
    
    endpts <- data |>
      filter(.data$t %in% c(max(.data$t), max(.data$t) - 1), .by = .data$var) |>
      drop_na() |>
      mutate(pt = 1:n(), .by = .data$var) |>
      filter(max(.data$pt) == 1, .by = .data$var)
    
    df <- complete(
      data, 
      t = t0:t1, 
      var = c(countryname(hero, to = "name_text"), "World median")
    )
    
    lab <- df |>
      drop_na(.data$n) |>
      filter(.data$var != "World median") |>
      filter(.data$t == max(.data$t)) |>
      mutate(lab_n = prettylabel(.data$n))

    plot <- ggplot(
      df, 
      aes(
        x = .data$t, 
        y = .data$n, 
        color = .data$var, 
        linetype = .data$var, 
        group = .data$var
      )
    ) + 
      geom_line(linewidth = k(.25), na.rm = TRUE) + 
      geom_segment(
        aes(x = .data$t, y = .data$n), lab,
        xend = t1,
        color = "black",
        linetype = "11",
        linewidth = k(.1)
      ) +
      geom_label(
        aes(y = .data$n, fill = .data$var, label = .data$lab_n), lab, 
        x = t1,
        color = "white", 
        fill = pal("blues", 2),
        size = k(.9), 
        fontface = "bold", 
        family = font,
        hjust = 0, 
        vjust = .5, 
        label.r = unit(.05, "lines"), 
        label.size = .1,
        show.legend = FALSE,
      ) +
      labs(title = title, caption = source) +
      
      scale_x_continuous(
        limits = c(t0, t1),
        breaks = c(1950, 1990, 2020),
        expand = expansion(mult = c(.025, .125)),
      ) +
      scale_y_continuous(
        name = axis$title,
        breaks = axis$breaks,
        labels = axis$labels,
      ) +
      scale_color_manual(
        values = c(pal("blues", 2), "black"),
        guide = guide_legend(nrow = 2)
      ) +
      coord_cartesian(clip = "off") +
      
      apply_theme("line", basesize = basesize, font = font) +
      theme(
        axis.title.y = element_text(
          size = basesize,
          margin = margin(r = k(2))
        ),
        axis.ticks.x = element_line(color = pal("blues"), linewidth = k(.05)),
      )
    
  } else {
    
    plot <- plot_empty(title, source, t0:t1, basesize, font)
  }
  
  return(plot)
}


# Birth rate --------------------------------------------------------------

plot_birth <- function(hero,
                       basesize,
                       font,
                       title = paste0(
                         "Birth rate,\n",
                         plot_data("birth", hero)$range |>
                           paste(collapse = "\u2013")
                       )) {
  
  k <- function(factor = 1) factor * basesize / .pt
  source <- "Source: World Bank."
  name <- namer(hero)
  
  data <- plot_data("birth", hero)$data
  t0 <- plot_data("birth", hero)$range[1]
  t1 <- plot_data("birth", hero)$range[2]
  
  if (nrow(data) > 0) {
    
    endpts <- data |>
      filter(.data$t %in% c(max(.data$t), max(.data$t) - 1), .by = .data$var) |>
      drop_na() |>
      mutate(pt = 1:n(), .by = .data$var) |>
      filter(max(.data$pt) == 1, .by = .data$var)
    
    df <- complete(
      data, 
      t = t0:t1, 
      var = c(countryname(hero, to = "name_text"), "World median")
    )
    
    lab <- df |>
      drop_na(.data$n) |>
      filter(.data$var != "World median") |>
      filter(.data$t == max(.data$t)) |>
      mutate(lab_n = prettylabel(.data$n))
    
    plot <- ggplot(
      df, 
      aes(
        x = .data$t, 
        y = .data$n, 
        color = .data$var, 
        linetype = .data$var, 
        group = .data$var
      )
    ) + 
      geom_line(linewidth = k(.25), na.rm = TRUE) + 
      geom_segment(
        aes(x = .data$t, y = .data$n), lab,
        xend = t1,
        color = "black",
        linetype = "11",
        linewidth = k(.1)
      ) +
      geom_label(
        aes(y = .data$n, fill = .data$var, label = .data$lab_n), lab, 
        x = t1,
        color = "white", 
        fill = pal("blues", 2),
        size = k(.9), 
        fontface = "bold", 
        family = font,
        hjust = 0, 
        vjust = .5, 
        label.r = unit(.05, "lines"), 
        label.size = .1,
        show.legend = FALSE,
      ) +
      labs(title = title, caption = source) +
      
      scale_x_continuous(
        limits = c(t0, t1),
        breaks = seq(t0, t1, 10),
        minor_breaks = seq(t0, t1, 5),
        expand = expansion(mult = c(.025, .125)),
        guide = guide_axis(minor.ticks = TRUE)
      ) +
      scale_y_continuous(name = "Births per 1000 population") +
      scale_color_manual(
        values = c(pal("blues", 2), "black"),
        guide = guide_legend(nrow = 2)
      ) +
      coord_cartesian(clip = "off") +
      
      apply_theme("line", basesize = basesize, font = font) +
      theme(
        axis.title.y = element_text(
          size = basesize,
          margin = margin(r = k(2))
        ),
        axis.ticks.x = element_line(color = pal("blues"), linewidth = k(.05)),
      )
    
  } else {
    
    plot <- plot_empty(title, source, t0:t1, basesize, font)
  }
  
  return(plot)
}


# Dependency ratio --------------------------------------------------------

plot_depend <- function(hero,
                        basesize,
                        font,
                        title = paste0(
                          "Age dependency ratio,\n",
                          plot_data("depend", hero)$range |>
                            paste(collapse = "\u2013")
                        )) {
  
  k <- function(factor = 1) factor * basesize / .pt
  source <- "Source: World Bank."
  name <- namer(hero)
  
  data <- plot_data("depend", hero)$data
  t0 <- plot_data("depend", hero)$range[1]
  t1 <- plot_data("depend", hero)$range[2]
  
  if (nrow(data) > 0) {
    
    endpts <- data |>
      filter(.data$t %in% c(max(.data$t), max(.data$t) - 1), .by = .data$var) |>
      drop_na() |>
      mutate(pt = 1:n(), .by = .data$var) |>
      filter(max(.data$pt) == 1, .by = .data$var)
    
    df <- complete(
      data, 
      t = t0:t1, 
      var = c(countryname(hero, to = "name_text"), "World median")
    )
    
    lab <- df |>
      drop_na(.data$n) |>
      filter(.data$var != "World median") |>
      filter(.data$t == max(.data$t)) |>
      mutate(lab_n = prettylabel(.data$n))
    
    plot <- ggplot(
      df, 
      aes(
        x = .data$t, 
        y = .data$n, 
        color = .data$var, 
        linetype = .data$var, 
        group = .data$var
      )
    ) + 
      geom_line(linewidth = k(.25), na.rm = TRUE) + 
      geom_segment(
        aes(x = .data$t, y = .data$n), lab,
        xend = t1,
        color = "black",
        linetype = "11",
        linewidth = k(.1)
      ) +
      geom_label(
        aes(y = .data$n, fill = .data$var, label = .data$lab_n), lab, 
        x = t1,
        color = "white", 
        fill = pal("blues", 2),
        size = k(.9), 
        fontface = "bold", 
        family = font,
        hjust = 0, 
        vjust = .5, 
        label.r = unit(.05, "lines"), 
        label.size = .1,
        show.legend = FALSE,
      ) +
      labs(title = title, caption = source) +
      
      scale_x_continuous(
        limits = c(t0, t1),
        breaks = seq(t0, t1, 10),
        minor_breaks = seq(t0, t1, 5),
        expand = expansion(mult = c(.025, .125)),
        guide = guide_axis(minor.ticks = TRUE)
      ) +
      scale_y_continuous(name = "Dependents per 100 working age") +
      scale_color_manual(
        values = c(pal("blues", 2), "black"),
        guide = guide_legend(nrow = 2)
      ) +
      coord_cartesian(clip = "off") +
      
      apply_theme("line", basesize = basesize, font = font) +
      theme(
        axis.title.y = element_text(
          size = basesize,
          margin = margin(r = k(2))
        ),
        axis.ticks.x = element_line(color = pal("blues"), linewidth = k(.05)),
      )
    
  } else {
    
    plot <- plot_empty(title, source, t0:t1, basesize, font)
  }
  
  return(plot)
}


# Income ------------------------------------------------------------------

plot_income <- function(hero,
                        basesize,
                        font,
                        title = paste0(
                          "GDP per capita,\n",
                          plot_data("income", hero)$range |>
                            paste(collapse = "\u2013")
                        )) {
  
  k <- function(factor = 1) factor * basesize / .pt
  source <- "Source: World Bank."
  name <- namer(hero)
  
  data <- plot_data("income", hero)$data
  t0 <- plot_data("income", hero)$range[1]
  t1 <- plot_data("income", hero)$range[2]
  
  if (nrow(data) > 0) {
    
    axis <- set_axis(data$n, "USD")
    
    endpts <- data |>
      filter(.data$t %in% c(max(.data$t), max(.data$t) - 1), .by = .data$var) |>
      drop_na() |>
      mutate(pt = 1:n(), .by = .data$var) |>
      filter(max(.data$pt) == 1, .by = .data$var)
    
    df <- complete(
      data, 
      t = t0:t1, 
      var = c(countryname(hero, to = "name_text"), "World median")
    )
    
    lab <- df |>
      drop_na(.data$n) |>
      filter(.data$var != "World median") |>
      filter(.data$t == max(.data$t)) |>
      mutate(lab_n = prettylabel(.data$n, currency = "$"))
    
    plot <- ggplot(
      df, 
      aes(
        x = .data$t, 
        y = .data$n, 
        color = .data$var, 
        linetype = .data$var, 
        group = .data$var
      )
    ) + 
      geom_line(linewidth = k(.25), na.rm = TRUE) + 
      geom_segment(
        aes(x = .data$t, y = .data$n), lab,
        xend = t1,
        color = "black",
        linetype = "11",
        linewidth = k(.1)
      ) +
      geom_label(
        aes(y = .data$n, fill = .data$var, label = .data$lab_n), lab, 
        x = t1,
        color = "white", 
        fill = pal("blues", 2),
        size = k(.9), 
        fontface = "bold", 
        family = font,
        hjust = 0, 
        vjust = .5, 
        label.r = unit(.05, "lines"), 
        label.size = .1,
        show.legend = FALSE,
      ) +
      labs(title = title, caption = source) +
      
      scale_x_continuous(
        limits = c(t0, t1),
        breaks = seq(t0, t1, 10),
        minor_breaks = seq(t0, t1, 5),
        expand = expansion(mult = c(.025, .125)),
        guide = guide_axis(minor.ticks = TRUE)
      ) +
      scale_y_continuous(
        name = axis$title,
        breaks = axis$breaks,
        labels = axis$labels,
      ) +
      scale_color_manual(
        values = c(pal("blues", 2), "black"),
        guide = guide_legend(nrow = 2)
      ) +
      coord_cartesian(clip = "off") +
      
      apply_theme("line", basesize = basesize, font = font) +
      theme(
        axis.title.y = element_text(
          size = basesize,
          margin = margin(r = k(2))
        ),
        axis.ticks.x = element_line(color = pal("blues"), linewidth = k(.05)),
      )
    
  } else {
    
    plot <- plot_empty(title, source, t0:t1, basesize, font)
  }
  
  return(plot)
}


# Inflation ---------------------------------------------------------------

plot_inf <- function(hero,
                     basesize,
                     font,
                     title = paste0(
                       "Inflation rate,\n",
                       plot_data("inf", hero)$range |>
                         paste(collapse = "\u2013")
                     )) {
  
  k <- function(factor = 1) factor * basesize / .pt
  source <- "Source: World Bank."
  name <- namer(hero)
  
  data <- plot_data("inf", hero)$data
  t0 <- plot_data("inf", hero)$range[1]
  t1 <- plot_data("inf", hero)$range[2]
  
  if (nrow(data) > 0) {
    
    endpts <- data |>
      filter(.data$t %in% c(max(.data$t), max(.data$t) - 1), .by = .data$var) |>
      drop_na() |>
      mutate(pt = 1:n(), .by = .data$var) |>
      filter(max(.data$pt) == 1, .by = .data$var)
    
    df <- complete(
      data, 
      t = t0:t1, 
      var = c(countryname(hero, to = "name_text"), "World median")
    )
    
    lab <- df |>
      drop_na(.data$n) |>
      filter(.data$var != "World median") |>
      filter(.data$t == max(.data$t)) |>
      mutate(lab_n = prettylabel(.data$n, pct = TRUE, signif = 1))
    
    plot <- ggplot(
      df, 
      aes(
        x = .data$t, 
        y = .data$n, 
        color = .data$var, 
        linetype = .data$var, 
        group = .data$var
      )
    ) + 
      geom_line(linewidth = k(.25), na.rm = TRUE) + 
      geom_segment(
        aes(x = .data$t, y = .data$n), lab,
        xend = t1,
        color = "black",
        linetype = "11",
        linewidth = k(.1)
      ) +
      geom_label(
        aes(y = .data$n, fill = .data$var, label = .data$lab_n), lab, 
        x = t1,
        color = "white", 
        fill = pal("blues", 2),
        size = k(.9), 
        fontface = "bold", 
        family = font,
        hjust = 0, 
        vjust = .5, 
        label.r = unit(.05, "lines"), 
        label.size = .1,
        show.legend = FALSE,
      ) +
      labs(title = title, caption = source) +
      
      scale_x_continuous(
        limits = c(t0, t1),
        breaks = seq(t0, t1, 10),
        minor_breaks = seq(t0, t1, 5),
        expand = expansion(mult = c(.025, .125)),
        guide = guide_axis(minor.ticks = TRUE)
      ) +
      scale_y_continuous(name = "Per cent") +
      scale_color_manual(
        values = c(pal("blues", 2), "black"),
        guide = guide_legend(nrow = 2)
      ) +
      coord_cartesian(clip = "off") +
      
      apply_theme("line", basesize = basesize, font = font) +
      theme(
        axis.title.y = element_text(
          size = basesize,
          margin = margin(r = k(2))
        ),
        axis.ticks.x = element_line(color = pal("blues"), linewidth = k(.05)),
      )
    
  } else {
    
    plot <- plot_empty(title, source, t0:t1, basesize, font)
  }
  
  return(plot)
}


# Unemployment rate -------------------------------------------------------

plot_unem <- function(hero,
                      basesize,
                      font,
                      title = paste0(
                        "Unemployment rate,\n",
                        plot_data("unem", hero)$range |>
                          paste(collapse = "\u2013")
                      )) {
  
  k <- function(factor = 1) factor * basesize / .pt
  source <- "Source: ILO."
  name <- namer(hero)
  
  data <- plot_data("unem", hero)$data
  t0 <- plot_data("unem", hero)$range[1]
  t1 <- plot_data("unem", hero)$range[2]
  
  if (nrow(data) > 0) {
    
    endpts <- data |>
      filter(.data$t %in% c(max(.data$t), max(.data$t) - 1), .by = .data$var) |>
      drop_na() |>
      mutate(pt = 1:n(), .by = .data$var) |>
      filter(max(.data$pt) == 1, .by = .data$var)
    
    df <- complete(
      data, 
      t = t0:t1, 
      var = c(countryname(hero, to = "name_text"), "World median")
    )
    
    lab <- df |>
      drop_na(.data$n) |>
      filter(.data$var != "World median") |>
      filter(.data$t == max(.data$t)) |>
      mutate(lab_n = prettylabel(.data$n, pct = TRUE, signif = 1))
    
    plot <- ggplot(
      df, 
      aes(
        x = .data$t, 
        y = .data$n, 
        color = .data$var, 
        linetype = .data$var, 
        group = .data$var
      )
    ) + 
      geom_line(linewidth = k(.25), na.rm = TRUE) + 
      geom_segment(
        aes(x = .data$t, y = .data$n), lab,
        xend = t1,
        color = "black",
        linetype = "11",
        linewidth = k(.1)
      ) +
      geom_label(
        aes(y = .data$n, fill = .data$var, label = .data$lab_n), lab, 
        x = t1,
        color = "white", 
        fill = pal("blues", 2),
        size = k(.9), 
        fontface = "bold", 
        family = font,
        hjust = 0, 
        vjust = .5, 
        label.r = unit(.05, "lines"), 
        label.size = .1,
        show.legend = FALSE,
      ) +
      labs(title = title, caption = source) +
      
      scale_x_continuous(
        limits = c(t0, t1),
        breaks = seq(t0, t1, 10),
        minor_breaks = seq(t0, t1, 5),
        expand = expansion(mult = c(.025, .125)),
        guide = guide_axis(minor.ticks = TRUE)
      ) +
      scale_y_continuous(name = "Per cent") +
      scale_color_manual(
        values = c(pal("blues", 2), "black"),
        guide = guide_legend(nrow = 2)
      ) +
      coord_cartesian(clip = "off") +
      
      apply_theme("line", basesize = basesize, font = font) +
      theme(
        axis.title.y = element_text(
          size = basesize,
          margin = margin(r = k(2))
        ),
        axis.ticks.x = element_line(color = pal("blues"), linewidth = k(.05)),
      )
    
  } else {
    
    plot <- plot_empty(title, source, t0:t1, basesize, font)
  }
  
  return(plot)
}






# Immigrant dependency ratio ----------------------------------------------

plot_immdep <- function(hero,
                        basesize,
                        font,
                        title = paste0(
                          "Age dependency ratio, ",
                          snap_data("immdep", hero)$range |>
                            paste(collapse = "\u2013")
                        )) {
  
  k <- function(factor = 1) factor * basesize / .pt
  source <- "Source: UN DESA; World Bank; IOM Global Data Institute."
  
  data <- snap_data("immdep", hero)$data
  
  timespan <- unique(migdemog$t)
  
  if (nrow(data) > 0) {
    
    t0 <- min(timespan)
    t1 <- max(timespan)
    
    endpts <- filter(data, t %in% c(max(t), max(t) - 1), .by = var) |> 
      drop_na() |> 
      mutate(pt = 1:n(), .by = var) |> 
      filter(max(pt) == 1, .by = var)
    
    df <- complete(data, t = timespan, var = c("gen", "immig"))
    
    lab <- data |> 
      filter(t == max(t)) |> 
      mutate(lab = round(v, digits = 0))
    
    plot <- ggplot(df, aes(x = t, y = v, color = var, group = var)) +
      geom_line(linewidth = k(.35), na.rm = TRUE) + 
      geom_point(
        aes(x = t, y = v, color = var), endpts,
        size = .5, shape = 15, show.legend = FALSE
      ) + 
      
      # Annotations
      geom_segment(
        aes(x = t, y = v), lab, 
        xend = t1 + 1,
        color = "black", 
        linetype = "11", 
        linewidth = k(.1)
      ) +
      geom_label(
        aes(y = v, fill = var, label = lab), lab, 
        x = t1 + 1,
        color = "white", 
        size = k(.9), 
        fontface = "bold", 
        family = font,
        hjust = 0, 
        vjust = .5, 
        label.r = unit(.05, "lines"), 
        label.size = .1,
        show.legend = FALSE,
      ) +
      labs(title = title, caption = source) +
      
      scale_x_continuous(
        expand = expansion(mult = c(.03, .05)),
        guide = guide_axis(minor.ticks = TRUE)
      ) +
      scale_y_continuous(name = "Dependents per\n100 working age persons") +
      scale_fill_manual(values = c(
        "gen" = pal("blues", 2), 
        "immig" = pal("greens")
      )) +
      scale_color_manual(
        label = c("General population", "Immigrant population"),
        values = c(
          "gen" = pal("blues", 2), 
          "immig" = pal("greens")
        )
      ) +
      coord_cartesian(clip = "off") +
      
      # Aesthetics
      apply_theme(type = "bar-vertical", basesize = basesize, font = font) +
      theme(
        axis.title.y = element_text(
          size = basesize,
          margin = margin(r = k(2))
        ),
        axis.ticks.x = element_line(color = pal("blues"), linewidth = k(.05)),
        plot.margin = margin(k(4), k(4), k(.25), k(4))
      )
    
  } else {
    plot <- plot_empty(title, source, timespan, basesize, font)
  }
  
  return(plot)
}




