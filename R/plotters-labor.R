

# Employment by place of birth --------------------------------------------

plot_emp <- function(hero,
                     basesize,
                     font,
                     title = paste0(
                       "Employment, ",
                       snap_data("emp")$range |>
                         paste(collapse = "\u2013")
                     )) {
  
  k <- function(factor = 1) factor * basesize / .pt
  source <- "Source: ILO."
  t0 <- snap_data("emp")$range[1]
  t1 <- snap_data("emp")$range[2]
  data_iso <- snap_data("emp", hero)$data
  
  if (nrow(data_iso) > 0) {
    
    df <- data_iso |> 
      mutate(
        pt = ifelse(is.na(lag(n)) & is.na(lead(n)), TRUE, FALSE),
        .by = birth
      )
    
    df_pt <- filter(df, pt)
    
    axis <- set_axis(df$n)
    
    lab <- df |> 
      filter(!is.na(n)) |> 
      filter(t == max(t), .by = birth) |> 
      mutate(lab = prettylabel(n))
    
    fills <- c(
      "Native-born"  = pal("blues", 2), 
      "Foreign-born" = pal("greens")
    )
    
    plot <- ggplot(df, aes(x = t, y = n, group = birth, color = birth)) + 
      geom_hline(yintercept = 0, linewidth = k(.05), color = pal("blues")) +
      geom_point(data = df_pt, size = .75, shape = 15, show.legend = FALSE) +
      geom_line(linewidth = k(.35), na.rm = TRUE) + 
      
      # Annotation
      geom_segment(
        data = lab,
        xend = t1 + .25,
        linetype = "11",
        linewidth = k(.1),
        color = "black",
      ) +
      geom_label(
        aes(y = n, fill = birth, label = lab), 
        lab,
        x = t1 + .25,
        color = "white",
        family = font,
        fontface = "bold", 
        hjust = 0, 
        size = k(.9), 
        vjust = .5, 
        label.r = unit(.05, "lines"), 
        label.size = .1,
        show.legend = FALSE,
      ) +
      labs(title = title, caption = source) +
      
      # Scales
      scale_x_continuous(
        breaks = seq(t0, t1, 5),
        expand = expansion(mult = c(.025, .100))
      ) +
      scale_y_continuous(
        name = axis$title,
        breaks = axis$breaks,
        labels = axis$labels,
        expand = expansion(mult = c(0, .025))
      ) + 
      scale_color_manual(values = fills) +
      scale_fill_manual(values = fills) +
      coord_cartesian(clip = "off") + 
      
      # Theme
      apply_theme("line", basesize = basesize, font = font) + 
      theme(
        axis.title.y = element_text(
          size = basesize, 
          margin = margin(r = k(2))
        ),
        axis.ticks.x = element_line(color = pal("blues"), linewidth = k(.05)),
        axis.minor.ticks.x.bottom = element_blank(),
        legend.key.height = unit(k(.1), "lines"),
        legend.box.spacing = unit(k(.25), "lines"),
      )
    
  } else {
    
    plot <- plot_empty(title, source, basesize, font)
  }
  
  return(plot)
}


# Employment by place of birth and education level ------------------------

plot_empeduc <- function(hero,
                         basesize,
                         font,
                         title = paste0(
                           "Disaggregated employment, ",
                           snap_data("empeduc", hero)$range[2]
                         )) {
  
  k <- function(factor = 1) factor * basesize / .pt
  source <- "Source: ILO."
  t1 <- snap_data("empeduc", hero)$range[2]
  data_iso <- snap_data("empeduc", hero)$data
  
  if (nrow(data_iso) > 0) {
    
    birth_order <- levels(snap_data("empeduc")$data$birth)
    sex_order <- levels(snap_data("empeduc")$data$sex)
    educ_order <- levels(snap_data("empeduc")$data$educ)
    group_order <- paste(
      rep(birth_order, each = length(sex_order)), 
      tolower(sex_order)
    )
    
    dims <- get_dims(
      ticks = length(educ_order), 
      inwidth = .1, 
      outwidth = .1
    )
    
    mid <- filter(dims, xtick)$mid
    
    df <- data_iso |> 
      complete(
        geo, 
        sex = sex_order,
        educ = educ_order,
        birth = birth_order
      ) |> 
      arrange(
        match(educ, educ_order), 
        match(birth, birth_order),
        match(sex, rev(sex_order)), 
      ) |> 
      mutate(
        xmin = rep(dims$xmin, each = 2),
        xmax = rep(dims$xmax, each = 2),
        ymin = case_when(sex == "Male" ~ lag(v), .default = 0),
        ymax = case_when(sex == "Male" ~ lag(v) + v, .default = v),
        group = paste0(birth, " ", tolower(sex)),
        group = factor(group, levels = group_order),
      )
    
    labels <- drop_na(df) |> 
      summarise(v = sum(v), .by = c(xmin, xmax, birth, educ)) |> 
      mutate(
        lab = prettylabel(100 * v),
        ymax = v,
        xpos = (xmin + xmax) / 2 
      ) |> 
      select(xpos, ypos = ymax, lab)
    
    plot <- ggplot() + 
      geom_rect(
        aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = group),
        df
      ) +
      labs(title = title, caption = source) +
      
      scale_x_continuous(
        limits = c(0, 1),
        breaks = mid,
        labels = c("Less than\nbasic", educ_order[-1]),
        expand = expansion(mult = .02)
      ) +
      scale_y_continuous(
        name = "Per cent",
        labels = function(x) 100 * x,
        expand = expansion(mult = c(0, .05))
      ) + 
      scale_fill_manual(values = c(
        pal("blues", 2), 
        pal("unblues", 2),
        pal("greens"), 
        pal("greens", 3)
      )) +
      guides(fill = guide_legend(nrow = 2)) +
      
      apply_theme("bar-vertical", basesize = basesize, font = font) + 
      theme(
        axis.title.y = element_text(
          size = basesize,
          margin = margin(r = k(2))
        ),
        axis.line.x = element_line(linewidth = k(.05), color = pal("blues")),
        legend.key.height = unit(k(.1), "lines"),
        legend.key.spacing.y = unit(k(.5), "points"),
        legend.position = "bottom",
        legend.box.spacing = unit(k(.2), "lines"),
      )
    
    plot <- plot + 
      geom_text(
        aes(x = xpos, y = ypos, label = lab),
        labels, 
        color = pal("blues"),
        family = font,
        size = k(.9),
        vjust = 0,
        nudge_y = layer_scales(plot)$y$range$range[2] / 40,
        show.legend = FALSE
      )

  } else {
    
    plot <- plot_empty(title, source, basesize, font)
  }
  
  return(plot)
}


# Unemployment rate by place of birth -------------------------------------

plot_unemrate <- function(hero,
                          basesize,
                          font,
                          title = paste0(
                            "Unemployment rate, ",
                            snap_data("unemrate")$range |>
                              paste(collapse = "\u2013")
                          )) {
  
  k <- function(factor = 1) factor * basesize / .pt
  source <- "Source: ILO."
  t0 <- snap_data("unemrate")$range[1]
  t1 <- snap_data("unemrate")$range[2]
  data_iso <- snap_data("unemrate", hero)$data
  
  if (nrow(data_iso) > 0) {
    
    df <- data_iso |> 
      mutate(
        pt = ifelse(is.na(lag(v)) & is.na(lead(v)), TRUE, FALSE),
        .by = birth
      )
    
    df_pt <- filter(df, pt)
    
    labels <- df |> 
      filter(!is.na(v)) |> 
      filter(t == max(t), .by = birth) |> 
      mutate(lab = prettylabel(v))
    
    fills <- c(
      "Native-born"  = pal("blues", 2), 
      "Foreign-born" = pal("greens")
    )
    
    plot <- ggplot(df, aes(x = t, y = v, group = birth, color = birth)) + 
      geom_hline(yintercept = 0, linewidth = k(.05), color = pal("blues")) +
      geom_point(data = df_pt, size = .75, shape = 15, show.legend = FALSE) +
      geom_line(linewidth = k(.35), na.rm = TRUE) + 
      
      # Annotation
      geom_segment(
        data = labels,
        xend = t1 + .25,
        linetype = "11",
        linewidth = k(.1),
        color = "black",
      ) +
      geom_label(
        aes(y = v, fill = birth, label = lab), 
        labels,
        x = t1 + .25,
        color = "white",
        size = k(.9), fontface = "bold", family = font,
        hjust = 0, vjust = .5, 
        label.r = unit(.05, "lines"), label.size = .1,
        show.legend = FALSE,
      ) +
      labs(title = title, caption = source) +
      
      # Scales
      scale_x_continuous(
        breaks = seq(t0, t1, 5),
        expand = expansion(mult = c(.025, .100))
      ) +
      scale_y_continuous(
        name = "Per cent",
        labels = function(x) 100 * x,
        expand = expansion(mult = c(0, .025))
      ) + 
      scale_color_manual(values = fills) +
      scale_fill_manual(values = fills) +
      coord_cartesian(clip = "off") + 
      
      # Theme
      apply_theme("bar-vertical", basesize = basesize, font = font) + 
      theme(
        axis.title.y = element_text(
          size = basesize, 
          margin = margin(r = k(2))
        ),
        axis.ticks.x = element_line(color = pal("blues"), linewidth = k(.05)),
        axis.minor.ticks.x.bottom = element_blank(),
        legend.key.height = unit(k(.1), "lines"),
        legend.box.spacing = unit(k(.25), "lines"),
      )
    
  } else {
    
    plot <- plot_empty(title, source, basesize, font)
  }
  
  return(plot)
}


# Unemployment rate by education ------------------------------------------

plot_unemeduc <- function(hero,
                          basesize,
                          font,
                          title = paste0(
                            "Disaggregated unemployment rate, ",
                            snap_data("unemeduc", hero)$range[2]
                          )) {
  
  k <- function(factor = 1) factor * basesize / .pt
  source <- "Source: ILO."
  t1 <- snap_data("unemeduc", hero)$range[2]
  data_iso <- snap_data("unemeduc", hero)$data |> 
    filter(sex != "Total", educ != "Total")
  
  if (nrow(data_iso) > 0) {
    
    birth_order <- snap_data("unemeduc")$data$birth |> levels()
    
    sex_order <- levels(snap_data("unemeduc")$data$sex)
    sex_order <- sex_order[sex_order != "Total"]
    
    educ_order <- levels(snap_data("unemeduc")$data$educ)
    educ_order <- educ_order[educ_order != "Total"]
    
    group_order <- paste(
      rep(birth_order, each = length(sex_order)), 
      tolower(sex_order)
    )
    
    dims <- get_dims(
      ticks = length(educ_order), 
      inwidth = .1, 
      outwidth = .1
    )
    
    mid <- filter(dims, xtick)$mid
    
    df <- data_iso |> 
      complete(
        geo, 
        sex = sex_order,
        educ = educ_order,
        birth = birth_order,
        fill = list(v = 0)
      ) |> 
      arrange(
        match(educ, educ_order), 
        match(birth, birth_order),
        match(sex, rev(sex_order)), 
      ) |> 
      mutate(
        xmin = rep(dims$xmin, each = 2),
        xmax = rep(dims$xmax, each = 2),
        ymin = case_when(sex == "Male" ~ lag(v), .default = 0),
        ymax = case_when(sex == "Male" ~ lag(v) + v, .default = v),
        group = paste0(birth, " ", tolower(sex)),
        group = factor(group, levels = group_order),
      )
    
    labels <- drop_na(df) |> 
      summarise(v = sum(v), .by = c(xmin, xmax, birth, educ)) |> 
      mutate(
        lab = prettylabel(v),
        ymax = v,
        xpos = (xmin + xmax) / 2 
      ) |> 
      select(xpos, ypos = ymax, lab)
    
    plot <- ggplot() + 
      geom_rect(
        aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = group),
        df
      ) +
      labs(title = title, caption = source) +
      
      scale_x_continuous(
        limits = c(0, 1),
        breaks = mid,
        labels = c("Less than\nbasic", educ_order[-1]),
        expand = expansion(mult = .02)
      ) +
      scale_y_continuous(
        name = "Per cent",
        expand = expansion(mult = c(0, .05))
      ) + 
      scale_fill_manual(values = c(
        pal("blues", 2), 
        pal("unblues", 2),
        pal("greens"), 
        pal("greens", 3)
      )) +
      guides(fill = guide_legend(nrow = 2)) +
      
      apply_theme("bar-vertical", basesize = basesize, font = font) + 
      theme(
        axis.title.y = element_text(
          size = basesize,
          margin = margin(r = k(2))
        ),
        axis.line.x = element_line(linewidth = k(.05), color = pal("blues")),
        legend.key.height = unit(k(.1), "lines"),
        legend.key.spacing.y = unit(k(.5), "points"),
        legend.position = "bottom",
        legend.box.spacing = unit(k(.2), "lines"),
      )
    
    plot <- plot + 
      geom_text(
        aes(x = xpos, y = ypos, label = lab),
        labels, 
        color = pal("blues"),
        family = font,
        size = k(.9),
        vjust = 0,
        nudge_y = layer_scales(plot)$y$range$range[2] / 40,
        show.legend = FALSE
      )
    
  } else {
    
    plot <- plot_empty(title, source, basesize, font)
  }
  
  return(plot)
}


# Earnings by status ------------------------------------------------------

plot_earnings <- function(hero,
                          basesize,
                          font,
                          title = paste0(
                            "Earnings, ",
                            snap_data("earnings")$range |>
                              paste(collapse = "\u2013")
                          )) {
  
  k <- function(factor = 1) factor * basesize / .pt
  source <- "Source: ILO."
  t0 <- snap_data("earnings")$range[1]
  t1 <- snap_data("earnings")$range[2]
  data_iso <- snap_data("earnings", hero)$data
  
  if (nrow(data_iso) > 0) {
    
    df <- data_iso |> 
      mutate(
        pt = ifelse(is.na(lag(v)) & is.na(lead(v)), TRUE, FALSE),
        .by = status
      )
    
    df_pt <- filter(df, pt)
    
    labels <- df |> 
      filter(!is.na(v)) |> 
      filter(t == max(t), .by = status) |> 
      mutate(lab = prettylabel(v, spell = TRUE, currency = "$"))
    
    fills <- c(
      "Native"  = pal("blues", 2), 
      "Foreign" = pal("greens")
    )
    
    plot <- ggplot(df, aes(x = t, y = v, group = status, color = status)) + 
      geom_point(data = df_pt, size = .75, shape = 15, show.legend = FALSE) +
      geom_line(linewidth = k(.35), na.rm = TRUE) + 
      
      # Annotation
      geom_segment(
        data = labels,
        xend = t1 + .25,
        linetype = "11",
        linewidth = k(.1),
        color = "black",
      ) +
      geom_label(
        aes(y = v, fill = status, label = lab), 
        labels,
        x = t1 + .25,
        color = "white",
        family = font,
        fontface = "bold", 
        hjust = 0, 
        size = k(.9), 
        vjust = .5, 
        label.r = unit(.05, "lines"), 
        label.size = .1,
        show.legend = FALSE,
      ) +
      labs(title = title, caption = source) +
      
      # Scales
      scale_x_continuous(
        breaks = seq(t0, t1, 5),
        expand = expansion(mult = c(.025, .100))
      ) +
      scale_y_continuous(
        name = "Average monthly earnings (US$)",
        labels = function(x) prettylabel(x, spell = TRUE),
        expand = expansion(mult = c(0, .025))
      ) + 
      scale_color_manual(values = fills) +
      scale_fill_manual(values = fills) +
      coord_cartesian(clip = "off") + 
      
      # Theme
      apply_theme("line", basesize = basesize, font = font) + 
      theme(
        axis.title.y = element_text(
          size = basesize, 
          margin = margin(r = k(2))
        ),
        axis.ticks.x = element_line(color = pal("blues"), linewidth = k(.05)),
        axis.minor.ticks.x.bottom = element_blank(),
        legend.key.height = unit(k(.1), "lines"),
        legend.box.spacing = unit(k(.25), "lines"),
        # plot.margin = margin(k(4), 0, k(.25), k(4)),
      )
    
  } else {
    
    plot <- plot_empty(title, source, basesize, font)
  }
  
  return(plot)
}


# Working hours by status -------------------------------------------------

plot_hours <- function(hero,
                       basesize,
                       font,
                       title = paste0(
                         "Working hours, ",
                         snap_data("hours")$range |>
                           paste(collapse = "\u2013")
                       )) {
  
  k <- function(factor = 1) factor * basesize / .pt
  source <- "Source: ILO."
  t0 <- snap_data("hours")$range[1]
  t1 <- snap_data("hours")$range[2]
  data_iso <- snap_data("hours", hero)$data

  if (nrow(data_iso) > 0) {
    
    df <- data_iso |> 
      mutate(
        pt = ifelse(is.na(lag(v)) & is.na(lead(v)), TRUE, FALSE),
        .by = status
      )
    
    df_pt <- filter(df, pt)
    
    labels <- df |> 
      filter(!is.na(v)) |> 
      filter(t == max(t), .by = status) |> 
      mutate(lab = prettylabel(v, signif = 3))
    
    fills <- c(
      "Native"  = pal("blues", 2), 
      "Foreign" = pal("greens")
    )
    
    ggplot(df, aes(x = t, y = v, group = status, color = status)) + 
      geom_line()
    
    plot <- ggplot(df, aes(x = t, y = v, group = status, color = status)) + 
      geom_point(data = df_pt, size = .75, shape = 15, show.legend = FALSE) +
      geom_line(linewidth = k(.35), na.rm = TRUE) + 
      
      # Annotation
      geom_segment(
        data = labels,
        xend = t1 + .25,
        linetype = "11",
        linewidth = k(.1),
        color = "black",
      ) +
      geom_label(
        aes(y = v, fill = status, label = lab), 
        labels,
        x = t1 + .25,
        color = "white",
        family = font,
        fontface = "bold", 
        hjust = 0, 
        size = k(.9), 
        vjust = .5, 
        label.r = unit(.05, "lines"), 
        label.size = .1,
        show.legend = FALSE,
      ) +
      labs(title = title, caption = source) +
      
      # Scales
      scale_x_continuous(
        breaks = seq(t0, t1, 5),
        expand = expansion(mult = c(.025, .100))
      ) +
      scale_y_continuous(
        name = "Mean weekly hours actually worked",
        labels = prettylabel,
        expand = expansion(mult = c(0, .025))
      ) + 
      scale_color_manual(values = fills) +
      scale_fill_manual(values = fills) +
      coord_cartesian(clip = "off") + 
      
      # Theme
      apply_theme("line", basesize = basesize, font = font) + 
      theme(
        axis.title.y = element_text(
          size = basesize, 
          margin = margin(r = k(2))
        ),
        axis.ticks.x = element_line(color = pal("blues"), linewidth = k(.05)),
        axis.minor.ticks.x.bottom = element_blank(),
        legend.key.height = unit(k(.1), "lines"),
        legend.box.spacing = unit(k(.25), "lines"),
        # plot.margin = margin(k(4), 0, k(.25), k(4)),
      )
    
  } else {
    
    plot <- plot_empty(title, source, basesize, font)
  }
  
  return(plot)
}


# Foreign employment by sector --------------------------------------------

plot_empsector <- function(hero,
                           basesize,
                           font,
                           title = paste0(
                             "Foreign-born share of employment by sector, ",
                             snap_data("empsector")$range[1]
                           )) {
  
  k <- function(factor = 1) factor * basesize / .pt
  source <- "Source: ILO."
  t1 <- snap_data("empsector")$range[1]
  data_iso <- snap_data("empsector", hero)$data
  
  if (nrow(data_iso) > 0) {
    
    df <- data_iso |> 
      mutate(label = prettylabel(100 * v))
    
    plot <- ggplot(df, aes(x = sector, y = v)) +
      labs(title = title, caption = source) +
      geom_bar(stat = "identity", width = .7, fill = pal("blues", 2)) +
      
      scale_x_discrete(
        labels = break_lines(sectors$label),
        expand = expansion(mult = .05),
      ) +
      scale_y_continuous(
        name = "Per cent",
        label = function(x) 100 * x,
        expand = expansion(mult = c(.02, .1)),
      ) +

      apply_theme("bar-vertical", basesize = basesize, font = font) +
      theme(
        axis.text.x = element_text(
          size = basesize,
          angle = 50,
          hjust = 1,
          lineheight = k(.3),
          margin = margin(t = 0)
        ),
        axis.title.y = element_text(
          size = basesize,
          margin = margin(r = k(2))
        ),
        plot.caption = element_text(
          margin = margin(t = 0, r = k(), l = k())
        ),
      )
    
    plot <- plot +
      geom_text(
        aes(label = label),
        color = pal("blues"),
        size = k(.9),
        vjust = 0,
        nudge_y = layer_scales(plot)$y$range$range[2] / 40,
      )
    
  } else {
    
    plot_base <- ggplot() +
      ggtitle(title) +
      apply_theme("bar-vertical", basesize = basesize, font = font) +
      theme(
        panel.background = element_rect(color = NA, fill = pal("unblues", 6)),
      )
    
    plot <- ggdraw(plot_base) +
      draw_label(
        "No data",
        y = .5,
        fontfamily = font,
        color = pal("blues", 3),
        size = k(3)
      )
  }
  
  return(plot)
}





plot_gva <- function(hero,
                     basesize,
                     font,
                     title = paste0(
                       "Value added by economic sector, ",
                       snap_data("gva", hero)$range |>
                         paste(collapse = ", ")
                     )) {
  
  k <- function(factor = 1) factor * basesize / .pt
  source <- "Source: UNSD."
  
  data <- snap_data("gva", hero)$data
  
  if (nrow(data) > 0) {
    
    axis <- set_axis(data$v, "USD")
    
    plot <- ggplot(data, aes(x = sector, y = v, fill = factor(t))) +
      geom_bar(
        stat = "identity", 
        position = position_dodge(width = .9),
        width = .8
      ) +
      labs(title = title, caption = source) +
      
      scale_x_discrete(
        labels = break_lines(levels(data$sector)),
      ) + 
      scale_y_continuous(
        name = axis$title,
        breaks = axis$breaks,
        labels = axis$labels,
        expand = expansion(mult = c(.02, .05))
      ) +
      scale_fill_manual(values = c(pal("greens", 2), pal("blues", 2))) +
      
      apply_theme("bar-vertical", basesize = basesize, font = font) +
      theme(
        axis.text.x = element_text(
          size = basesize - 1,
          angle = 50,
          hjust = 1,
          lineheight = k(.3),
          margin = margin(t = 0, l = -k()),
        ),
        axis.title.y = element_text(
          size = basesize,
          margin = margin(r = k(2))
        ),
        legend.box.spacing = unit(-.5, "lines"),
        plot.caption = element_text(
          margin = margin(t = k(), r = k(), l = k())
        ),
      )
    
    nudge_y <- layer_scales(plot)$y$range$range[2] / 40
    
    plot <- plot +
      geom_text(
        aes(y = v + nudge_y, label = prettylabel(v)),
        position = position_dodge(width = .9),
        color = pal("blues"),
        family = font,
        size = k(.7),
        vjust = 0,
      )
    
  } else {
    
    plot <- plot_empty(title, source, basesize, font)
  }
  
  return(plot)
}

plot_exports <- function(hero,
                         basesize,
                         font,
                         title = paste0(
                           "Exports by economic sector, ",
                           snap_data("exports", hero)$range |>
                             paste(collapse = ", ")
                         )) {
  
  k <- function(factor = 1) factor * basesize / .pt
  source <- "Source: UN COMTRADE."
  
  data <- snap_data("exports", hero)$data
  
  if (sum(data$v) > 0) {
    
    axis <- set_axis(data$v, "USD")
    
    plot <- ggplot(data, aes(x = sector, y = v, fill = factor(t))) +
      geom_bar(
        stat = "identity", 
        position = position_dodge(width = .9),
        width = .8
      ) +
      labs(title = title, caption = source) +
      
      scale_x_discrete(
        labels = break_lines(levels(data$sector)),
      ) + 
      scale_y_continuous(
        name = axis$title,
        breaks = axis$breaks,
        labels = axis$labels,
        expand = expansion(mult = c(.02, .05))
      ) +
      scale_fill_manual(values = c(pal("greens", 2), pal("blues", 2))) +
      
      apply_theme("bar-vertical", basesize = basesize, font = font) +
      theme(
        axis.text.x = element_text(
          size = basesize - 1,
          angle = 50,
          hjust = 1,
          lineheight = k(.3),
          margin = margin(t = 0, l = -k()),
        ),
        axis.title.y = element_text(
          size = basesize,
          margin = margin(r = k(2))
        ),
        legend.box.spacing = unit(-.5, "lines"),
        plot.caption = element_text(
          margin = margin(t = k(), r = k(), l = k())
        ),
      )
    
    nudge_y <- layer_scales(plot)$y$range$range[2] / 40
    
    plot <- plot +
      geom_text(
        aes(y = v + nudge_y, label = ifelse(v == 0, "", prettylabel(v))),
        position = position_dodge(width = .9),
        color = pal("blues"),
        family = font,
        size = k(.7),
        vjust = 0,
      )
    
  } else {
    
    plot <- plot_empty(title, source, basesize, font)
  }
  
  return(plot)
}

plot_empoccup <- function(hero,
                          basesize,
                          font,
                          title = paste0(
                            "Employment by sector and skill level, ",
                            snap_data("empoccup", hero)$range[1]
                          )) {
  
  k <- function(factor = 1) factor * basesize / .pt
  source <- "Source: ILO."
  
  data <- snap_data("empoccup", hero)$data |> 
    mutate(v = 100 * n / sum(n))
  
  if (nrow(data) > 0) {
    
    # axis <- set_axis(data$n)
    
    df_agg <- data |>
      summarise(n = sum(n), .by = sector) |> 
      mutate(v = 100 * n / sum(n))
    
    plot <- ggplot() +
      geom_bar(
        aes(x = sector, y = v, fill = skill),
        data,
        stat = "identity", 
        position = "stack",
        width = .8
      ) +
      labs(title = title, caption = source) +
      
      scale_x_discrete(
        labels = break_lines(sectors$label),
      ) + 
      scale_y_continuous(
        name = "Per cent",
        # breaks = axis$breaks,
        # labels = axis$labels,
        expand = expansion(mult = c(.02, .05))
      ) +
      scale_fill_manual(
        name = "Skill level",
        values = c(
          "high" = pal("blues", 2),
          "medium" = pal("greens"), 
          "low" = pal("yellows"), 
          "unknown" = pal("grays", 3)
        ),
        labels = c("High", "Medium", "Low", "Unknown")
      ) +
      
      apply_theme("bar-vertical", basesize = basesize, font = font) +
      theme(
        axis.text.x = element_text(
          size = basesize - 3,
          angle = 50,
          hjust = 1,
          lineheight = k(.3),
          margin = margin(t = -k(.5), l = -k()),
        ),
        axis.title.y = element_text(
          size = basesize,
          margin = margin(r = k(2))
        ),
        legend.title = element_text(
          size = basesize,
          margin = margin(r = -k()),
        ),
        legend.box.spacing = unit(0, "lines"),
        plot.caption = element_text(
          margin = margin(t = k(), r = k(), l = k())
        ),
      )
    
    nudge_y <- layer_scales(plot)$y$range$range[2] / 40
    
    plot <- plot +
      geom_text(
        aes(
          x = sector, 
          y = v + nudge_y, 
          label = ifelse(n == 0, "", prettylabel(v, pct = TRUE))
        ),
        df_agg,
        color = pal("blues"),
        family = font,
        size = k(.6),
        vjust = 0,
      )
    
  } else {
    
    plot <- plot_empty(title, source, basesize, font)
  }
  
  return(plot)
}


plot_unemoccup <- function(hero,
                           basesize,
                           font,
                           title = paste0(
                             "Unemployment by previous economic sector, ",
                             snap_data("unemoccup", hero)$range[1]
                           )) {
  
  k <- function(factor = 1) factor * basesize / .pt
  source <- "Source: ILO."
  
  data <- snap_data("unemoccup", hero)$data
  
  if (nrow(data) > 0) {
    
    plot <- ggplot() +
      geom_bar(
        aes(x = sector, y = v),
        data,
        stat = "identity", 
        fill = pal("blues", 2),
        width = .8
      ) +
      labs(title = title, caption = source) +
      
      scale_x_discrete(
        labels = break_lines(sectors$label),
      ) + 
      scale_y_continuous(
        name = "Share of unemployed",
        labels = function(x) prettylabel(100 * x, pct = TRUE),
        expand = expansion(mult = c(.02, .05))
      ) +
      
      apply_theme("bar-vertical", basesize = basesize, font = font) +
      theme(
        axis.text.x = element_text(
          size = basesize - 3,
          angle = 50,
          hjust = 1,
          lineheight = k(.3),
          margin = margin(t = -k(.5), l = -k()),
        ),
        axis.title.y = element_text(
          size = basesize,
          margin = margin(r = k(2))
        ),
        legend.box.spacing = unit(0, "lines"),
        plot.caption = element_text(
          margin = margin(t = k(), r = k(), l = k())
        ),
      )
    
    nudge_y <- layer_scales(plot)$y$range$range[2] / 40
    
    plot <- plot +
      geom_text(
        aes(
          x = sector, 
          y = v + nudge_y, 
          label = ifelse(v == 0, "", prettylabel(100 * v))
        ),
        data,
        color = pal("blues"),
        family = font,
        size = k(.6),
        vjust = 0,
      )
    
  } else {
    
    plot <- plot_empty(title, source, basesize, font)
  }
  
  return(plot)
}


# Employment by skill -----------------------------------------------------

plot_empskill <- function(hero,
                          basesize,
                          font,
                          title = paste0(
                            "Employment by sex and skill level, ",
                            snap_data("empskill", hero)$range[1]
                          )) {
  
  k <- function(factor = 1) factor * basesize / .pt
  source <- "Source: ILO."
  name <- namer(hero)
  
  data <- snap_data("empskill", hero)$data
  
  if (nrow(data) > 0) {
    
    skill_plotter <- function(skill_level) {
      
      data_skill <- data |> 
        filter(skill == skill_level) |> 
        mutate(var = factor(var, levels = c("iso", "imm", "orig", "world")))
      
      data_skill_agg <- data_skill |> 
        summarise(v = sum(v), .by = c(geo, t, skill, var))
      
      plot <- ggplot(data_skill) +
        geom_bar(
          aes(x = var, y = v, fill = sex),
          stat = "identity", 
          width = .6,
        ) +
        labs(title = data_skill$skill[1]) +
        
        scale_x_discrete(
          labels = c(
            "General\npopulation", 
            "Immigrant\npopulation", 
            "Origin\ncountry\naverage", 
            "Global\naverage"
          ),
          expand = expansion(mult = .2),
        ) + 
        scale_y_continuous(
          name = "Share of employment",
          labels = function(x) prettylabel(100 * x, pct = TRUE),
          expand = expansion(mult = c(.02, .1))
        ) +
        scale_fill_manual(values = c(pal("blues", 2), pal("greens", 2))) +
        
        apply_theme("bar-vertical", basesize = basesize, font = font) +
        theme(
          axis.text.x = element_text(size = basesize - 1),
          legend.position = "none",
          panel.spacing.x = unit(1.25, "lines"),
          plot.title = element_text(face = "plain"),
          plot.margin = margin(k(), k(4), k(), k(4))
        )
      
      if (skill_level == "High-skill") {
        plot <- plot + 
          theme(
            axis.title.y = element_text(
              size = basesize,
              margin = margin(r = k(3))
            ),
          )
      }
      
      nudge_y <- layer_scales(plot)$y$range$range[2] / 30
      
      plot <- plot +
        geom_text(
          aes(
            x = var,
            y = v + nudge_y,
            label = prettylabel(100 * v)
          ),
          data_skill_agg,
          color = pal("blues"),
          family = font,
          size = k(),
          vjust = 0,
        )
      
      return(plot)
    }
    
    panels <- cowplot::plot_grid(
      skill_plotter("High-skill"),
      skill_plotter("Medium-skill"),
      skill_plotter("Low-skill"),
      nrow = 1
    )
    
    plot_legend <- ggplot(data, aes(x = var, y = v, fill = sex)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c(pal("blues", 2), pal("greens", 2))) +
      apply_theme("bar-vertical", basesize = basesize, font = font) + 
      theme(plot.margin = margin(0, 0, 0, 0))
    
    plot <- cowplot::plot_grid(
      ggplot() + 
        ggtitle(title) + 
        apply_theme("bar-vertical", basesize = basesize, font = font),
      panels,
      get_legend(plot_legend),
      ggplot() + 
        labs(caption = source) +
        apply_theme("bar-vertical", basesize = basesize, font = font) + 
        theme(plot.caption = element_text(margin = margin(0, k(), 0, k()))),
      
      ncol = 1,
      rel_heights = c(.15, 1, .1, .05)
    )
    
  } else {
    
    plot <- plot_empty(title, source, basesize, font)
  }
  
  return(plot)
}


# Employment by sector ----------------------------------------------------

plot_empcountry <- function(hero,
                            basesize,
                            font,
                            row = 1) {
  
  k <- function(factor = 1) factor * basesize / .pt
  source <- "Source: ILO."
  name <- namer(hero)
  
  data <- snap_data("empcountry", hero)$data
  
  if (nrow(data) > 0) {
    
    top_sectors <- levels(data$sector)
    top_sectors_labels <- sectors$label[match(top_sectors, sectors$code)]
    
    data <- data |> 
      mutate(
        label = factor(label, levels = top_sectors_labels),
        var = factor(var, levels = c("iso", "immig", "orig", "world"))
      )
    
    if (row == 1) {
      df <- data |> 
        filter(sector %in% top_sectors[1:3])
    } else {
      df <- data |> 
        filter(sector %in% top_sectors[4:6])
    }
    
    plot <- ggplot() +
      facet_wrap(~label, nrow = 1, scales = "free") +
      geom_bar(
        aes(x = var, y = v),
        df,
        stat = "identity", 
        width = .6,
        fill = pal("blues", 2),
      ) +
      
      scale_x_discrete(
        labels = c(
          "General\npopulation", 
          "Immigrant\npopulation",
          "Origin\ncountry\naverage", 
          "Global\naverage"
        ),
        expand = expansion(mult = .2),
      ) + 
      scale_y_continuous(
        name = "Share of employment",
        labels = function(x) prettylabel(100 * x, pct = TRUE),
        expand = expansion(mult = c(.02, .1))
      ) +
      
      apply_theme(
        "bar-vertical", 
        basesize = basesize, 
        font = font, 
        facets = TRUE
      ) +
      theme(
        axis.text.x = element_text(size = basesize - 1),
        axis.title.y = element_text(
          size = basesize,
          margin = margin(r = k(3))
        ),
        panel.spacing.x = unit(1.25, "lines"),
      )
    
    nudge_y <- layer_scales(plot)$y$range$range[2] / 40
    
    plot <- plot +
      geom_text(
        aes(
          x = var,
          y = v + nudge_y,
          label = format(100 * v, trim = TRUE, digits = 2, nsmall = 1)
        ),
        df,
        color = pal("blues"),
        family = font,
        size = k(),
        vjust = 0,
      )
    
    if (row == 2) {
      plot <- plot + labs(caption = source)
    }
    
  } else {
    
    plot <- plot_empty(title = NULL, source, basesize, font)
  }
  
  return(plot)
}




