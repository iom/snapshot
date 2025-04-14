

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
      "Native-born" = pal("blues", 2), 
      "Foreign-born" = pal("greens"),
      "Unknown" = pal("grays", 3)
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
        aes(y = n, fill = birth, label = lab), lab,
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
        name = axis$title,
        breaks = axis$breaks,
        labels = axis$labels,
        expand = expansion(mult = c(0, .025))
      ) + 
      scale_color_manual(breaks = order, values = fills) +
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
        plot.margin = margin(k(4), 0, k(.25), k(4)),
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
    
    birth_order <- snap_data("empeduc")$data$birth |> levels()
    sex_order <- snap_data("empeduc")$data$sex |> levels()
    educ_order <- snap_data("empeduc")$data$educ |> levels()
    group_order <- paste(
      rep(birth_order, each = length(sex_order)), 
      tolower(sex_order)
    )
    
    dims <- get_dims(
      ticks = length(educ_order), 
      inwidth = .1, 
      outwidth = .1
    )
    
    mid <- filter(dims, xtick) |> pull(mid)
    
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
        lab = ifelse(v >= .04, prettylabel(100 * v), NA)
      )
    
    plot <- ggplot(
      df, 
      aes(
        xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, 
        fill = group
      )
    ) + 
      geom_rect() +
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
        legend.position = "bottom",
        legend.box.spacing = unit(k(.2), "lines"),
        plot.margin = margin(k(4), 0, k(.25), k(4)),
      )
    
    plot <- plot + 
      geom_text(
        aes(
          x = (xmin + xmax) / 2, 
          y = ymax, 
          color = sex,
          label = lab
        ),
        family = font,
        size = k(.9),
        vjust = 1,
        nudge_y = -layer_scales(plot)$y$range$range[2] / 40,
        show.legend = FALSE
      ) + 
      scale_color_manual(values = c(pal("blues"), "white"))
    
  } else {
    
    plot <- plot_empty(title, source, basesize, font)
  }
  
  return(plot)
}








