
plot_migpyr <- function(hero,
                        basesize,
                        font,
                        title = paste0(
                          "Age and sex structure of migrants, ",
                          plot_data("migpyr", hero)$range
                        )) {
  
  k <- function(factor = 1) factor * basesize / .pt
  source <- "Source: IOM Global Data Institute; UN DESA."
  name <- namer(hero)
  
  data <- plot_data("migpyr", hero)$data
  
  plot_elements <- list(
    facet_wrap(~.data$panel, scales = "free_y"),
    annotate(
      "rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 3.5,
      fill = pal("unblues", 5), alpha = .5
    ),
    annotate(
      "rect", xmin = -Inf, xmax = Inf, ymin = 13.5, ymax = Inf,
      fill = pal("unblues", 5), alpha = .5
    ),
    geom_vline(
      xintercept = 0, 
      color = pal("unblues", 4), 
      linewidth = k(.075)
    ),
    labs(title = title, caption = source),
    apply_theme(
      "bar-horizontal", 
      basesize = basesize, 
      font = font, 
      facets = TRUE
    ),
    scale_y_discrete(expand = expansion(mult = .05)),
    theme(
      axis.title.x = element_text(size = basesize - 2, color = pal("blues", 5)),
      axis.text.y = element_text(size = basesize - 2),
      panel.spacing.x = unit(k(), "lines"),
      plot.caption = element_text(margin = margin(t = 0, r = k(), l = k())),
      plot.margin = margin(k(4), k(4), k(.25), k(4)),
      strip.text = element_text(
        size = basesize,
        margin = margin(t = 0, b = k(2))
      )
    )
  )
  
  if (nrow(drop_na(data)) > 0) {
    
    wpp <- plot_data("pyr", hero)$data
    
    extent <- max(abs(c(drop_na(data, v)$v, wpp$v)))
    
    set_breaks <- c(-4, -2, 0, 2, 4)
    set_labels <- c("4%", "2%", 0, "2%", "4%")
    if (extent >= 4.5 & extent < 6.5) {
      set_breaks <- c(-5, -2.5, 0, 2.5, 5)
      set_labels <- c("5%", "2.5%", 0, "2.5%", "5%")
    }
    if (extent >= 6.5 & extent < 9) {
      set_breaks <- c(-8, -4, 0, 4, 8)
      set_labels <- c("8%", "4%", 0, "4%", "8%")
    }
    if (extent >= 9 & extent < 17) {
      set_breaks <- c(-16, -8, 0, 8, 16)
      set_labels <- c("16%", "8%", 0, "8%", "16%")
    }
    if (extent >= 17) {
      set_breaks <- c(-20, -10, 0, 10, 20)
      set_labels <- c("20%", "10%", 0, "10%", "20%")
    }
    
    df <- data |> 
      mutate(
        lab = prettylabel(v, signif = 1, pct = TRUE),
        v = case_when(sex == "male" ~ -v, .default = v),
      )
    
    plot <- ggplot(
      df, 
      aes(x = .data$v, y = forcats::fct_relevel(.data$age, "5-9", after = 1))
    ) +
      plot_elements +
      geom_bar(aes(fill = .data$sex), stat = "identity", width = .7) + 
      geom_path(aes(group = .data$sex, color = .data$sex), wpp) +
      
      scale_x_continuous(
        name = "",
        limits = c(-extent, extent),
        breaks = set_breaks,
        labels = set_labels,
        expand = expansion(mult = .05)
      ) +
      scale_fill_manual(
        name = "Migrant population:",
        labels = c("Male", "Female"),
        values = c(pal("blues", 3), pal("greens", 3))
      ) + 
      scale_color_manual(
        name = "General population:",
        labels = c("Male", "Female"),
        values = c(pal("blues"), pal("greens"))
      ) +
      
      theme(
        legend.spacing.y = unit(10, "lines"),
        legend.title = element_text(
          size = basesize, margin = margin(r = -k(2))),
        legend.text = element_text(margin = margin(r = -k(), l = k())),
        legend.position = "bottom",
        legend.justification.bottom = "right",
        legend.box.spacing = unit(0, "lines"),
      )
      
  } else {
    
    df <- expand.grid(
      age = c(
        "0-4", 
        "5-9", 
        "10-14", 
        "15-19", 
        "20-24", 
        "25-29", 
        "30-34", 
        "35-39", 
        "40-44", 
        "45-49", 
        "50-54", 
        "55-59", 
        "60-64", 
        "65-69", 
        "70-74", 
        "75+"
      ),
      panel = c("Emigrants", "Immigrants"),
      v = 0
    )
    
    plot <- ggplot(
      df, 
      aes(x = .data$v, y = forcats::fct_relevel(.data$age, "5-9", after = 1))
    ) +
      plot_elements +
      geom_bar(stat = "identity") + 
      scale_x_continuous(
        limits = c(-10, 10),
        breaks = NULL,
        expand = expansion(mult = .05)
      )
      
    plot <- cowplot::ggdraw(plot) + 
      cowplot::draw_label(
        "No data", 
        x = .2605, y = .475,
        fontfamily = font,
        color = pal("blues", 3), 
        size = k(3)
      ) + 
      cowplot::draw_label(
        "No data", 
        x = .7765, y = .475,
        fontfamily = font,
        color = pal("blues", 3), 
        size = k(3)
      )
  }
  
  return(plot)
}

plot_immpyr <- function(hero,
                        basesize,
                        font,
                        title = paste0(
                          "Age and sex structure of immigrants, ",
                          plot_data("migpyr", hero)$range
                        )) {
  
  k <- function(factor = 1) factor * basesize / .pt
  source <- "Source: IOM Global Data Institute; UN DESA."
  name <- namer(hero)
  
  data <- plot_data("migpyr", hero)$data |> 
    filter(panel == "Immigrants")
  
  plot_elements <- list(
    annotate(
      "rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 3.5,
      fill = pal("unblues", 5), alpha = .5
    ),
    annotate(
      "rect", xmin = -Inf, xmax = Inf, ymin = 13.5, ymax = Inf,
      fill = pal("unblues", 5), alpha = .5
    ),
    geom_vline(
      xintercept = 0, 
      color = pal("unblues", 4), 
      linewidth = k(.075)
    ),
    labs(title = title, caption = source),
    apply_theme(
      "bar-horizontal", 
      basesize = basesize, 
      font = font, 
      facets = TRUE
    ),
    scale_y_discrete(expand = expansion(mult = .05)),
    theme(
      axis.title.x = element_text(size = basesize - 2, color = pal("blues", 5)),
      axis.text.y = element_text(size = basesize - 2),
      plot.caption = element_text(margin = margin(t = 0, r = k(), l = k())),
      plot.margin = margin(k(4), k(4), k(.25), k(4)),
    )
  )
  
  if (nrow(drop_na(data)) > 0) {
    
    wpp <- plot_data("pyr", hero)$data
    
    extent <- max(abs(c(drop_na(data, v)$v, wpp$v)))
    
    set_breaks <- c(-4, -2, 0, 2, 4)
    set_labels <- c("4%", "2%", 0, "2%", "4%")
    if (extent >= 4.5 & extent < 6.5) {
      set_breaks <- c(-5, -2.5, 0, 2.5, 5)
      set_labels <- c("5%", "2.5%", 0, "2.5%", "5%")
    }
    if (extent >= 6.5 & extent < 9) {
      set_breaks <- c(-8, -4, 0, 4, 8)
      set_labels <- c("8%", "4%", 0, "4%", "8%")
    }
    if (extent >= 9 & extent < 17) {
      set_breaks <- c(-16, -8, 0, 8, 16)
      set_labels <- c("16%", "8%", 0, "8%", "16%")
    }
    if (extent >= 17) {
      set_breaks <- c(-20, -10, 0, 10, 20)
      set_labels <- c("20%", "10%", 0, "10%", "20%")
    }
    
    df <- data |> 
      mutate(
        lab = prettylabel(v, signif = 1, pct = TRUE),
        v = case_when(sex == "male" ~ -v, .default = v),
      )
    
    plot <- ggplot(
      df, 
      aes(x = .data$v, y = forcats::fct_relevel(.data$age, "5-9", after = 1))
    ) +
      plot_elements +
      geom_bar(aes(fill = .data$sex), stat = "identity", width = .7) + 
      geom_path(aes(group = .data$sex, color = .data$sex), wpp) +
      
      scale_x_continuous(
        name = "",
        limits = c(-extent, extent),
        breaks = set_breaks,
        labels = set_labels,
        expand = expansion(mult = .05)
      ) +
      scale_fill_manual(
        name = "Immigrant population",
        labels = c("Male", "Female"),
        values = c(pal("blues", 3), pal("greens", 3))
      ) + 
      scale_color_manual(
        name = "General population",
        labels = c("Male", "Female"),
        values = c(pal("blues"), pal("greens"))
      ) +
      guides(
        fill = guide_legend(nrow = 1),
        color = guide_legend(nrow = 1),
      ) +
      
      theme(
        legend.key.height = unit(basesize - 2.5, "points"),
        legend.title = element_text(
          size = basesize, margin = margin(b = k())
        ),
        # legend.text = element_text(margin = margin(r = -k(), l = k())),
        legend.position = "bottom",
        legend.direction = "vertical",
        legend.box.spacing = unit(0, "lines"),
        plot.caption = element_text(
          margin = margin(t = k(3.5), r = k(1), l = k(1))
        ),
        
        
        # legend.background = element_rect(fill = NA, color = "red"),
        # legend.box.background = element_rect(fill = NA, color = "green"),
        # legend.box.margin = margin(0, 0, 0, 0),
        # legend.margin = margin(0, 0, 0, 0),
        # legend.spacing = unit(0, "lines"),
      )
      
  } else {
    
    df <- expand.grid(
      age = c(
        "0-4", 
        "5-9", 
        "10-14", 
        "15-19", 
        "20-24", 
        "25-29", 
        "30-34", 
        "35-39", 
        "40-44", 
        "45-49", 
        "50-54", 
        "55-59", 
        "60-64", 
        "65-69", 
        "70-74", 
        "75+"
      ),
      v = 0
    )
    
    plot <- ggplot(
      df, 
      aes(x = .data$v, y = forcats::fct_relevel(.data$age, "5-9", after = 1))
    ) +
      plot_elements +
      geom_bar(stat = "identity") + 
      scale_x_continuous(
        limits = c(-10, 10),
        breaks = NULL,
        expand = expansion(mult = .05)
      )
      
    plot <- cowplot::ggdraw(plot) + 
      cowplot::draw_label(
        "No data", 
        x = .2605, y = .475,
        fontfamily = font,
        color = pal("blues", 3), 
        size = k(3)
      ) + 
      cowplot::draw_label(
        "No data", 
        x = .7765, y = .475,
        fontfamily = font,
        color = pal("blues", 3), 
        size = k(3)
      )
  }
  
  return(plot)
}


