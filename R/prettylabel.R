#' Pretty number formatting
#'
#' @description Formats numbers for display in charts.
#'
#' @param N Number or vector of numbers to be formatted.
#' @param signif Number of significant digits.
#' @param shorten Whether to abbreviate long numbers.
#' @param spell Whether to spell out "millions", "billions", and "trillions".
#' @param padzero Whether to include zeros after the decimal point.
#' @param pct Whether to include a percent sign.
#' @param currency Whether to add a currency sign.
#'
#' @return String or vector of strings.
#'
#' @export
prettylabel <- function(N,
                        signif = 2,
                        spell = FALSE,
                        padzero = FALSE,
                        pct = FALSE,
                        currency = NULL) {
  
  if (signif <= 0) {
    cli::cli_abort("`signif` must be greater than zero.")
  } 
  
  fmt <- function(num) {
    
    parts <- strsplit(as.character(num), "\\.")[[1]]
    units <- nchar(parts[1])
    decimals <- ifelse(is.na(parts[2]), 0, nchar(parts[2]))

    digits <- signif
    if (units > signif) digits <- units
    num <- signif(num, digits = digits)
    
    nsmall <- 0
    if (padzero & (units + decimals < signif)) {
      nsmall <- signif - units
    }
    
    format(
      num,
      digits = signif,
      nsmall = nsmall,
      trim = TRUE,
      big.mark = ",",
      scientific = FALSE
    )
  }

  if (spell) {

    suffixes <- list(
      pc = " per cent",
      mn = " million",
      bn = " billion"
    )

  } else {

    suffixes <- list(
      pc = "%",
      th = "K",
      mn = "M",
      bn = "B"
    )
  }

  labels <- c()

  for (n in N) {

    label <- list()
    label$prefix <- if(is.null(currency)) NULL else currency
    label$number <- NULL
    label$suffix <- NULL

    if (pct) {

      # if (n < 1) n <- 100 * n
      label$number <- fmt(n)
      label$suffix <- suffixes$pc

    } else if (n <= 999.5) {

      label$number <- fmt(n)

    } else if (n >= 999.5 & n < 999500) {
      
      if (spell) {
        label$number <- fmt(n)
      } else {
        n <- n / 1000
        label$number <- fmt(n)
        label$suffix <- suffixes$th
      }

    } else if (n >= 999500 & n < 999.5 * 10^6) {
      
      n <- n / 10^6
      label$number <- fmt(n)
      label$suffix <- suffixes$mn
      
    } else if (n >= 999.5 * 10^6 & n < 999.5 * 10^9) {
      
      n <- n / 10^9
      label$number <- fmt(n)
      label$suffix <- suffixes$bn
      
    }
    
    labels <- c(labels, paste0(label$prefix, label$number, label$suffix))
  }
  
  #   
  #   
  #   
  #   
  #   
  #   if (max_n < 12) {
  #     output$title <- units
  #     output$breaks <- c(0, 5, 10)
  #     output$labels <- waiver()
  #   }
  #   if (max_n >= 12 & max_n < 1200) {
  #     output$title <- units
  #     output$labels <- waiver()
  #   }
  #   if (max_n >= 1200 & max_n < 1.20 * 10^6) {
  #     output$title <- write_title("Thousands", units)
  #     output$labels <- function(x) x / 1000
  #   }
  #   if (max_n >= 1.20 * 10^6 & max_n < 1.40 * 10^6) {
  #     output$breaks <- seq(0, 1.25 * 10^6, .25 * 10^6)
  #     output$labels <- c("0", "0.25", "0.50", "0.75", "1", "1.25")
  #   }
  #   if (max_n >= 1.40 * 10^6 & max_n < 1.80 * 10^6) {
  #     output$breaks <- seq(0, 1.50 * 10^6, .50 * 10^6)
  #     output$labels <- c("0", "0.5", "1", "1.5")
  #   }
  #   
  #   if (max_n >= 1.20 * 10^9 & max_n < 1.40 * 10^9) {
  #     output$title <- write_title("Billions", units)
  #     output$breaks <- seq(0, 1.25 * 10^9, .25 * 10^9)
  #     output$labels <- c("0", "0.5", "1", "1.5")
  #   }
  #   if (max_n >= 1.40 * 10^9 & max_n < 1.80 * 10^9) {
  #     output$title <- write_title("Billions", units)
  #     output$breaks <- seq(0, 1.50 * 10^9, .50 * 10^9)
  #     output$labels <- c("0", "0.5", "1", "1.5")
  #   }
  #   if (max_n >= 1.80 * 10^9) {
  #     output$title <- write_title("Billions", units)
  #     output$labels <- function(x) x / 10^9
  #   }
  #   
  # }
  # 
  # 
  # 
  # nsmall <- ifelse(dotzero, d, 0)
  # 
  # labels <- c()
  # 
  # for (n in N) {
  # 
  #   if (shorten) {
  # 
  #     if (spell) {
  #       tn <- " trillion"
  #       bn <- " billion"
  #       mn <- " million"
  #     } else {
  #       tn <- "T"
  #       bn <- "B"
  #       mn <- "M"
  #     }
  # 
  #     magnitude <- abs(n) |> log10() |> floor()
  # 
  #     if (is.na(magnitude)) {
  #       labels <- c(labels, NA)
  #       next
  #     }
  # 
  #     labeler <- function(n, magnitude, divide = 1, suffix = NULL) {
  # 
  #       m <- n / divide
  #       new_magnitude <- abs(m) |> log10() |> floor()
  # 
  #       # If m is single digit, add decimal
  #       if (n != 0 & new_magnitude <= 0) {
  #         paste0(
  #           currency,
  #           format(
  #             m,
  #             trim = TRUE,
  #             digits = 1,
  #             big.mark = ",",
  #             nsmall = nsmall,
  #             scientific = FALSE
  #           ),
  #           suffix
  #         )
  #       } else {
  #         paste0(
  #           currency,
  #           format(
  #             m,
  #             trim = TRUE,
  #             digits = 1,
  #             big.mark = ",",
  #             nsmall = 0,
  #             scientific = FALSE
  #           ),
  #           suffix
  #         )
  #       }
  #     }
  # 
  #     label <- dplyr::case_when(
  #       magnitude >= 12 & magnitude < 15 ~ labeler(n, magnitude, 10^12, tn),
  #       magnitude >= 9 & magnitude < 12 ~ labeler(n, magnitude, 10^9, bn),
  #       magnitude >= 6 & magnitude < 9 ~ labeler(n, magnitude, 10^6, mn),
  #       magnitude >= 3 & magnitude < 6 & !spell ~
  #         labeler(n, magnitude, 10^3,  "K"),
  #       magnitude >= 3 & magnitude < 6 & spell ~
  #         labeler(round(n, -3), magnitude),
  #       # magnitude >= 2 & magnitude < 3 ~ labeler(round(n, -2), magnitude),
  #       magnitude < 3 ~ labeler(n, magnitude),
  #       .default = "0"
  #     )
  # 
  #   } else {
  # 
  #     if (d == 0) {
  #       label <- paste0(
  #         currency,
  #         format(n, digits = 1, big.mark = ",", nsmall = 0)
  #       )
  #     } else {
  #       label <- paste0(
  #         currency,
  #         format(n, digits = d, big.mark = ",", nsmall = nsmall)
  #       )
  #     }
  #   }
  # 
  #   if (pct) label <- paste0(label, "%")
  #   if (omit_zero & n == 0) label <- ""
  # 
  #   labels <- c(labels, label)
  # }
  
  return(labels)
}
