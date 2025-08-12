
#' Test a plot template for all countries
#'
#' @param key Plot key.
#' @param save_path Country.
#' @param ... Optional arguments.
#'
#' @description Generate the `key` plot for all countries and save to
#'   `save_path`.
#'
#' @export
test_plot <- function(key,
                      save_path = "test-plots",
                      width = 10,
                      height = 8,
                      ...) {
  
  plotter_name <- paste0("plot_", key)
  
  countries <- gdidata::countrynames |> 
    filter(snapshot == 1) |> 
    pull(iso3)
  
  if (exists(plotter_name, envir = asNamespace("snapshot"), inherits = FALSE)) {
    
    for (country in countries) {
      
      tryCatch({
        
        plotter <- get(paste0("plot_", key), envir = asNamespace("snapshot"))
        ggsave(
          str_glue("{save_path}/{key}/{country}.png"),
          plotter(country, basesize, font, ...),
          device = png,
          width = width,
          height = height,
          unit = "cm"
        )
        
      }, error = function(e) {
        cat("Error at ", country, ":", e$message, "\n")
      })
    }
    
  } else cli::cli_abort("{key} is not a valid plot key.")
}