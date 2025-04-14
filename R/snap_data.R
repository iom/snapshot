
#' Get data for plot
#'
#' @param key Get key.
#' @param iso Country.
#' @param ... Optional arguments.
#'
#' @returns List.
#'
#' @export
snap_data <- function(key, iso = NULL, ...) {
  
  fxn_name <- paste0("get_", key)
  
  # Check if key is valid
  if (exists(fxn_name, envir = asNamespace("snapshot"), inherits = FALSE)) {
    
    # If iso is provided, check if valid
    if (!is.null(iso)) {
      if (!(iso %in% gdidata::countrynames$iso3)) {
        cli::cli_abort("{iso} is not a valid ISO3 code.")
      }
    }
    
    getter <- get(fxn_name, envir = asNamespace("snapshot"))
    return(getter(iso, ...))
    
  } else {
    
    cli::cli_abort("{key} is not a valid getter key.")
  }
}