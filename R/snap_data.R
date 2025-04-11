
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
  
  if (exists(fxn_name, envir = asNamespace("snapshot"), inherits = FALSE)) {
    
    if (!is.null(iso)) {
      
      if (iso %in% gdidata::countrynames$iso3) {
        
        getter <- get(fxn_name, envir = asNamespace("snapshot"))
        return(getter(iso, ...))
        
      } else cli::cli_abort("{iso} is not a valid ISO3 code.")
      
    } else cli::cli_abort("`iso` missing. Choose a country to plot.")
    
  } else cli::cli_abort("{key} is not a valid getter key.")
  
}
