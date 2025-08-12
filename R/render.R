
#' Render a Snapshot
#'
#' @param iso Chosen country (ISO3).
#' @param target_dir Target directory. If `NULL`, saves output in the root
#'   directory.
#' @param simplify_name Whether to simply file names.
#'
#' @description By default, output file name is the country's official name. If
#'   `simplify_name` is set to `TRUE`, file name will be in all lowercase
#'   without special characters and spaces will be replaced with "-".
#'
#' @returns A PDF.
#'
#' @export
render_snapshot <- function(iso, 
                            target_dir = NULL,
                            simplify_name = FALSE) {
  
  devtools::load_all(".")
  
  # Check if ISO is valid
  valid_iso <- filter(gdidata::countrynames, snapshot == 1)$iso3
  if (!(iso %in% valid_iso)) {
    cli::cli_abort("{iso} has no associated Snapshot.")
  }
  
  name <- gdidata::countryname(iso, to = "name_text")
  if (simplify_name) {
    filename <- gsub(" ", "-", tolower(name))
    filename <- gsub("ô", "o", filename)
    filename <- gsub("ü", "u", filename)
    filename <- gsub("é", "e", filename)
    filename <- gsub("ã", "a", filename)
    filename <- gsub("í", "i", filename)
    filename <- gsub("ç", "c", filename)
    filename <- gsub("’", "-", filename)
    filename <- gsub("\\(", "", filename)
    filename <- gsub("\\)", "", filename)
    filename <- gsub(",", "", filename)
  } else {
    filename <- name
  }
  
  cat(
    build_template(iso, "template", name = name, filename = filename), 
    file = "_temp.qmd"
  )
  quarto::quarto_render("_temp.qmd")
  unlink("_temp*")
  
  if (!is.null(target_dir)) {
    source <- paste0(filename, ".pdf")
    target <- file.path(target_dir, paste0(filename, ".pdf"))
    file.rename(source, target)
  }
  
  beepr::beep(10)
}

# build <- function(iso, name, filename) {
#   
#   version <- packageVersion("snapshot")
#   
#   yaml <- stringr::str_glue(
# '---
# title: "{name}"
# output-file: "{filename}"
# iso: {iso}
# version: {version}
# execute:
#   echo: false
# warning: false
# format:
#   digest-typst: default
# ---'
#   )
#   
#   text <- readLines("template.qmd")
#   text <- text[-(1:10)]
#   text <- paste(text, collapse = "\n")
#   
#   return(paste0(yaml, "\n", text))
# }


#' Render a Labour Mobility Snapshot
#'
#' @param iso Chosen country (ISO3).
#' @param target_dir Target directory. If `NULL`, saves output in the root
#'   directory.
#' @param simplify_name Whether to simply file names.
#'
#' @description By default, output file name is the country's official name. If
#'   `simplify_name` is set to `TRUE`, file name will be in all lowercase
#'   without special characters and spaces will be replaced with "-".
#'
#' @returns A PDF.
#'
#' @export
render_labor_snapshot <- function(iso, 
                                  target_dir = NULL,
                                  simplify_name = FALSE) {
  
  devtools::load_all(".")
  
  # Check if ISO is valid
  valid_iso <- filter(gdidata::countrynames, snapshot == 1)$iso3
  if (!(iso %in% valid_iso)) {
    cli::cli_abort("{iso} has no associated Snapshot.")
  }
  
  name <- gdidata::countryname(iso, to = "name_text")
  if (simplify_name) {
    filename <- gsub(" ", "-", tolower(name))
    filename <- gsub("ô", "o", filename)
    filename <- gsub("ü", "u", filename)
    filename <- gsub("é", "e", filename)
    filename <- gsub("ã", "a", filename)
    filename <- gsub("í", "i", filename)
    filename <- gsub("ç", "c", filename)
    filename <- gsub("’", "-", filename)
    filename <- gsub("\\(", "", filename)
    filename <- gsub("\\)", "", filename)
    filename <- gsub(",", "", filename)
  } else {
    filename <- name
  }
  
  cat(
    build_template(iso, name = name, filename = filename, type = "labor"), 
    file = "_temp.qmd"
  )
  quarto::quarto_render("_temp.qmd")
  unlink("_temp*")
  
  if (!is.null(target_dir)) {
    source <- paste0(filename, ".pdf")
    target <- file.path(target_dir, paste0(filename, ".pdf"))
    file.rename(source, target)
  }
  
  beepr::beep(10)
}

build_template <- function(iso, name, filename, type = NULL) {
  
  template <- "snapshot-labor-typst"
  qmd <- "template.qmd"
  if (type == "labor") {
    template <- "snapshot-labor-typst"
    qmd <- "template-labor.qmd"
  }
  version <- packageVersion("snapshot")
  
  yaml <- stringr::str_glue(
    '---
title: "{name}"
output-file: "{filename}"
iso: {iso}
version: {version}
execute:
  echo: false
warning: false
format:
  {template}: default
---'
  )
  
  text <- readLines(qmd)
  text <- text[-(1:10)]
  text <- paste(text, collapse = "\n")
  
  return(paste0(yaml, "\n", text))
}

