
render_snapshot <- function(iso) {
  devtools::load_all(".")
  cat(build(iso, version), file = "_temp.qmd")
  quarto::quarto_render("_temp.qmd")
  unlink("_temp*")
}

build <- function(iso, version) {
  
  name <- countryname(iso, to = "name_text")
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
  
  yaml <- stringr::str_glue(
    '---
title: "{name}"
output-file: "{filename}"
iso: {iso}
execute:
  echo: false
warning: false
format:
  gdi-country-snapshot-typst: default
---'
  )
  
  text <- readLines("template.qmd")
  text <- text[-(1:10)]
  text <- paste(text, collapse = "\n")
  
  return(paste0(yaml, "\n", text))
}

