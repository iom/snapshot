#' Choose an IOM color
#'
#' @param color Accepts "blues", "unblues", "yellows", "greens", "oranges",
#'    "reds", and "grays".
#' @param shade The shade of the chosen color, from 1 (darkest) to 5 (lightest).
#'    Accepts vectors.
#'
#' @return Hex code/s as a string or vector of strings.
#' @examples
#'
#' pal("blues")
#' pal("reds", c(1, 3, 5))
#'
#' @export
pal <- function(color, shade = 1) {
  
  palette <- list(
    blues    = c("#0033A0", "#4068B8", "#8099D0", "#B3C2E3", "#D9E0F1"),
    unblues  = c("#418FDE", "#84ADEC", "#ADC9F2", "#CEDEF7", "#E6EFFB", "#F4F8FD"),
    yellows  = c("#FFB81C", "#FFCA55", "#FFDC8E", "#FFEABB", "#FFF4DD"),
    greens   = c("#5CB8B2", "#85CAC5", "#AEDCD9", "#CEEAE8", "#E7F4F3"),
    oranges  = c("#FF671F", "#FF8D57", "#FFB38F", "#FFD1BC", "#FFE8DD"),
    reds     = c("#D22630", "#DD5C64", "#E99398", "#F2BEC1", "#F8DEE0"),
    grays    = c("#404040", "#666666", "#999999", "#CCCCCC", "#F2F2F2")
  )
  
  colors <- c()
  for (i in 1:length(shade)) {
    colors <- c(colors, palette[[color]][shade[i]])
  }
  
  return(colors)
}
