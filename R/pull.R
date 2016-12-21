
#' exctract a column from dplyr as a vector
#'
#' @param x tibble
#' @param y name of the collumn to extract
#'
#' @return a vector
#' @export
#'
pull <- function(x,y) {x[,if(is.name(substitute(y))) deparse(substitute(y)) else y, drop = FALSE][[1]]}

