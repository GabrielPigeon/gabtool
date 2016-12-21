#' caculate SE
#'
#' @param x vector of numeric
#'
#' @return SE
#' @export
#'
g.SE <- function(x) {sqrt(var(x)/length(x))}

