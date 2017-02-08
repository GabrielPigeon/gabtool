#' Make Kags Key
#' convert categorical values to numeric in order of their occurence in the df
#'
#' @param x vector of character (different factors) to convert to numeric for JAGS analysis
#'
#' @return an ordered vector of numeric representing the initial factors
#' @export
#'
g.mkJagsKey <- function(x){
  # TODO make conditional statement to treat character and numeric and avoid error
  x <- as.character(x)
  res <- as.numeric(factor(x, levels = unique(x))) 
  return(res)
}
