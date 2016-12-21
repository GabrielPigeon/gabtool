###   inverse logit    ######
#' Title
#'
#' @param x  a logit number
#'
#' @return the inverse logit
#' @export
#'
inv.logit <-  function (x) {
   plogis(x)
}
