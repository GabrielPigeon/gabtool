#' caculate angle between 2 vectors
#'
#' @param v1 vector 1
#' @param v2 vector 1
#' @param unit character,. either "rad" or "degree" 
#'
#' @return angle between 2 vector as a single numeric
#' @export
#'
#' @examples g.angle(c(1,3,4),c(0,0,-4),"degree")
#' 
g.angle <- function(v1,v2,unit="degree"){
   #calculate the angle(default is in degree) between 2 vectors (v1 and v2)
   #where v1=c(x,y,z) .....
   theta <- acos( sum(v1 * v2) / ( sqrt(sum(v1 * v1)) * sqrt(sum(v2 * v2))))
   if(unit=="degree") res <- theta * 180 / pi
   if(unit=="rad") res <-theta
   res
}