#' write tab delimited files
#'
#' @param d  data frame to write
#' @param file  where to save it
#'
#' @export
#'
write.delim <-function(d,file){
   write.table(x=d,file=file, col.names= TRUE, sep = "\t", quote=T, dec=".",row.names=F)
}
