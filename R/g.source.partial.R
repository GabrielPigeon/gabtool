###   partial source  ###
#' Title
#'
#' @param file file to source from
#' @param start  first line
#' @param end  last line
#' @param ... 
#'
#' @export
#'
g.source.partial <- function(file,start=0,end=NULL,...){
   con <- file(file)
   read <- readLines(con)
   fname <- tempfile()
   if(is.null(end)){end <- length(read)}
   cat(read[start:end], file = fname,  sep = "\n")
   source(fname,...)
   file.remove(fname)
   close(con)
}
