#' makes an animated gif
#'
#' @param it numeric vector of values to pass to fun to generate series of frame
#' @param fun function taking for sole input it and returning a figure
#' @param file file in wich to save the output gif
#' @param width size of the gif in px
#' @param heigth size of the gif in px
#' @param delay delay value to pass on to convert, in millisecand between frame change
#' @param loop  value to pass on to convert, should the gif loop. if so keep at 0
#'
#' @return a gif image
#' @export
#'
g.makeGif <- function(it,fun,file,width=500,heigth=500,delay=10,loop=0){
  if(!substr(file,1,1) %in% c("~","/")){
    now=getwd()
    file=paste(now,file,sep = "/")
  }
  
  tmpdirname <- tempdir()
  if(!dir.exists(paste0(tmpdirname,"/gif")))   dir.create(paste0(tmpdirname,"/gif"))
  
  for(i in it){
    tmpfilename <- paste0(tmpdirname,"/gif/","gif",10000+i,".png")
    png(tmpfilename,width = width,height = heigth)
    fun(i)
    dev.off()
  }
  command <- paste0(" -delay ",delay," -loop ",loop," ",tmpdirname,"/gif/*.png ",file)
  system2("convert", args = command)
  unlink(paste0(tmpdirname,"/gif"),recursive = T)
}
