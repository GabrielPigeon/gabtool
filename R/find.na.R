#' find NAs in a dataframe & otionaly graph them
#'
#' @param file dataframe in which to check for na
#' @param plot logical value. should a graph be reported
#'
#' @return either the posisiton of NAs of a grpah
#'
#' @examples find.na(data,plot=T)
#' @export


find.na<-function(file,plot=F){
   if(plot==F){
      res <- lapply(file,FUN=function(x){
         which(is.na(x))
      }  )
      return(res)
   }
   if(plot==T){
      tmp <- as.data.frame(is.na(as.matrix(file)))
      tmp$row <- 1:nrow(tmp)
      g <- ggplot(melt(tmp,id.vars = "row"),aes(x=variable,y=row,fill=value))+
         geom_raster(alpha=0.5)+coord_flip()+scale_fill_manual(breaks=c(T,F),values =c("darkgrey","red"),labels=c("missing","present"))
      return(g)
   }
   
}
