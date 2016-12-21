
#' Title
#'
#' @param x  vactor of numeric
#' @param breaks  number of bin
#' @param ...  args passed to nothing
#'
#' @export
#'
check.normal <- function (x,breaks=10,...) {
   nameofx <- deparse(substitute(x))
   if(sum(as.numeric(is.infinite(x)))>0){
      print("some numbers where infinite")
      n.inf <- sum(as.numeric(is.infinite(x)))
      print(paste("removed",n.inf,"values"))
   }
   x <- x[!is.infinite(x)]
   hist(x,breaks,xlab=nameofx,main=paste("Histogram of",nameofx))
   h<-hist(x,breaks,plot=F) 
   xfit<-seq(min(x),max(x),length=40) 
   yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
   yfit <- yfit*diff(h$mids[1:2])*length(x) 
   lines(xfit, yfit, col="blue", lwd=2)
}

