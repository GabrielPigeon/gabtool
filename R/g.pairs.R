#' make pairs graph to observe correlations and all
#'
#' @param data a data frame containing the variables to plot
#' @param bigger.text a scaling factor for the text
#' @param ...   other parameters to pass oin to plot
#'
#' @return a graphic of pared correlation and histogram
#'
#' @examples data(CO2)
#' g.pairs(CO2)
#' @export
g.pairs <- function(data,bigger.text=1,...){
   panel.hist <- function(x, ...)
   {
      usr <- par("usr"); on.exit(par(usr))
      par(usr = c(usr[1:2], 0, 1.5) )
      h <- hist(x, plot = FALSE)
      breaks <- h$breaks; nB <- length(breaks)
      y <- h$counts; y <- y/max(y)
      rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
   }
   ## put (absolute) correlations on the upper panels,
   ## with size proportional to the correlations.
   panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
   {
      usr <- par("usr"); on.exit(par(usr))
      par(usr = c(0, 1, 0, 1))
      r <- abs(cor(x, y))
      txt <- format(c(r, 0.123456789), digits = digits)[1]
      txt <- paste0(prefix, txt)
      if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
      text(0.5, 0.5, txt, cex = cex.cor * r * bigger.text)
   }
   #pairs(data[,c("previous.pdo","dens.f","BW","rly.growth","next.pdo","wt.1yr","wt.2yr","afr","alr","lrs")],
   pairs(data,diag.panel = panel.hist,upper.panel=panel.cor)
}