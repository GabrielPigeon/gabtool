###			Start Up Script			#####
#############################################################.
# library(plyr)
# library(dplyr)
# library(ICC)
# require(ggplot2)
# library(stringr)
# library(reshape2)
# library(lubridate)
# library(lme4)
#library(nlme)
# library(AICcmodavg)
#library(data.table)
#library(AED)
# options(stringsAsFactors=FALSE)
#############################################################.
# ###    install all packages  V. 13 mars 2013   ######
# install.packages(c("ade4", "AICcmodavg",  "BaSTA",
#                    "brew", "car","coda", "colorRamps", "colorspace",
#                    "data.table", "devtools",
#                    "dichromat", "digest", "doMC", "effects",
#                    "foreach", "formatR", "gdata", "gee",  "GGally", "ggm",
#                   "graph", "gtable", "gtools","gamm4",
#                    "ICC",  "igraph", "IPMpack", "kinship2", "knitr",
#                    "lavaan", "leaps", "lme4",
#                    "lubridate", "markdown", "MasterBayes", "mclust",
#                    "MCMCglmm",  "TeachingDemo",
#                    "MuMIn", "munsell",  "parser",  "pedantics",
#                    "pedigreemm", "pingr",
#                    "plyr","dplyr",  "ggplot2", "reshape2", "readr","tidyr", "stringr","readxl",
#                    "popbio", "extrafont",
#                    "psych", "RBGL", "RColorBrewer", "Rcpp",
#                     "rJava",  "rstudio",
#                    "texreg", "vegan", "xtable",  "zoo"))

#############################################################.
#############################################################.
#####			custum fonctions		#####.
#####  super pairs graph
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
#####	trouver NA
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

#############################################################.
##### write delim
write.delim <-function(d,file){
   write.table(x=d,file=file, col.names= TRUE, sep = "\t", quote=T, dec=".",row.names=F)
}

#############################################################.
#####	calcul de r2 des effets fixes selon Fanie
#pour nlme
g.r2 <- function (model.final, model.null, type = "lme4") 
{
   if (type == "nlme") {
      require(nlme)
      vc.f <- VarCorr(model.final)
      vf <- sum((as.numeric(vc.f[, 2]))^2, na.rm = T)
      vc.i <- VarCorr(model.null)
      vi <- sum((as.numeric(vc.i[, 2]))^2, na.rm = T)
   }
   if (type == "lme4") {
      require(lme4)
      vc.f <- as.data.frame(VarCorr(model.final))
      vf <- sum((as.numeric(vc.f[, 'sdcor']))^2, na.rm = T)
      vc.i <- as.data.frame(VarCorr(model.null))
      vi <- sum((as.numeric(vc.i[,  'sdcor']))^2, na.rm = T)
   }
   r2 <- (vi - vf)/vi
   r2
}


#############################################################.
##### calculate Standard Error
g.SE <- function(x) sqrt(var(x)/length(x))


###############################################################################.
####                  paire-wise correlations (with pearson)
####                  gives data frame with cor,p.value and IC
g.pairedCorr <- function (data) {
   q <- ncol(data)
   #      q2 <- sum((q-1):1)
   paired.mes <- data.frame()
   paires.names <- vector()
   for(j in 1:q){
      for (i in 1:q){
         t.n<-paste(names(data)[i],names(data)[j],sep="-")
         cor <- cor.test(data[,i],data[,j],method="pearson")
         n <- max(length(na.omit(data[,i])),length(na.omit(data[,j])))
         res <- c(cor$estimate,cor$conf.int,cor$p.value,n)
         paired.mes  <- rbind(paired.mes,res)
         paires.names <- c(paires.names,t.n)
      }
   }
   paired.mes  <- cbind(paired.mes,paires.names)
   
   tri<- upper.tri(matrix(1,nrow=q,ncol=q))
   paire<- matrix(paired.mes[,6],nrow=q,ncol=q)[tri]
   p.est<- matrix(paired.mes[,1],nrow=q,ncol=q)[tri]
   p.value <- matrix(paired.mes[,4],nrow=q,ncol=q)[tri]
   IC.L<- matrix(paired.mes[,2],nrow=q,ncol=q)[tri]
   IC.H<- matrix(paired.mes[,3],nrow=q,ncol=q)[tri]
   n<- matrix(paired.mes[,5],nrow=q,ncol=q)[tri]
   p.m <- data.frame(paire,p.est,p.value,IC.L,IC.H,n)
   p.m
}

###############################################################################.
##                         angle between 2 vectors
g.angle <- function(v1,v2,unit="degree"){
   #calculate the angle(default is in degree) between 2 vectors (v1 and v2)
   #where v1=c(x,y,z) .....
   theta <- acos( sum(v1 * v2) / ( sqrt(sum(v1 * v1)) * sqrt(sum(v2 * v2))))
   if(unit=="degree") res <- theta * 180 / pi
   if(unit=="rad") res <-theta
   res
}

###############################################################################.
##                          make multiple ggplot graph                
multiplot <- function(..., plotlist=NULL, ncol) {
    require(grid)

    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)

    numPlots = length(plots)

    # Make the panel
    plotCols = ncol                         # Number of columns of plots
    plotRows = ceiling(numPlots/plotCols) # Number of rows needed, calculated from # of cols

    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(plotRows,
                                               plotCols,
#                                                widths = unit(rep(1, plotCols), "null"),
#                                                heights = unit(rep(1, plotRows), "null"),
#                                                respect=respect
                                               )))
    vplayout <- function(x, y)
        viewport(layout.pos.row = x, layout.pos.col = y)

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
        curRow = ceiling(i/plotCols)
        curCol = (i-1) %% plotCols + 1
        print(plots[[i]], vp = vplayout(curRow, curCol ))
    }
}

###############################################################################.
##                   Add whisker tips to ggplot boxplot
# ggBoxplot_wisker <-    stat_summary(fun.data = function(x) {
# #    r <- quantile(x, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
# #    r <- c(r[2]-1.5*IQR(x),r[4]+1.5*IQR(x))
#    r <- c(boxplot(x,plot=F)$stats)
#    r <- r[c(1,5)]
#    names(r) <- c("ymin", "ymax")
#    r }     , geom="errorbar")

###############################################################################.
##                   function to predict y of a model
g.predict <- function (model,name.x,new.x) {
   #    model=t3
   #    name.x=c("yr","I(yr^2)","I(yr^3)")
   #    new.x=c(xx,xx^2,xx^3)
   cm <- colMeans(model.matrix(model))
   cm.n <- matrix(cm,ncol=length(new.x)/length(name.x),nrow=length(cm),byrow=F)
   cm.n[which(names(cm) %in% name.x),] <- matrix(new.x,nrow=length(name.x),byrow=T)
   yy <- coef(model) %*% cm.n
   yy
}
#########################################################.
###     fonction pour differente pallette de couleur dans R
g.colorPalette <- function (name, n, type = c("discrete", "continuous")) {
   require(wesanderson)
   list.palette <- list(
   # FOCUS PALETTES
   # Red as highlight
   redfocus = c("#CB181D", "#252525", "#525252", "#737373", "#969696", "#BDBDBD", "#D9D9D9", "#F0F0F0"),
   # Green as highlight
   greenfocus = c("#41AB5D", "#252525", "#525252", "#737373", "#969696", "#BDBDBD", "#D9D9D9", "#F0F0F0"),
   # Blue as highlight
   bluefocus = c("#0033FF", "#252525", "#525252", "#737373", "#969696", "#BDBDBD", "#D9D9D9", "#F0F0F0"),
   
   # EQUAL WEIGHT
   # Generated with rainbow(12, s = 0.6, v = 0.75)
   rainbow12equal = c("#BF4D4D", "#BF864D", "#BFBF4D", "#86BF4D", "#4DBF4D", "#4DBF86", "#4DBFBF", "#4D86BF", "#4D4DBF", "#864DBF", "#BF4DBF", "#BF4D86"),
   rainbow10equal = c("#BF4D4D", "#BF914D", "#A8BF4D", "#63BF4D", "#4DBF7A", "#4DBFBF", "#4D7ABF", "#634DBF", "#A84DBF", "#BF4D91"),
   rainbow8equal = c("#BF4D4D", "#BFA34D", "#86BF4D", "#4DBF69", "#4DBFBF", "#4D69BF", "#864DBF", "#BF4DA3"),
   rainbow6equal = c("#BF4D4D", "#BFBF4D", "#4DBF4D", "#4DBFBF", "#4D4DBF", "#BF4DBF"),
   # Generated with package "gplots" function rich.colors(12)
   rich12equal = c("#000040", "#000093", "#0020E9", "#0076FF", "#00B8C2", "#04E466", "#49FB25", "#E7FD09", "#FEEA02", "#FFC200", "#FF8500", "#FF3300"),
   rich10equal = c("#000041", "#0000A9", "#0049FF", "#00A4DE", "#03E070", "#5DFC21", "#F6F905", "#FFD701", "#FF9500", "#FF3300"),
   rich8equal = c("#000041", "#0000CB", "#0081FF", "#02DA81", "#80FE1A", "#FDEE02", "#FFAB00", "#FF3300"),
   rich6equal = c("#000043", "#0033FF", "#01CCA4", "#BAFF12", "#FFCC00", "#FF3300"),
   # Generated with package "fields" function tim.colors(12), which is said to emulate the default matlab colorset
   tim12equal = c("#00008F", "#0000EA", "#0047FF", "#00A2FF", "#00FEFF", "#5AFFA5", "#B5FF4A", "#FFED00", "#FF9200", "#FF3700", "#DB0000", "#800000"),
   tim10equal = c("#00008F", "#0000FF", "#0070FF", "#00DFFF", "#50FFAF", "#BFFF40", "#FFCF00", "#FF6000", "#EF0000", "#800000"),
   tim8equal = c("#00008F", "#0020FF", "#00AFFF", "#40FFBF", "#CFFF30", "#FF9F00", "#FF1000", "#800000"),
   tim6equal = c("#00008F", "#005AFF", "#23FFDC", "#ECFF13", "#FF4A00", "#800000"),
   # Generated with sort(brewer.pal(8,"Dark2")) #Dark2, Set2
   dark8equal = c("#1B9E77", "#666666", "#66A61E", "#7570B3", "#A6761D", "#D95F02", "#E6AB02", "#E7298A"),
   dark6equal = c("#1B9E77", "#66A61E", "#7570B3", "#D95F02", "#E6AB02", "#E7298A"),
   set8equal = c("#66C2A5", "#8DA0CB", "#A6D854", "#B3B3B3", "#E5C494", "#E78AC3", "#FC8D62", "#FFD92F"),
   set6equal = c("#66C2A5", "#8DA0CB", "#A6D854", "#E78AC3", "#FC8D62", "#FFD92F"),
   
   # MONOCHROME PALETTES
   # sort(brewer.pal(8,"Greens"))
   redmono = c("#99000D", "#CB181D", "#EF3B2C", "#FB6A4A", "#FC9272", "#FCBBA1", "#FEE0D2", "#FFF5F0"),
   greenmono = c("#005A32", "#238B45", "#41AB5D", "#74C476", "#A1D99B", "#C7E9C0", "#E5F5E0", "#F7FCF5"),
   bluemono = c("#084594", "#2171B5", "#4292C6", "#6BAED6", "#9ECAE1", "#C6DBEF", "#DEEBF7", "#F7FBFF"),
   grey8mono = c("#000000","#252525", "#525252", "#737373", "#969696", "#BDBDBD", "#D9D9D9", "#F0F0F0"),
   grey6mono = c("#242424", "#494949", "#6D6D6D", "#929292", "#B6B6B6", "#DBDBDB"),
   
   # Qualitative color schemes by Paul Tol
   tol1qualitative=c("#4477AA"),
   tol2qualitative=c("#4477AA", "#CC6677"),
   tol3qualitative=c("#4477AA", "#DDCC77", "#CC6677"),
   tol4qualitative=c("#4477AA", "#117733", "#DDCC77", "#CC6677"),
   tol5qualitative=c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677"),
   tol6qualitative=c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677","#AA4499"),
   tol7qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#DDCC77", "#CC6677","#AA4499"),
   tol8qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677","#AA4499"),
   tol9qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677", "#882255", "#AA4499"),
   tol10qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#882255", "#AA4499"),
   tol11qualitative=c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#882255", "#AA4499"),
   tol12qualitative=c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#AA4466", "#882255", "#AA4499"),
   tol14rainbow=c("#882E72", "#B178A6", "#D6C1DE", "#1965B0", "#5289C7", "#7BAFDE", "#4EB265", "#90C987", "#CAE0AB", "#F7EE55", "#F6C141", "#F1932D", "#E8601C", "#DC050C"),
   tol15rainbow=c("#114477", "#4477AA", "#77AADD", "#117755", "#44AA88", "#99CCBB", "#777711", "#AAAA44", "#DDDD77", "#771111", "#AA4444", "#DD7777", "#771144", "#AA4477", "#DD77AA"),
   tol18rainbow=c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788"),
   tol21rainbow= c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", "#117744", "#44AA77", "#88CCAA", "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788"),
   
   # Rcookbook colorblind pallette
   # The palette with grey:
   cbPalette = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"),
   # The palette with black:
   cbbPalette = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"),
   
   ##  colourlovers palette
   cheer_up_emo_kid = c('#556270', '#4ECDC4', '#C7F464', '#FF6B6B', '#C44D58'),
   ocean_five = c('#00A0B0','#6A4A3C','#CC333F','#EB6841','#EDC951'),
   albenaj = c('#AF2972','#F61358','#B6D504','#29D24E','#38BAF8'),
   jediKip = c('#792D1F','#C83E10','#F5A536','#A58C30','#C32F69'),
   sugar1 = c('#490A3D','#BD1550','#E97F02','#F8CA00','#8A9B0F')
   )
type <- match.arg(type)
big.pal <- c(wes_palettes,list.palette)
if(name %in% c('?', 'list','palettes','Palettes','Palette','palette')){print(big.pal)}
size.p <- length(big.pal)
pal <- big.pal[[name]]
if (is.null(pal)) 
#    print(big.pal)
   stop(paste("Palette not found. chose one of",size.p,'palettes'))
if (missing(n)) {
   n <- length(pal)
}
if (type == "discrete" && n > length(pal)) {
   stop("Number of requested colors greater than what palette can offer")
}
out <- switch(type, continuous = colorRampPalette(pal)(n), 
              discrete = pal[1:n])
structure(out, class = "palette", name = name)
}

###############################################################################.
###  fit normal histogram


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

###   plays an audio file  
#  watch out for spaces
g.MakeNoise <- function(your_audio_file){
   system(paste("vlc -Idummy --no-loop --no-repeat --playlist-autostart --no-media-library --play-and-exit", your_audio_file), wait = FALSE)
}

####  get the golden number for perfect proportions  ####.
g.golden.nb <- function()(1+sqrt(5))/2

###   inverse logit    ######
inv.logit <-  function (x) {
   plogis(x)
}

###   partial source  ###
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

#  exctract a column from dplyr as a vector
pull <- function(x,y) {x[,if(is.name(substitute(y))) deparse(substitute(y)) else y, drop = FALSE][[1]]}

