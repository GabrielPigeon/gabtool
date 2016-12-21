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

###   plays an audio file  
#  watch out for spaces
g.MakeNoise <- function(your_audio_file){
   system(paste("vlc -Idummy --no-loop --no-repeat --playlist-autostart --no-media-library --play-and-exit", your_audio_file), wait = FALSE)
}



