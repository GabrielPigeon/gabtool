\name{g.colorPalette}
\alias{g.colorPalette}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
g.colorPalette
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
g.colorPalette(pal = NULL, plot = F, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pal}{
%%     ~~Describe \code{pal} here~~
}
  \item{plot}{
%%     ~~Describe \code{plot} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (pal = NULL, plot = F, ...) 
{
    list.palette <- list(FocusPallette = c("bluefocus", "greenfocus", 
        "redfocus"), EqualWeigth = c("rainbow6equal", "rainbow8equal", 
        "rainbow10equal", "rainbow12equal", "rich6equal", "rich8equal", 
        "rich10equal", "rich12equal", "tim6equal", "tim8equal", 
        "tim10equal", "tim12equal", "set6equal", "set8equal", 
        "dark6equal", "dark8equal"), MonoChrome = c("bluemono", 
        "greenmono", "redmono", "grey6mono", "grey8mono"), Qualitative = c("tol1qualitative", 
        "tol2qualitative", "tol3qualitative", "tol4qualitative", 
        "tol5qualitative", "tol6qualitative", "tol7qualitative", 
        "tol8qualitative", "tol9qualitative", "tol10qualitative", 
        "tol11qualitative", "tol12qualitative", "tol14rainbow", 
        "tol15rainbow", "tol18rainbow", "tol21rainbow"), ColorBlind = c("cbbPalette", 
        "cbPalette"))
    redfocus = c("#CB181D", "#252525", "#525252", "#737373", 
        "#969696", "#BDBDBD", "#D9D9D9", "#F0F0F0")
    greenfocus = c("#41AB5D", "#252525", "#525252", "#737373", 
        "#969696", "#BDBDBD", "#D9D9D9", "#F0F0F0")
    bluefocus = c("#0033FF", "#252525", "#525252", "#737373", 
        "#969696", "#BDBDBD", "#D9D9D9", "#F0F0F0")
    rainbow12equal = c("#BF4D4D", "#BF864D", "#BFBF4D", "#86BF4D", 
        "#4DBF4D", "#4DBF86", "#4DBFBF", "#4D86BF", "#4D4DBF", 
        "#864DBF", "#BF4DBF", "#BF4D86")
    rainbow10equal = c("#BF4D4D", "#BF914D", "#A8BF4D", "#63BF4D", 
        "#4DBF7A", "#4DBFBF", "#4D7ABF", "#634DBF", "#A84DBF", 
        "#BF4D91")
    rainbow8equal = c("#BF4D4D", "#BFA34D", "#86BF4D", "#4DBF69", 
        "#4DBFBF", "#4D69BF", "#864DBF", "#BF4DA3")
    rainbow6equal = c("#BF4D4D", "#BFBF4D", "#4DBF4D", "#4DBFBF", 
        "#4D4DBF", "#BF4DBF")
    rich12equal = c("#000040", "#000093", "#0020E9", "#0076FF", 
        "#00B8C2", "#04E466", "#49FB25", "#E7FD09", "#FEEA02", 
        "#FFC200", "#FF8500", "#FF3300")
    rich10equal = c("#000041", "#0000A9", "#0049FF", "#00A4DE", 
        "#03E070", "#5DFC21", "#F6F905", "#FFD701", "#FF9500", 
        "#FF3300")
    rich8equal = c("#000041", "#0000CB", "#0081FF", "#02DA81", 
        "#80FE1A", "#FDEE02", "#FFAB00", "#FF3300")
    rich6equal = c("#000043", "#0033FF", "#01CCA4", "#BAFF12", 
        "#FFCC00", "#FF3300")
    tim12equal = c("#00008F", "#0000EA", "#0047FF", "#00A2FF", 
        "#00FEFF", "#5AFFA5", "#B5FF4A", "#FFED00", "#FF9200", 
        "#FF3700", "#DB0000", "#800000")
    tim10equal = c("#00008F", "#0000FF", "#0070FF", "#00DFFF", 
        "#50FFAF", "#BFFF40", "#FFCF00", "#FF6000", "#EF0000", 
        "#800000")
    tim8equal = c("#00008F", "#0020FF", "#00AFFF", "#40FFBF", 
        "#CFFF30", "#FF9F00", "#FF1000", "#800000")
    tim6equal = c("#00008F", "#005AFF", "#23FFDC", "#ECFF13", 
        "#FF4A00", "#800000")
    dark8equal = c("#1B9E77", "#666666", "#66A61E", "#7570B3", 
        "#A6761D", "#D95F02", "#E6AB02", "#E7298A")
    dark6equal = c("#1B9E77", "#66A61E", "#7570B3", "#D95F02", 
        "#E6AB02", "#E7298A")
    set8equal = c("#66C2A5", "#8DA0CB", "#A6D854", "#B3B3B3", 
        "#E5C494", "#E78AC3", "#FC8D62", "#FFD92F")
    set6equal = c("#66C2A5", "#8DA0CB", "#A6D854", "#E78AC3", 
        "#FC8D62", "#FFD92F")
    redmono = c("#99000D", "#CB181D", "#EF3B2C", "#FB6A4A", "#FC9272", 
        "#FCBBA1", "#FEE0D2", "#FFF5F0")
    greenmono = c("#005A32", "#238B45", "#41AB5D", "#74C476", 
        "#A1D99B", "#C7E9C0", "#E5F5E0", "#F7FCF5")
    bluemono = c("#084594", "#2171B5", "#4292C6", "#6BAED6", 
        "#9ECAE1", "#C6DBEF", "#DEEBF7", "#F7FBFF")
    grey8mono = c("#000000", "#252525", "#525252", "#737373", 
        "#969696", "#BDBDBD", "#D9D9D9", "#F0F0F0")
    grey6mono = c("#242424", "#494949", "#6D6D6D", "#929292", 
        "#B6B6B6", "#DBDBDB")
    tol1qualitative = c("#4477AA")
    tol2qualitative = c("#4477AA", "#CC6677")
    tol3qualitative = c("#4477AA", "#DDCC77", "#CC6677")
    tol4qualitative = c("#4477AA", "#117733", "#DDCC77", "#CC6677")
    tol5qualitative = c("#332288", "#88CCEE", "#117733", "#DDCC77", 
        "#CC6677")
    tol6qualitative = c("#332288", "#88CCEE", "#117733", "#DDCC77", 
        "#CC6677", "#AA4499")
    tol7qualitative = c("#332288", "#88CCEE", "#44AA99", "#117733", 
        "#DDCC77", "#CC6677", "#AA4499")
    tol8qualitative = c("#332288", "#88CCEE", "#44AA99", "#117733", 
        "#999933", "#DDCC77", "#CC6677", "#AA4499")
    tol9qualitative = c("#332288", "#88CCEE", "#44AA99", "#117733", 
        "#999933", "#DDCC77", "#CC6677", "#882255", "#AA4499")
    tol10qualitative = c("#332288", "#88CCEE", "#44AA99", "#117733", 
        "#999933", "#DDCC77", "#661100", "#CC6677", "#882255", 
        "#AA4499")
    tol11qualitative = c("#332288", "#6699CC", "#88CCEE", "#44AA99", 
        "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", 
        "#882255", "#AA4499")
    tol12qualitative = c("#332288", "#6699CC", "#88CCEE", "#44AA99", 
        "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", 
        "#AA4466", "#882255", "#AA4499")
    tol14rainbow = c("#882E72", "#B178A6", "#D6C1DE", "#1965B0", 
        "#5289C7", "#7BAFDE", "#4EB265", "#90C987", "#CAE0AB", 
        "#F7EE55", "#F6C141", "#F1932D", "#E8601C", "#DC050C")
    tol15rainbow = c("#114477", "#4477AA", "#77AADD", "#117755", 
        "#44AA88", "#99CCBB", "#777711", "#AAAA44", "#DDDD77", 
        "#771111", "#AA4444", "#DD7777", "#771144", "#AA4477", 
        "#DD77AA")
    tol18rainbow = c("#771155", "#AA4488", "#CC99BB", "#114477", 
        "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", 
        "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", 
        "#DDAA77", "#771122", "#AA4455", "#DD7788")
    tol21rainbow = c("#771155", "#AA4488", "#CC99BB", "#114477", 
        "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", 
        "#117744", "#44AA77", "#88CCAA", "#777711", "#AAAA44", 
        "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", 
        "#AA4455", "#DD7788")
    cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", 
        "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
        "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    if (is.null(pal)) 
        print(list.palette)
    if (length(pal) == 1) {
        ifelse(is.numeric(pal), pal <- as.character(unlist(list.palette)[pal]), 
            pal <- pal)
        print(eval(parse(text = pal)))
    }
    if (length(pal) > 1) {
        col <- eval(parse(text = pal))
        plot <- T
    }
    if (plot == T) {
        col <- eval(parse(text = pal))
        n <- length(col)
        plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1), 
            axes = FALSE, xlab = "", ylab = "", ...)
        rect(0:(n - 1)/n, 0, 1:n/n, 1, col = col, border = "light gray")
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
