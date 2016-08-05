\name{g.pairedCorr}
\alias{g.pairedCorr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
g.pairedCorr
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
g.pairedCorr(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
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
function (data) 
{
    q <- ncol(data)
    paired.mes <- data.frame()
    paires.names <- vector()
    for (j in 1:q) {
        for (i in 1:q) {
            t.n <- paste(names(data)[i], names(data)[j], sep = "-")
            cor <- cor.test(data[, i], data[, j], method = "pearson")
            n <- max(length(na.omit(data[, i])), length(na.omit(data[, 
                j])))
            res <- c(cor$estimate, cor$conf.int, cor$p.value, 
                n)
            paired.mes <- rbind(paired.mes, res)
            paires.names <- c(paires.names, t.n)
        }
    }
    paired.mes <- cbind(paired.mes, paires.names)
    tri <- upper.tri(matrix(1, nrow = q, ncol = q))
    paire <- matrix(paired.mes[, 6], nrow = q, ncol = q)[tri]
    p.est <- matrix(paired.mes[, 1], nrow = q, ncol = q)[tri]
    p.value <- matrix(paired.mes[, 4], nrow = q, ncol = q)[tri]
    IC.L <- matrix(paired.mes[, 2], nrow = q, ncol = q)[tri]
    IC.H <- matrix(paired.mes[, 3], nrow = q, ncol = q)[tri]
    n <- matrix(paired.mes[, 5], nrow = q, ncol = q)[tri]
    p.m <- data.frame(paire, p.est, p.value, IC.L, IC.H, n)
    p.m
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
