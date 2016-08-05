\name{g.r2}
\alias{g.r2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
g.r2
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
g.r2(model.final, model.null, type = "lme4")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model.final}{
%%     ~~Describe \code{model.final} here~~
}
  \item{model.null}{
%%     ~~Describe \code{model.null} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
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
function (model.final, model.null, type = "lme4") 
{
    if (type == "nlme") {
        vc.f <- VarCorr(model.final)
        vf <- sum((as.numeric(vc.f[, 2]))^2, na.rm = T)
        vc.i <- VarCorr(model.null)
        vi <- sum((as.numeric(vc.i[, 2]))^2, na.rm = T)
    }
    if (type == "lme4") {
        vf <- sum(as.numeric(model.final@REmat[, "Variance"]))
        vi <- sum(as.numeric(model.null@REmat[, "Variance"]))
    }
    r2 <- (vi - vf)/vi
    r2
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
