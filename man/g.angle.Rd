\name{g.angle}
\alias{g.angle}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
g.angle
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
g.angle(v1, v2, unit = "degree")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{v1}{
%%     ~~Describe \code{v1} here~~
}
  \item{v2}{
%%     ~~Describe \code{v2} here~~
}
  \item{unit}{
%%     ~~Describe \code{unit} here~~
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
function (v1, v2, unit = "degree") 
{
    theta <- acos(sum(v1 * v2)/(sqrt(sum(v1 * v1)) * sqrt(sum(v2 * 
        v2))))
    if (unit == "degree") 
        res <- theta * 180/pi
    if (unit == "rad") 
        res <- theta
    res
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
