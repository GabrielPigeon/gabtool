\name{g.predict}
\alias{g.predict}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
g.predict
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
g.predict(model, name.x, new.x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
%%     ~~Describe \code{model} here~~
}
  \item{name.x}{
%%     ~~Describe \code{name.x} here~~
}
  \item{new.x}{
%%     ~~Describe \code{new.x} here~~
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
function (model, name.x, new.x) 
{
    cm <- colMeans(model.matrix(model))
    cm.n <- matrix(cm, ncol = length(new.x)/length(name.x), nrow = length(cm), 
        byrow = F)
    cm.n[which(names(cm) \%in\% name.x), ] <- matrix(new.x, nrow = length(name.x), 
        byrow = T)
    yy <- coef(model) \%*\% cm.n
    yy
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
