% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/g.Jags.R
\name{g.mkJagsKey}
\alias{g.mkJagsKey}
\title{Make Kags Key
convert categorical values to numeric in order of their occurence in the df}
\usage{
g.mkJagsKey(x)
}
\arguments{
\item{x}{vector of character (different factors) to convert to numeric for JAGS analysis}
}
\value{
an ordered vector of numeric representing the initial factors
}
\description{
Make Kags Key
convert categorical values to numeric in order of their occurence in the df
}
