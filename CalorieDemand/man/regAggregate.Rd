\name{regAggregate}
\alias{regAggregate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Regional aggregation}
\description{Country-specific data are aggregated on the regional level according to a specified function}
\usage{
regAggregate(data, vectorfunction = function(x) {sum(x, na.rm = TRUE)}, 
  query = iso_reg, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{demand object}
  \item{vectorfunction}{function used for aggregation, default="sum"}
  \item{query}{data.frame containing grouping elements using "iso" for data and "reg" for aggregation levels}
  \item{...}{any information additionally needed for function vectorfunction}
}
\details{Country-specific time series are aggregated according to grouping elements defined in the data.frame query and by applying the function specified as vectorfunction}
\value{demand object with the same temporal resolution as data}
\references{Bodirsky, B.L., S. Rolinski, A. Biewald, I. Weindl, A. Popp and H. Lotze-Campen (2015): 'Global food demand scenarios for the 21st century', PlosOne, doi:10.1371/journal.pone.0139201.}
\author{Susanne Rolinski, Benjamin Bodirsky}
%\note{}
%\seealso{}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
