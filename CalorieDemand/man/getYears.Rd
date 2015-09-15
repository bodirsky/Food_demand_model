\name{getYears}
\alias{getYears}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Extraction of years}
\description{When used for demand objects, it gives the years in numbers from the attributes of the object.}
\usage{
getYears(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{2D-array with income values created by function demand_object}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{A vector of numerical values with the length of the time series in demand object x.}
\author{Susanne Rolinski, Benjamin Bodirsky}
%\note{}
%\seealso{}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
income <- as.array(matrix(c(30000,20000,60000),nrow=3,ncol=5))
dimnames(income)[[2]] <- paste("y",seq(1950,2050,length=5),sep="")
yy <- getYears(income)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
