\name{func_hB}
\alias{func_hB}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Time-dependent model for the livestock share of the total calorie demand fitting SRES scenario family B.}
\description{Calculates the animal-based share of the total calorie demand dependent on income and time using a u-shaped function.}
\usage{
func_hB(x, p = c(pa1 = 0.01371507, pa2 = -5.295249e-06, 
  pb1 = -0.000110241, pb2 = 6.403996e-08))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{2D-array with income values created by function demand_object}
  \item{p}{Parameter vector with 4 entries; factors pa1 and pb1 and elasticities pa2 and pb2.}
}
\details{The function determines the livestock share of the total calorie demand from income by using a function which combines under-proportionally increasing values for low income with an exponential decline in the high-income ranges.
Parameters can be estimated by fitting to function
hB(I; t) = (pa1 + pa2 t) * I^{1/2} * exp(-(pb1 + pb2  t) I).}
\value{An object of structure x with calorie demand values.}
\references{Bodirsky, B.L., S. Rolinski, A. Biewald, I. Weindl, A. Popp and H. Lotze-Campen (2015): 'Global food demand scenarios for the 21st century', PlosOne, doi:10.1371/journal.pone.0139201.}
\author{Susanne Rolinski, Benjamin Bodirsky}

%\note{}
%\seealso{}
\examples{
income <- as.array(matrix(c(30000,20000,60000),nrow=3,ncol=5))
dimnames(income)[[2]] <- paste("y",seq(1950,2050,length=5),sep="")
cal <- func_hB(income)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
