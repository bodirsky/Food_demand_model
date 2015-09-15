\name{func_hA}
\alias{func_hA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Time-dependent model for the livestock share of the total calorie demand fitting SRES scenario family A.}
\description{Calculates the share of animal-based calories in the total calorie demand dependent on income and time using a multilinear regression.}
\usage{
func_hA(x, p = c(pa = -36.732779363, pb = 4.497483702, 
  pc = 0.016039027, pd = -0.002077227))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{2D-array with income values created by function demand_object}
  \item{p}{Parameter vector with 4 entries (see details).}
}
\details{The function determines the share of livestock share of the total calorie demand from income and time by a mulltilinear regression model.
Parameters can be estimated by fitting a linear model to 
hA(I; t) = exp(pa + pb * ln(I) + pc t + pd ln(I) t).}
\value{An object of structure x with calorie demand values.}
\references{Bodirsky, B.L., S. Rolinski, A. Biewald, I. Weindl, A. Popp and H. Lotze-Campen (2015): 'Global food demand scenarios for the 21st century', PlosOne, doi:10.1371/journal.pone.0139201.}
\author{Susanne Rolinski, Benjamin Bodirsky}
%\note{}
%\seealso{}
\examples{
income <- as.array(matrix(c(30000,20000,60000),nrow=3,ncol=5))
dimnames(income)[[2]] <- paste("y",seq(1950,2050,length=5),sep="")
cal <- func_hA(income)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
