\name{func_gA}
\alias{func_gA}
\title{Time-dependent model for total calorie demand fitting SRES scenario family A.}
\description{Calculates total calorie demand dependent on income and time using a power function.}
\usage{
func_gA(x, p = c(pa1 = 2.8251159, pa2 = 0.0021313756, 
  pb1 = 0.1622219, pb2 = -3.12439e-05))
}
\arguments{
  \item{x}{2D-array with income values created by function demand_object}
  \item{p}{Parameter vector with 4 entries; factors pa1 and pb1 and elasticities pa2 and pb2.}
}
\details{The function determines total calorie demand from income by using a power function.
Parameters can be estimated by fitting a linear mixed model to the Engel curve using time as random effect and income as fixed effect.}
\value{An object of structure x with calorie demand values.}
\references{Bodirsky, B.L., S. Rolinski, A. Biewald, I. Weindl, A. Popp and H. Lotze-Campen (2015): 'Global food demand scenarios for the 21st century', PlosOne, doi:10.1371/journal.pone.0139201.}
\author{Susanne Rolinski, Benjamin Bodirsky}
%\seealso{}
\examples{
income <- as.array(matrix(c(30000,20000,60000),nrow=3,ncol=5))
dimnames(income)[[2]] <- paste("y",seq(1950,2050,length=5),sep="")
cal <- func_gA(income)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
