\name{func_g0}
\alias{func_g0}
\title{Time-independent Engel curve}
\description{Calculates total calorie demand only dependent on income using a power function.}
\usage{
func_g0(x, p = c(pa = 7.074079, pb = 0.099321))
}
\arguments{
  \item{x}{Any object with income values}
  \item{p}{Parameter vector with 2 entries}
}
\details{The function determines total calorie demand from income by using a power function e^{pa}*x^{pb}. Parameters can be estimated by linearisation of the Engel curve and using a linear regression for fitting log(demand) to income.}
\value{An object of structure x with calorie demand values.}
\references{Bodirsky, B.L., S. Rolinski, A. Biewald, I. Weindl, A. Popp and H. Lotze-Campen (2015): 'Global food demand scenarios for the 21st century', PlosOne, doi:10.1371/journal.pone.0139201.}
\author{Susanne Rolinski, Benjamin Bodirsky}
%\seealso{}
\examples{
income <- seq(0,60000,by=100)
cal <- func_g0(income) 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
