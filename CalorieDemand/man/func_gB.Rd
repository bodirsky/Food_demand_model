\name{func_gB}
\alias{func_gB}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Time-dependent model for total calorie demand fitting SRES scenario family B.}
\description{Calculates total calorie demand dependent on income and time using a power function.}
\usage{
func_gB(x, p = c(pa1 = 387.473708, pa2 = 9.774726, pa3 = 933.888522, 
  pb1 = 0.008445119, pb2 = -0.755692561, pb3 = 0.08940805))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{2D-array with income values created by function demand_object}
  \item{p}{Parameter vector with 6 entries; three of each used for an asymptotic Michaelis-Menten-type function (see details).}
}
\details{
The formulation for the power function used here is
gB(I; t) = m_1(t) * I^{m_2(t)},
where m is an asymptotic function of Michaelis-Menten type with intercept p_1, asymptotic value p_2, half-saturation constant p_3 and base year t_1 = 1960
m(t) = m_1 + m_2 (t - t_1)/(t-t_1+m_3).
Using this asymptotic function results in rapidly changing values during the first period and rather less increase in the last period.
}
\value{An object of structure x with calorie demand values.}
\references{Bodirsky, B.L., S. Rolinski, A. Biewald, I. Weindl, A. Popp and H. Lotze-Campen (2015): 'Global food demand scenarios for the 21st century', PlosOne, doi:10.1371/journal.pone.0139201.}
\author{Susanne Rolinski, Benjamin Bodirsky}
%\note{}
%\seealso{}
\examples{
income <- as.array(matrix(c(30000,20000,60000),nrow=3,ncol=5))
dimnames(income)[[2]] <- paste("y",seq(1950,2050,length=5),sep="")
cal <- func_gB(income)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
