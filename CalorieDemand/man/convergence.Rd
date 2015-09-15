\name{convergence}
\alias{convergence}
\title{Transition form from data to projections}
\description{Functional form of transition between calibration data and projections}
\usage{
convergence(origin, aim, start_year = NULL, end_year = NULL, 
  direction = NULL, type = "smooth")
}
\arguments{
  \item{origin}{2D array of type demand_object}
  \item{aim}{either a scalar or a vector with calibration target values}
  \item{start_year}{year of calibration target}
  \item{end_year}{year in which projections are to be met}
  \item{direction}{optional declaration of calibration direction, 'up' meaning that the calibration year is at the beginning of the calibration period and 'down' meaning the opposite}
  \item{type}{describes transition between calibrated and projected values, default='smooth' uses a cubic formula, 'linear' a linear formula and 's' a sigmoid formula}
}
\details{For the calibration of projected demand values to data, the form of the transition can be chosen. The function determines the necessary calibration factor that ensures identity with the data at the chosen calibration year and with the projections at the end of the calibration period.}
\value{An object of structure x with calorie demand values.}
\references{Bodirsky, B.L., S. Rolinski, A. Biewald, I. Weindl, A. Popp and H. Lotze-Campen (2015): 'Global food demand scenarios for the 21st century', PlosOne, doi:10.1371/journal.pone.0139201.}
\author{Susanne Rolinski, Benjamin Bodirsky}
%\note{}
%\seealso{}
\examples{
##---- Should be DIRECTLY executable !! ----
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
