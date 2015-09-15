\name{calibrate_it}
\alias{calibrate_it}
\title{Calibration of projected time series}
\description{Calibration of projected time series to data-derived values in a certain year}
\usage{
calibrate_it(origin, cal_to, cal_type = "convergence", 
  cal_year = NULL, end_year = NULL, 
  report_calibration_factors = FALSE)
}
\arguments{
  \item{origin}{2D-array with projected demand values of type demand object created by projection function}
  \item{cal_to}{Vector of values to which projections are calibrated of length nrow(x)}
  \item{cal_type}{Functional description of transition between calibration value and projection with choice between ('none', default='convergence', growth_rate') (see details)}
  \item{cal_year}{Year to which calibration is desired}
  \item{end_year}{Year until which projections are calibrated so that projected values are reached}
  \item{report_calibration_factors}{When TRUE, calculated calibration values are printed}
}
\details{The function determines calibration factors that ensure identity in the year of calibration and reach the projected value at the end of the calibration period. Methods to calculate the transition between both include
'none': no calibration is applied and the calibration factors remain 1.
'convergence': linear convergene between calibration data and projection using function convergence.
'growth_rate': uses the relation of calibrated values to original values in the calibration year as factor for all years.
The determined calibration factors are multiplied with the projections.}
\value{An object of structure x with calorie demand values.}
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
