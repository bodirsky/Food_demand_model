\name{demand_calculation}
\alias{demand_calculation}
\title{Calculation of demand scenarios}
\description{Wrapper function for determining the demand scenarios for total and animal-based calories}
\usage{
demand_calculation(scenario_name, dat_scen = demand_input, 
  pop_scen = "pop_mio_ssp2", gdp_scen = "gdp_mioUSD05MER_ssp2", 
  dem_regr_type = func_gA, ls_regr_type = func_hA, 
  calib_year_start = "y1990", calib_year_end = "y2100", calib_type = "convergence", 
  dem_aim = NULL, dem_aim_type = "s", dem_aim_direction = "down", 
  dem_aim_startyear = "y2010", dem_aim_endyear = "y2100", 
  ls_aim = NULL, ls_aim_type = "s", ls_aim_direction = "up", 
  ls_aim_startyear = "y2010", ls_aim_endyear = "y2100")
}
\arguments{
  \item{scenario_name}{name of the chosen scenario}
  \item{dat_scen}{3D array including the necessary input and calibration data}
  \item{pop_scen}{name in dat_scen for the population scenario data}
  \item{gdp_scen}{name in dat_scen for the income scenario data}
  \item{dem_regr_type}{function for calculating total calorie demand}
  \item{ls_regr_type}{function for calculating the livestock share of the total calorie demand}
  \item{calib_year_start}{year at which calibration is desired}
  \item{calib_year_end}{year until which projections are calibrated, i.e. projected values are reached}
  \item{calib_type}{type of calibration used in function \code{\link{calibrate_it}}}
  \item{dem_aim}{optional value or vector of values for the total calorie demand that have to be met at the end of the projection period}
  \item{dem_aim_type}{type of transition to the specified aim used in function \code{\link{convergence}}}
  \item{dem_aim_direction}{optional declaration of calibration direction, 'up' meaning that the calibration year is at the beginning of the calibration period and 'down' meaning the opposite}
  \item{dem_aim_startyear}{year at which transition starts}
  \item{dem_aim_endyear}{year at which transition ends}
  \item{ls_aim}{optional value or vector of values for the livestock share of the total calorie demand that have to be met at the end of the projection period}
  \item{ls_aim_type}{type of transition to the specified aim used in function \code{\link{convergence}}}
  \item{ls_aim_direction}{optional declaration of calibration direction, 'up' meaning that the calibration year is at the beginning of the calibration period and 'down' meaning the opposite}
  \item{ls_aim_startyear}{year at which transition starts}
  \item{ls_aim_endyear}{year at which transition ends}
}
\details{This function is the main calculation wrapper for the calculation of the scenarios. By defining the dataset of input and calibration data, the regression functions for the projections and the calibration type as well as upper limits for projections, generation of projections can be controlled.}
\value{List of two threedimensional arrays (spatial unit, year, variable) with the variables
\item{dem}{total calorie demand (EJ/year)}
\item{l}{animal-based calorie demand (EJ/year)}
\item{ls}{share of animal-based calories of the total calorie demand (share)}
\item{kcal}{per-capita total calorie demand (kcal/capita/day)}
\item{pop}{population (Mio capita)}
\item{gdp}{income (Mio USD 05 in market exchange rates)}
\item{gdp_pc}{per-capita income (USD 05 in market exchange rates/capita)}

The spatial units is provided for two resolutions
\item{reg}{regionally aggregated values (see function \code{\link{regAggregate}})}
\item{country}{country-specific values}
}
\references{Bodirsky, B.L., S. Rolinski, A. Biewald, I. Weindl, A. Popp and H. Lotze-Campen (2015): 'Global food demand scenarios for the 21st century', PlosOne, doi:10.1371/journal.pone.0139201.}
\author{Susanne Rolinski, Benjamin Bodirsky}
%\note{}
\seealso{\code{\link{demand_input}}, \code{\link{calibrate_it}}}
\examples{
data(demand_input)
data(iso_reg) 

scenario_a1 <- demand_calculation(
  scenario_name = "a1",
  dat_scen=demand_input,
  pop_scen="pop_mio_a1",
  gdp_scen="gdp_mioUSD05MER_a1", 
  dem_regr_type=func_gA,
  ls_regr_type=func_hA
)    

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
