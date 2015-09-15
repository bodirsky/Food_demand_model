\name{CalorieDemand-package}
\alias{CalorieDemand-package}
\alias{CalorieDemand}
\docType{package}
\title{Calorie Demand Scenario Library}
\description{
The package provides functions for calorie demand projections which depend on income and time and includes useful functions to apply these to population, income and food demand data.

The main function of the package is demand_calculation(), which calculates food demand projections based on a selection of assumptions that can be specified in the parameter list.
Historical data and future projections are included in the data object demand_input.
All countries are identified according to their 3-digit ISO 3166-1 alpha-3 code.
All other functions and data objects are auxiliary to the function demand_calculation().
The functional relations between income, time and food demand are incorporated into the functions func_gA(), func_gB(), func_hA, and func_hB.
Calibration takes place through function calibrate_it().
Aggregation to world regions is derived with regAggregate(), and makes use of an assignment of countries to regions specified in data object iso_reg.
}
\details{
\tabular{ll}{
Package: \tab CalorieDemand\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-03-09\cr
License: \tab CC BY-NC-SA 4.0\cr
}
%~~ An overview of how to use the package, including the most important functions ~~
}
\author{Susanne Rolinski and Benjamin L. Bodirsky

Maintainer: Susanne Rolinski <Susanne.Rolinski@pik-potsdam.de>}
\references{Bodirsky, B.L., S. Rolinski, A. Biewald, I. Weindl, A. Popp and H. Lotze-Campen (2015): 'Global food demand scenarios for the 21st century', PlosOne, doi:10.1371/journal.pone.0139201.}
\keyword{ package }
\seealso{
%~~ Optional links to other man pages, e.g. ~~
%~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
## demand_calculation()

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
