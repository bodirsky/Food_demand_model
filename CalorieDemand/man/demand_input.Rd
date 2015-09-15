\name{demand_input}
\alias{demand_input}
\docType{data}
\title{Example input dataset}
\description{Array of potential input and calibration data for the calculation of demand projections}
\usage{data(demand_input)}
\format{The data are in the format of a 3D array with dimensions (countries, years, data). Countries have names according to convention ISO 3166-1 alpha-3 with three letters followed by '.n' with n in 1:dim[[1]], Years have names beginning with 'y' followed by 4 digit numbers. Data have names composed of data type ("gdp","pop","kcal","lvst"), unit (e.g."mio") and origin or scenario name (e.g. "a1", "fao").}
\details{The array demand_input includes input data for the projections (income and population) and potential calibration data for total and animal-based calorie demand}
\source{
FAOSTAT. 2009 Database Collection of the Food and Agriculture Organization of the United Nations. www.faostat.fao.org.
WORLDBANK. 2011. World Development Indicators. http://data.worldbank.org/data-catalog/world-development-indicators.
CIESIN. 2002a. Country-Level GDP and Downscaled Projections Based on the A1, A2, B1, and B2 Marker Scenarios, 1990-2100. http://www.ciesin.columbia.edu/datasets/downscaled.
CIESIN. 2002b. Country-Level Population and Downscaled Projections Based on the B2 Scenario, 1990-2100. http://www.ciesin.columbia.edu/datasets/downscaled.
}
\references{Bodirsky, B.L., S. Rolinski, A. Biewald, I. Weindl, A. Popp and H. Lotze-Campen (2015): 'Global food demand scenarios for the 21st century', PlosOne, doi:10.1371/journal.pone.0139201.}
\examples{
data(demand_input)
str(demand_input)
}
\keyword{datasets}
