\name{iso_reg}
\alias{iso_reg}
\docType{data}
\title{Data with region mapping}
\description{This dataframe contains a region name for each country. }
\usage{data(iso_reg)}
\format{A dataframe with at least 2 columns: "iso" gives names for each spatial unit used in the input data, "reg" gives a region name (here we use the 10 world regions of the model MAgPIE)}
\details{Countries are identified according to their 3-digit ISO 3166-1 alpha-3 code.}
\source{http://en.wikipedia.org/wiki/ISO_3166-1_alpha-3}
\references{Bodirsky, B.L., S. Rolinski, A. Biewald, I. Weindl, A. Popp and H. Lotze-Campen (2015): 'Global food demand scenarios for the 21st century', PlosOne, doi:10.1371/journal.pone.0139201.}
\examples{
data(iso_reg)
str(iso_reg)
}
\keyword{datasets}
