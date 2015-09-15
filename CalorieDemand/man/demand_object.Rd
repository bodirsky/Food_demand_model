\name{demand_object}
\alias{demand_object}
\title{Conversion into array object}
\description{Specific conversion of 2D array into an object that can be used by the following functions for the demand projections.}
\usage{
demand_object(x, country, years)
}
\arguments{
  \item{x}{2D array with the spatial resolution in first and the temporal dimension in the second dimension}
  \item{country}{three-letter iso code country names for the first dimension}
  \item{years}{years for names for the second dimension}
}
\details{Creates dimnames for both dimensions in order to create an object that can be used in the following colculation of the demand projections}
\value{demand object}
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
