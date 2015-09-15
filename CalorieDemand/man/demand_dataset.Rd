\name{demand_dataset}
\alias{demand_dataset}
\title{Extraction of input data}
\description{Extracts the necessary input data for the calculation of the demand projections}
\usage{
demand_dataset(xgdp, gdpname, xpop, popname)
}
\arguments{
  \item{xgdp}{dataset containing income projections}
  \item{gdpname}{name in dataset specifying income data}
  \item{xpop}{dataset containing population projections}
  \item{popname}{name in dataset specifying population data}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{List of 2D arrays of format demand object
\item{gdp }{projections of income}
\item{pop }{projections of population}
}
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
