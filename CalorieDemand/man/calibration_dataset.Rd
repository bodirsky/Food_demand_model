\name{calibration_dataset}
\alias{calibration_dataset}
\title{Extraction of calibration data for the specified year}
\description{From the calibration data object x, data are chosen for the specified year.}
\usage{
calibration_dataset(x, year, names = c(tcname, acname, fcname))
}
\arguments{
  \item{x}{3D-array with data}
  \item{year}{Year from which data are to be chosen}
  \item{names}{Specifying data variables which have to be in the attribute dimnames(x)[[3]]}
}
\details{By specifying the names, a subset of x is chosen by function demand_data and from these the vector from the chosen year is extracted. 
}
\value{Data.frame with columns names for the chosen year.}
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
