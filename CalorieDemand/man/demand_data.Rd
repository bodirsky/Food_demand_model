\name{demand_data}
\alias{demand_data}
\title{Extracts data from the array x}
\description{For the provided example dataset 'demand_input' the data with a specified name are extracted and converted to a demand object}
\usage{
demand_data(x, dname)
}
\arguments{
  \item{x}{the demand input dataset, a 3D array with dimensions (countries, years, data) and annotated names}
  \item{dname}{specifies a name in the third dimension of x}
}
\details{The demand_input object is derived from a download from FAO and contains different data in the format of an 3D array. The dimensions have to be in the order spatial resolution (countries), temporal resolution (years) and data. From this object, the desired data are extracted and converted by demand_object to a structure that is used for the following functions}
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
