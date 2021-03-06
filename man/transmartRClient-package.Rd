\name{transmartRClient-package}
\alias{transmartRClient-package}
\alias{transmartRClient}
\docType{package}
\title{
R Client for accessing the tranSMART RESTful API
}
\description{
This package contains functions that enable a user to authenticate and access
a tranSMART database. Data can be retrieved from this database via get-functions, which access tranSMART's
RESTful API. The R client currently allows a user to retrieve both clinical
and high dimensional data (to which the user is authorized to access).
}
\details{

The following functions are available in the package:
  \tabular{l}{
  \code{\link{connectToTransmart}}\cr
  \code{\link{getStudies}}\cr
  \code{\link{getSubjects}}\cr
  \code{\link{getConcepts}}\cr
  \code{\link{getObservations}}\cr
  \code{\link{getHighdimData}}\cr
  \code{\link{highdimInfo}}\cr
  \code{\link{getPatientSet}}\cr
}
}

\author{Tim Dorscheidt, Jan Kanis, Rianne Jansen. 
Contact: development@thehyve.nl}
\references{
}
\keyword{ package }
\keyword{ transmart }
\keyword{ RClient }
