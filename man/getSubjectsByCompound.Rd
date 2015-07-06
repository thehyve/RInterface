\name{getSubjectsByCompound}
\alias{getSubjectsByCompound}
\title{
  Obtain a list of all subjects associated with a compound.
}
\description{
  This function will retrieve a list of all subjects and their personal information for those subjects who have a
  concept linked to the requested compound. This function is still in development, and is currently dependent on a
  direct database connection instead of the regular connection used by the other functions.
}
\usage{
getSubjectsByCompound(directDatabaseConnection, compoundIDs = NULL, study.names = NULL)
}

\arguments{
  \item{directDatabaseConnection}{A database connection created by RPostgreSQL::dbConnect}
  \item{compoundIDs}{An array of character strings with compound IDs}
  \item{study.names}{optional. An array of character strings with study names to limit the search}
}

\details{ The function will return the subjectIDs and some phenotypical information for each subject that is linked to the requested compound.}

\value{
  A data.frame (or list) is returned, whereby each row (or list element) contains the information partaining to
  one subject.
}
\references{}
\author{Tim Dorscheidt, Jan Kanis, Rianne Jansen. 
Contact: development@thehyve.nl}
\note{To be able to access a transmart database, you need to be connected to the server the database is on. If you haven't connected to the server yet, establish a connection using the \code{\link{connectToTransmart}} function. In addition you will need a direct connection to the database.}
\seealso{\code{\link{getStudies}}}
\examples{
\dontrun{
    # The following will retrieve a data.frame with subject-information for the study "foo"
    getSubjectsByCompound(dbConnection, "c1")
    }
}

\keyword{ database }
\keyword{ transmart }
