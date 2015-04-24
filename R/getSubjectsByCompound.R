# Copyright 2014 Janssen Research & Development, LLC.
#
# This file is part of tranSMART R Client: R package allowing access to
# tranSMART's data via its RESTful API.
#
# This program is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation, either version 3 of the License, or (at your
# option) any later version, along with the following terms:
#
#   1. You may convey a work based on this program in accordance with
#      section 5, provided that you retain the above notices.
#   2. You may convey verbatim copies of this program code as you receive
#      it, in any medium, provided that you retain the above notices.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
# Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program. If not, see <http://www.gnu.org/licenses/>.

getSubjectsByCompound <- function(directDatabaseConnection, compoundIDs = NULL, study.names = NULL) {
    .checkTransmartConnection()
    
    compounds <- .getMetadataTagsDirectlyFromDB(directDatabaseConnection)
    compoundMatches <- na.omit(match(compoundIDs, compounds$tag))
    if (is.null(study.names)) study.names <- getStudies()$id
    compoundConceptLinks <- c()
    for (study.name in study.names) {
        studyConcepts <- getConcepts(study.name, cull.columns = F)
        result <- studyConcepts$api.link.self.href[match(compounds$path[compoundMatches], studyConcepts$fullName)]
        if (length(result) > 0 && !is.null(result) && !is.na(result)) compoundConceptLinks <- c(compoundConceptLinks, result)
    }
        
    .getSubjectsByConceptLinks(compoundConceptLinks)
}

.getSubjectsByConceptLinks <- function(concept.links) {
    allSubjectsFound <- data.frame()
    for (link in concept.links) {
        serverResult <- .transmartServerGetRequest( paste(link,"/subjects", sep=""), accept.type = "hal")
        listOfSubjects <- serverResult$subjects
        subjectIDs <- sapply(listOfSubjects, FUN = function(x) { x[["id"]] })
        names(listOfSubjects) <- paste("id",subjectIDs,sep="")
        allSubjectsFound <- rbind(allSubjectsFound, .listToDataFrame(listOfSubjects))
    }
    allSubjectsFound
}

.getMetadataTagsDirectlyFromDB <- function(dbConnection) {
    compoundTagType <- "Compound collaborator ID"
    compoundTags <- dbGetQuery(dbConnection, paste(sep = "","
            select * from i2b2metadata.i2b2_tags where tag_type like '", compoundTagType, "';"))
    compoundTags
}



