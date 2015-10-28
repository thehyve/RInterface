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

getCompoundConcepts <- function(collaboratorId = NULL, compoundId = NULL, studies = NULL) {
  if (is.null(studies)) {
    studies <- getStudies()$id
  }
  browser()
  compoundConcepts <- NULL
  for (study in studies) {
    metaData <- .getMetadataTagsForStudy(study)
    LSPCompoundIdentifierTags <- c("Compound collaboratorId", "Compound compoundId")
    compoundIdentifierCols <- match(LSPCompoundIdentifierTags, colnames(metaData))
    browser()
    metaData <- metaData[ , c(1, compoundIdentifierCols), drop = FALSE]
    metaData <- metaData[!apply(is.na(metaData[ , -1, drop = FALSE]), 1, all),]
    if (all(!is.na(compoundIdentifierCols))) {
      if (is.null(collaboratorId)) { collaboratorIdMatches <- 1:nrow(metaData)
      } else { collaboratorIdMatches <- which(metaData[ , compoundIdentifierCols[1]] %in% collaboratorId) }
      if (is.null(compoundId)) { compoundIdMatches <- 1:nrow(metaData)
      } else { compoundIdMatches <- which(metaData[ , compoundIdentifierCols[2]] %in% compoundId) }
      new.compoundConcepts <- metaData[intersect(collaboratorIdMatches, compoundIdMatches), , drop = FALSE]
      compoundConcepts <- rbind(compoundConcepts, new.compoundConcepts)
    }
  }
  compoundConcepts
}

.getMetadataTagsForStudy <- function(study.name) {
  concepts <- getConcepts(study.name, cull.columns = F)
  metaDataCols <- grep("^metadata\\.", colnames(concepts))
  conceptLinkCol <- match("api.link.self.href", colnames(concepts))
  metaData <- concepts[ , c(conceptLinkCol, metaDataCols), drop = FALSE]
  colnames(metaData) <- gsub("^metadata\\.", "", colnames(metaData))
  # Keep only concepts with at least 1 metadata tag
  metaData <- metaData[apply(metaData[ , -1, drop = FALSE], 1, function(x) any(!is.na(x))), ]
  return(metaData)
}

getSubjectsByConceptLinks <- function(concept.links) {
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
