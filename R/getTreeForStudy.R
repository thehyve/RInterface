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

getTreeForStudy <- function(study.name, addLowDimensionalData = FALSE, addHighDimensionalData = FALSE, orderOfProjections = "all_data") {
    .checkTransmartConnection()
    
    # Get all concepts for this study and split the concept-paths by nodes
    concepts <- getConcepts(study.name)
    concepts <- concepts[order(concepts[ , 2]), ]
    conceptNodeNames <- strsplit(concepts[ , 2], "\\\\")
    
    # Each concept's nodes needs to be put into a data-frame.
    # The number of columns need to be equal to the max number of concepts in a path
    noOfColumns <- max(unlist(lapply(conceptNodeNames, length)))
    
    # Pad each list-element with empty elements up to the number of columns.
    conceptNodeNames <- lapply(conceptNodeNames, function(x) { x <- c(x, rep("", noOfColumns - length(x))) })
    # Prune the first nodes up to the studyname
    pruneTopNoNodes = match(toupper(study.name), toupper(conceptNodeNames[[1]]))
    conceptNodeNames <- t(as.data.frame(conceptNodeNames, stringsAsFactors = FALSE))[, -c(0:pruneTopNoNodes)]    
    
    # Construct the study's tree
    studyTree <- .addConceptsToTree(conceptNodeNames, concepts, addLowDimensionalData, addHighDimensionalData, orderOfProjections)
    studyTree
}

.addConceptsToTree <- function(conceptNodeNames, concepts, addLowDimensionalData, addHighDimensionalData, orderOfProjections) {
    # this function is used recursively. End-points of recursion are when input is empty.
    if (is.null(dim(conceptNodeNames)) || all(conceptNodeNames=="")) { return(NULL) }
    newTree <- list()
    # Creating a single layer of the tree in this iteration, with one element for each of the unique concept node names.
    nodesToAdd <- unique(conceptNodeNames[ , 1])
    # Do not add empty node names.
    nodesToAdd <- nodesToAdd[nodesToAdd != ""]
    # In addition to adding node names, we'll also be including all fetched metadata, for which we need to reserve a name.
    reservedNames <- c("metaData", "getObservations", "getHighdimData", "observationData", "highdimData")
    # Each element in a list must have a name that can be used for "auto completion".
    # Simplification of names can lead to naming collision, so guarentee uniqueness, including with the reserved named.
    cleanedNames <- make.unique(c(reservedNames, make.names(nodesToAdd)))[-c(1:length(reservedNames))]
    for (i in 1:length(nodesToAdd)) {
        # Create element in list for node
        newTree[[cleanedNames[i]]] <- list()
        # Which concepts belong to this node
        rowsToAddToNewChildNode <- which(conceptNodeNames[ , 1] == nodesToAdd[i])
        # Add these concepts to the subtree underneath this element
        newTree[[cleanedNames[i]]] <- .addConceptsToTree(conceptNodeNames[rowsToAddToNewChildNode, -1, drop = FALSE], concepts[rowsToAddToNewChildNode, , drop = FALSE], addLowDimensionalData, addHighDimensionalData, orderOfProjections)
        # Add metadata of this node
        metaData <- concepts[rowsToAddToNewChildNode[1], , drop = FALSE]
        newTree[[cleanedNames[i]]][[reservedNames[1]]] <- metaData
        # Add function call or data to node
        if ("api.link.observations.href" %in% colnames(metaData) && !is.na(metaData$api.link.observations.href)) {
            isLeaveNode <- length(rowsToAddToNewChildNode) == 1 && sum(conceptNodeNames[, ] != "") == 1
            if (addLowDimensionalData && isLeaveNode) {
                newTree[[cleanedNames[i]]][[reservedNames[4]]] <-  getObservations(concept.links = metaData$api.link.self.href)
            } else {
                newTree[[cleanedNames[i]]][[reservedNames[2]]] <- function(...) {getObservations(..., concept.links = metaData$api.link.self.href)}
            }
        } else if ("api.link.highdim.href" %in% colnames(metaData) && !is.na(metaData$api.link.highdim.href)) {
            if (addHighDimensionalData) {
                projectionOptions <- getHighdimData(concept.link = metaData$api.link.self.href)
                projectionToUse <- projectionOptions[!is.na(match(projectionOptions, orderOfProjections))][1]
                newTree[[cleanedNames[i]]][[reservedNames[5]]] <- getHighdimData(concept.link = metaData$api.link.self.href, projection = projectionToUse)
            } else {
                newTree[[cleanedNames[i]]][[reservedNames[3]]] <- function(...) {getHighdimData(..., concept.link = metaData$api.link.self.href)}
            }
        }
    }
    newTree
}

