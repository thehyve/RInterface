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

.checkTransmartConnection <- function() {
    ping <- .transmartServerGetRequest("/oauth/verify", accept.type = "default")
    if (getOption("verbose")) { message(paste(ping, collapse = ": ")) }

    if (!is.null(ping)) {
        message("Cannot connect to tranSMART database.")
        return(FALSE)
    }
    return(TRUE)
}

.transmartServerGetRequest <- function(apiUrl, apiCall, auth_token, ...)  {

  httpHeaderFields <- c(Authorization = paste("Bearer ", auth_token, sep=""))
  fullCall <- paste(apiUrl, apiCall, sep="")

  tryCatch(result <- .serverMessageExchange(fullCall, httpHeaderFields, ...),
          error = function(e) {
              message("Sorry, the R client was unable to carry out your request.",
                      "Please make sure that the transmart server is still running. \n\n",
                      "If the server is not down, you've encountered a bug.\n",
                      "You can help fix it by contacting us. Type ?transmartRClient for contact details.\n",
                      "Optional: type options(verbose = TRUE) and replicate the bug to find out more details.")
              stop(e)
          })
  result
}

.serverMessageExchange <-
function(apiCall, httpHeaderFields, accept.type = "default") {
    if (any(accept.type == c("default", "hal"))) {
        if (accept.type == "hal") { httpHeaderFields <- c(httpHeaderFields, accept = "application/hal+json") }
        result <- getURL(apiCall,
                verbose = getOption("verbose"),
                httpheader = httpHeaderFields)
        if (getOption("verbose")) { message("Server response:\n\n", result, "\n") }
        if (is.null(result) || result == "null") { return(NULL) }
        result <- fromJSON(result, asText = TRUE, nullValue = NA)
        if (accept.type == "hal") { return(.simplifyHalList(result)) }
        return(result)
    } else if (accept.type == "binary") {
        result <- list()
        h <- basicTextGatherer()
        result$content <- getBinaryURL(apiCall,
                .opts = list(headerfunction = h$update),
                httpheader = httpHeaderFields)
        result$header <- parseHTTPHeader(h$value())
        if (getOption("verbose")) {
            message(paste("Server binary response header:", as.character(data.frame(result$header)), "", sep="\n"))
        }
        return(result)
    }
    return(NULL)
}

.listToDataFrame <- function(list) {
    # TODO: (timdo) dependency on 'plyr' package removed; figure out whether dependency is present elsewhere, or remove dependency
    # add each list-element as a new row to a matrix, in two passes
    # first pass: go through each list element, unlist it and remember future column names
    columnNames <- c()
    for (i in 1:(length(list))) {
        list[[i]] <- unlist(list[[i]])
        columnNames <- union(columnNames, names(list[[i]]))
    }

    # second pass: go through each list element and add its elements to correct column
    df <- matrix(nrow = length(list), ncol = length(columnNames))
    for (i in 1:(length(list))) {
        df[i, match(names(list[[i]]), columnNames)] <- list[[i]]
    }
    colnames(df) <- columnNames

    # check whether list contains valid row names, and if true; use them
    if (is.null(names(list)) || is.na(names(list)) || length(names(list)) != length(list)) {
        rownames(df) <- NULL
    } else { rownames(df) <- names(list) }
    # convert matrix to data.frame
    as.data.frame(df, stringsAsFactors = FALSE)
}

# this function is needed for .listToDataFrame to recursively replace NULL
# values with NA, otherwise, unlist() will exclude those values.
.recursiveReplaceNullWithNa <- function(list) {
    if (length(list) == 0) return(list())
    for (i in 1:length(list)) {
        if (is.list(list[[i]])) {
            list[[i]] <- .recursiveReplaceNullWithNa(list[[i]])
        } else {
            if (is.null(list[[i]])) list[[i]] <- NA
        }
    }
    list
}

.simplifyHalList <- function(halList) {
    # rename _links element to api.link
    names(halList)[which(names(halList) == "_links")] <- "api.link"
    # remove embedded intermediate element and add its sub-elements to this level
    if ("_embedded" %in% names(halList)) {
        halList <- as.list(c(halList, halList[["_embedded"]]))
        halList[["_embedded"]] <- NULL
    }
    # recursion: apply this function to list-elements of current list
    if (length(halList) > 0) {
        for (elementIndex in 1:length(halList)) {
            if (is.list(halList[[elementIndex]])) {
                halList[[elementIndex]] <- .simplifyHalList(halList[[elementIndex]])
            }
        }
    }
    return(halList)
}
