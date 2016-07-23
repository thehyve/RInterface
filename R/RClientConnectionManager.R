# Copyright 2014, 2015 The Hyve B.V.
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

connectToTransmart <- 
function (transmartDomain, use.authentication = TRUE, token = NULL, request.token = NULL, .access.token = NULL, ...) {
    if (!exists("defaultGlobalTransmartConnection") || defaultGlobalTransmartConnection$transmartDomain != transmartDomain) { 
        conn <- TransmartConnection(transmartDomain, token=token, .access.token=.access.token, ...)
        assign("defaultGlobalTransmartConnection", conn, envir = .GlobalEnv)
    } else {
        conn <- defaultGlobalTransmartConnection
    }

    if(conn$connect(use.authentication, request.token)) {
        conn
    } else {
        NULL
    }
}

connect <- 
function (use.authentication = TRUE, request.token=NULL) {
    if(isAlive(quiet=TRUE)) {
        message("Connection active")
        return(invisible(TRUE))
    }

    if (use.authentication && is.null(accessToken)) {
        authenticated <- authenticateWithTransmart(request.token)
        if(!authenticated) return(invisible(FALSE))
    } else if (!use.authentication && !is.null(accessToken)) {
        accessToken <<- NULL
    }

    if(!isAlive(retry=TRUE)) {
        stop("Connection unsuccessful. Type: ?connectToTransmart for help.")
    } else {
        message("Connection successful.")
        return(invisible(TRUE))
    }
}

getTransmartToken <- function() {
    refreshToken
}

.updateFromResponse <-
function(response) {
    oauthInfo <<- response

    map <- c(accessToken='access_token', tokenExpiresIn='expires_in', refreshToken='refresh_token')
    for(field in names(map)) {
        if(map[[field]] %in% names(response)) {
            .self$field(field, response[[map[field]]])
        }
    }
    if('access_token' %in% names(response)) {
        accessTokenTimestamp <<- Sys.time()        
    }
}

authenticateWithTransmart <- 
function(request.token=NULL) {
    oauth.request.token.url <- paste(sep = "",
            oauthDomain,
            "/oauth/authorize?response_type=code",
            "&client_id=", URLencode(clientId, TRUE),
            "&client_secret=", URLencode(clientSecret, TRUE),
            "&redirect_uri=", URLencode(oauthDomain, TRUE), URLencode("/oauth/verify", TRUE)
            )

    if (is.null(request.token)) {
        cat("Please visit the following url to authenticate this RClient ",
            "(enter nothing to cancel):\n\n",
            oauth.request.token.url, "\n\n",
            "And paste the verifier token here:\n")
        request.token <- readline() 
    }

    if (request.token == "") { 
        cat("Authentication cancelled\n")
        return(FALSE)
    }

    oauth.exchange.token.path <- "/oauth/token"
    post.body <- list(
        grant_type="authorization_code",
        code=request.token,
        redirect_uri=paste0(oauthDomain, "/oauth/verify"))

    oauthResponse <- .transmartServerPostOauthRequest(oauth.exchange.token.path, "Authentication", post.body)
    if (is.null(oauthResponse)) return(FALSE)

    .updateFromResponse(oauthResponse$content)
    cat("Authentication completed\n")
    return(TRUE)
}

.refreshToken <- function() {
    if (is.null(refreshToken)) {
        message("Unable to refresh the connection, no refresh token found")
        return(FALSE)
    }
    message("Trying to reauthenticate using the refresh token...")
    refreshPath <- "/oauth/token"
    post.body <- list(grant_type="refresh_token",
        refresh_token=refresh_token,
        redirect_uri=paste0(oauthDomain, "/oauth/verify"))

    oauthResponse <- .transmartServerPostOauthRequest(refreshPath, "Refreshing access", post.body)
    if (is.null(oauthResponse)) return(FALSE)
    if (!'access_token' %in% names(oauthResponse$content)) {
        message("Refreshing access failed, server response did not contain access_token. HTTP", statusString)
        return(FALSE)
    }
    .updateFromResponse(oauthResponse$content)
    return(TRUE)
}

.transmartServerPostOauthRequest <- function(path, action, post.body) {
    oauthResponse <- .transmartServerGetRequest(path, onlyContent=F, post.body=post.body)
    statusString <- paste("status code ", oauthResponse$status, ": ", oauthResponse$headers[['statusMessage']], sep='')
    if (!oauthResponse$JSON) {
        cat(action, " failed, could not parse server response of type ", oauthResponse$headers[['Content-Type']], ". ", statusString, "\n", sep='')
        return(NULL)
    }
    if ('error' %in% names(oauthResponse$content)) {
        cat(action, " failed: ", oauthResponse$content[['error_description']], "\n", sep='')
        return(NULL)
    }
    if (!oauthResponse$status == 200) {
        cat(action, "error: HTTP", statusString, "\n")
        return(NULL)
    }
    return(oauthResponse)
}

ensureAlive <- function() {return(isAlive(stop.on.error = TRUE))}

isAlive <- function(retry = FALSE, stop.on.error = FALSE, quiet = FALSE) {
    if(stop.on.error) {
        stopfn <- stop
    } else if(!quiet) {
        stopfn <- function(e) {message(e); return(FALSE)}
    } else {
        stopfn <- function(e) {return(FALSE)}
    }

    if(failed && !retry) {
        return(stopfn("Connection is dead. Call TransmartConnection$isAlive(retry=TRUE) to try again."))
    }
    
    failed <<- TRUE

    if (!is.null(accessToken)) {
        ping <- .transmartServerGetRequest("/oauth/inspectToken", accept.type = "default", onlyContent = F)
        if(ping$status == 404) {
            # Maybe we're talking to an older version of Transmart that uses the version 1 oauth plugin
            ping <- .transmartServerGetRequest("/oauth/verify", accept.type = "default", onlyContent = F)
        }
        if (getOption("verbose")) { message(paste(ping$content, collapse = ": ")) }

        if(ping$status == 200) {
            failed <<- FALSE
            return(TRUE)
        }

        if(!'error' %in% names(ping$content)) {
            return(stopfn(paste("HTTP ", ping$status, ": ", ping$statusMessage, sep='')))
        }
        if(ping$status != 401 || ping$content[['error']] != "invalid_token") {
            return(stopfn(paste("HTTP ", ping$status, ": ", ping$statusMessage, "\n", ping$content[['error']],  ": ", ping$content[['error_description']], sep='')))
        }
    } else if (is.null(refreshToken)) {
        return(stopfn("Unable to refresh authentication: no refresh token"))
    }

    # try to refresh authentication
    if (.refreshToken()) {
        failed <<- FALSE
        message("Access token refreshed.")
        return(TRUE)
    } else {
        # failed <<- TRUE already set
        return(stopfn("Refreshing access failed. "))
        stopfn("Refreshing access failed, connection is dead. Call TransmartConnection$connect() to re-establish the connection.")
    }
}

.requestErrorHandler <- function(e, result=NULL) {
    message("Sorry, the R client encountered the following error: ", e,
            "\n\nPlease make sure that the transmart server is still running. ",
            "If the server is not down, you may have encountered a bug.\n",
            "You can help fix it by contacting us. Type ?transmartRClient for contact details.\n", 
            "Optional: type options(verbose = TRUE) and replicate the bug to find out more details.")
    # If e is a condition adding the call. parameter triggers another warning
    if(inherits(e, "condition")) {
        stop(e)
    } else {
        stop(e, call.=FALSE)
    }
}

.transmartGetJSON <- function(apiCall, ...) { .transmartServerGetRequest(apiCall, ensureJSON = TRUE, accept.type = "hal", ...) }

# If you just want a result, use the default parameters. If you want to do your own error handling, call with
# onlyContent = NULL, this will return a list with data, headers and status code.
.transmartServerGetRequest <- function(apiCall, errorHandler = .requestErrorHandler, onlyContent = c(200),
        ensureJSON = FALSE, ...)  {
    if (!is.null(accessToken)) {
        httpHeaderFields <- c(Authorization = paste("Bearer", accessToken))
    } else { httpHeaderFields <- character(0) }

    tryCatch(result <- .serverMessageExchange(apiCall, httpHeaderFields, ...), error = errorHandler)
    if(!exists("result")) { return(NULL) }
    if(is.numeric(onlyContent)) {
        errmsg <- ''
        if(result$JSON && 'error' %in% names(result$content)) {
            errmsg <- paste(":", result$content[['error']])
            if('error_description' %in% names(result$content)) {
                errmsg <- paste(errmsg, ": ", result$content[['error_description']], sep='')
            }
        }
        if(!result$status %in% onlyContent) {
            errmsg <- paste("HTTP", result$status, result$statusMessage, "(expected result code(s):", toString(onlyContent), ")")
            if(result$JSON && 'error' %in% names(result$content)) {
                errmsg <- paste(errmsg, ": ", result$content[['error']], sep='')
                if('error_description' %in% names(result$content)) {
                    errmsg <- paste(errmsg, ": ", result$content[['error_description']], sep='')
                }
            }
            return(errorHandler(errmsg, result))
        }
        if(ensureJSON && !result$JSON) {
            return(errorHandler(paste("No JSON returned but", result$headers[['Content-Type']]), result))
        }
        return(result$content)
    }
    result
}

# plain function
.contentType <- function(headers) {
    if(! 'content-type' %in% names(headers)) {
        return('content-type header not found')
    }
    h <- headers[['content-type']]
    if(grepl("^application/json(;|\\W|$)", h)) {
        return('json')
    }
    if(grepl("^application/hal\\+json(;|\\W|$)", h)) {
        return('hal')
    }
    if(grepl("^text/html(;|\\W|$)", h)) {
        return('html')
    }
    return('unknown')
}

# plain function
.serverMessageExchange <- 
function(apiCall, httpHeaderFields, accept.type = "default", post.body = NULL, show.progress = (accept.type == 'binary') ) {
    if (any(accept.type == c("default", "hal"))) {
        if (accept.type == "hal") {
            httpHeaderFields <- c(httpHeaderFields, Accept = "application/hal+json;charset=UTF-8")
        }
        result <- list(JSON = FALSE)
        api.url <- paste0(databaseUrl, apiCall)
        if (is.null(post.body)) {
            req <- GET(api.url,
                       add_headers(httpHeaderFields),
                       authenticate(clientId, clientSecret),
                       config(verbose = getOption("verbose")))
        } else {
            req <- POST(api.url,
                        body = post.body,
                        add_headers(httpHeaderFields),
                        authenticate(clientId, clientSecret),
                        encode='form',
                        config(verbose = getOption("verbose")))
            if (getOption("verbose")) { message("POST body:\n", .list2string(post.body), "\n") }
        }
        result$content <- content(req, "text")
        if (getOption("verbose")) { message("Server response:\n", result$content, "\n") }
        result$headers <- headers(req)
        result$status <- req$status_code
        result$statusMessage <- http_status(req)$message
    	switch(.contentType(result$headers),
               json = {
                   result$content <- fromJSON(result$content)
                   result$JSON <- TRUE
               },
               hal = {
                   result$content <- .simplifyHalList(fromJSON(result$content))
                   result$JSON <- TRUE
               })
        return(result)
    } else if (accept.type == "binary") {
        if(show.progress) cat("Retrieving data...\n")
        result <- list(JSON = FALSE)
        api.url <- paste(sep="", databaseUrl, apiCall)
        if (is.null(post.body)) {
            req <- GET(api.url,
                       add_headers(httpHeaderFields),
                       authenticate(clientId, clientSecret),
                       if(show.progress) progress(),
                       config(verbose = getOption("verbose")))
        } else {
            req <- POST(api.url,
                        body = post.body,
                        add_headers(httpHeaderFields),
                        authenticate(clientId, clientSecret),
                        if(show.progress) progress(),
                        encode='form',
                        config(verbose = getOption("verbose")))
        }
        if(show.progress) cat("\nDownload complete.\n")
        result$content <- content(req, "raw")
        result$headers <- headers(req)
        result$status <- req$status_code
        result$statusMessage <- http_status(req)$message
        # even though we asked for binary the server may have returned something else
        # (e.g. an error message)
        if (getOption("verbose") && .contentType(result$headers) %in% c('json', 'hal', 'html')) {
        	message("Server response:\n", result$content, "\n")
        }
    	return(result)
    }
    return(NULL)
}


# plain function
.listToDataFrame <- function(l) {
    # TODO: (timdo) dependency on 'plyr' package removed; figure out whether dependency is present elsewhere, or remove dependency
    # add each list-element as a new row to a matrix, in two passes
    # first pass: go through each list element, unlist it and remember future column names
    columnNames <- c()
    if (length(l) > 0) {
        for (i in 1:(length(l))) {
            l[[i]] <- unlist(l[[i]])
            columnNames <- union(columnNames, names(l[[i]]))
        }
    }
    
    # second pass: go through each list element and add its elements to correct column
    df <- matrix(nrow = length(l), ncol = length(columnNames))
    if (length(l) > 0) {
        for (i in 1:(length(l))) {
            df[i, match(names(l[[i]]), columnNames)] <- l[[i]]
        }
    }
    colnames(df) <- columnNames

    # check whether list contains valid row names, and if true; use them
    if (length(l) < 1 || is.null(names(l)) || is.na(names(l)) || length(names(l)) != length(l)) { 
        rownames(df) <- NULL
    } else { rownames(df) <- names(l) }
    # convert matrix to data.frame
    as.data.frame(df, stringsAsFactors = FALSE)
}

# this function is needed for .listToDataFrame to recursively replace NULL
# values with NA, otherwise, unlist() will exclude those values.
# plain function
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

# plain function
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

# plain function
.list2string <- function(lst) {
    if(is.null(names(lst))) return(paste(lst, sep=", "))

    final <- character(length(lst)*2)
    paste(mapply(function(name, val) {paste0(name, ': "', encodeString(val), '"')}, names(lst), lst), collapse=", ")
}
