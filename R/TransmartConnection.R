# Copyright 2015 The Hyve B.V..
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

TransmartConnection <- setRefClass('TransmartConnection', fields=c(
    'transmartDomain', 'oauthDomain', 'databaseUrl', 'clientId', 'clientSecret',
    'accessToken', 'accessTokenTimestamp', 'tokenExpiresIn', 'refreshToken',
    'oauthInfo', 'failed'
    ),
    methods=list(
        initialize=function(transmartDomain, oauthDomain = transmartDomain, databaseUrl = transmartDomain,
                            clientId = 'api-client', clientSecret = 'api-client',
                            token = NULL, .access.token = NULL) {
            transmartDomain <<- transmartDomain
            oauthDomain <<- oauthDomain
            databaseUrl <<- databaseUrl
            clientId <<- clientId
            clientSecret <<- clientSecret
            accessToken <<- .access.token
            accessTokenTimestamp <<- NULL
            tokenExpiresIn <<- NULL
            refreshToken <<- token
            oauthInfo <<- NULL
            failed <<- FALSE
        },
        
         connect = connect,
         authenticateWithTransmart = authenticateWithTransmart,
         getToken = getTransmartToken,
         .updateFromResponse = .updateFromResponse,
         .refreshToken = .refreshToken,
         .transmartServerGetOauthRequest = .transmartServerGetOauthRequest,
         ensureAlive = ensureAlive,
         isAlive = isAlive,
        .transmartGetJSON = .transmartGetJSON
        
    )
)
