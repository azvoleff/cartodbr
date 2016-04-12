build_cdb_url <- function(endpoint, version) {
    if (Sys.getenv('cdbr_user') == '') {
        stop("Need to configure cartodb username in environment variable 'cdbr_user'")
    } else {
        user <- Sys.getenv('cdbr_user')
    }
    if (Sys.getenv('cdbr_domain') == '') {
        domain <- 'cartodb.com'
    } else {
        domain <- Sys.getenv('cdbr_domain')
    }

    if (Sys.getenv('cdbr_subdomain') == '') {
        subdomain <- TRUE
    } else {
        subdomain <- Sys.getenv('cdbr_subdomain')
        stopifnot(subdomain %in% c(TRUE, FALSE))
    }
    if (Sys.getenv('cdbr_protocol') == '') {
        protocol <- 'https'
    } else {
        protocol <- Sys.getenv('cdbr_protocol')
        stopifnot(protocol %in% c('http', 'https'))
    }

    if (subdomain) {
        paste0(protocol, '://', user, '.', domain, '/api/', version, '/', 
               endpoint)
    } else {
        paste0(protocol, '://', domain, '/user/', user, '/api/', version, '/', 
               endpoint)
    }
}

get_api_key <- function() {
    if (Sys.getenv('cdbr_api_key') == '') {
        stop("Need to configure cartodb api key in environment variable 'cdbr_api_key'")
    } else {
        Sys.getenv('cdbr_api_key')
    }
}

#' Make a HTTP GET call to the CartoDB map API
#'
#' @importFrom httr GET
cdb_api_get <- function(endpoint, version='v1', query=list(), ...) {
    GET(build_cdb_url(endpoint, version), query=query, ...)
}

#' Make a PUT call to the CartoDB map API
#'
#' @importFrom httr PUT
cdb_api_put <- function(endpoint, version='v1', encode='json', ...) {
    PUT(build_cdb_url(endpoint, version), body=body, encode=encode, 
        query=list(api_key=get_api_key()), ...)
}

#' Make a POST call to the CartoDB map API
#'
#' @importFrom httr POST
cdb_api_post <- function(endpoint, version='v1', body=NULL, query=NULL,
                         encode='json', ...) {
    POST(build_cdb_url(endpoint, version), body=body, query=query, 
         encode=encode, ...)
}

#' Make a POST call to the CartoDB map API
#'
#' @importFrom httr DELETE
cdb_api_delete <- function(endpoint, version='v1', encode='json', ...) {
    DELETE(build_cdb_url(endpoint, version), 
           query=list(api_key=get_api_key()), ...)
}
