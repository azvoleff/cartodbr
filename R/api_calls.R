build_cdb_url <- function(user, endpoint, domain, version, protocol,
                          subdomain) {
    if (subdomain) {
        paste0(protocol, '://', user, '.', domain, '/api/', version, '/', 
               endpoint)
    } else {
        paste0(protocol, '://', domain, '/user/', user, '/api/', version, '/', 
               endpoint)
    }
}

build_cdb_query <- function(query='', api_key='') {
    query_list <- list()
    if (query != '') query_list['query'] <- query
    if (api_key != '') query_list['api_key'] <- api_key
    return(query_list)
}

#' Make a HTTP GET call to the CartoDB map API
#'
#' @importFrom httr GET
cdb_api_get <- function(user, endpoint, domain='cartodb.com', 
                        query=list(), version='v1', protocol='https', 
                        subdomain=TRUE, ...) {
    stopifnot(protocol %in% c('https', 'http'))
    GET(build_cdb_url(user, endpoint, domain, version, protocol, subdomain), 
        query=query, ...)
}

#' Make a PUT call to the CartoDB map API
#'
#' @importFrom httr PUT
cdb_api_put <- function(user, endpoint, domain='cartodb.com', 
                        api_key='', version='v1', protocol='https', 
                        subdomain=TRUE, encode='json', ...) {
    stopifnot(protocol %in% c('https', 'http'))
    PUT(build_cdb_url(user, endpoint, domain, version, protocol, subdomain), 
        body=body, encode=encode, query=build_cdb_query(api_key=api_key), ...)
}

#' Make a POST call to the CartoDB map API
#'
#' @importFrom httr POST
cdb_api_post <- function(user, endpoint, body, domain='cartodb.com', 
                         query=list(), version='v1', protocol='https', 
                         subdomain=TRUE, encode='json', ...) {
    stopifnot(protocol %in% c('https', 'http'))
    POST(build_cdb_url(user, endpoint, domain, version, protocol, subdomain), 
         body=body, encode=encode, query=query, ...)
}

#' Make a POST call to the CartoDB map API
#'
#' @importFrom httr DELETE
cdb_api_delete <- function(user, endpoint, domain='cartodb.com', 
                           api_key='', version='v1', protocol='https', 
                           subdomain=TRUE, encode='json', ...) {
    stopifnot(protocol %in% c('https', 'http'))
    DELETE(build_cdb_url(user, endpoint, domain, version, protocol, subdomain), 
           query=list(api_key=api_key), ...)
}
