#' Create a named map
#'
#' @export
#' @importFrom httr status_code content
#' @return response from POST request
create_named_map <- function(name, layers, user, api_key, overwrite=FALSE, ...) {
    if (overwrite & (name %in% list_named_maps(user=user, api_key=api_key, ...))) {
        delete_named_map(name, user=user, api_key=api_key, ...)
    }
    #TODO check that name starts with letter or number and only contains 
    #letters, numbers, dashes, and underscores
    body <- list(version="0.0.1", name=name, layergroup=list(version="1.3.0", layers=layers))
    r <- cdb_api_post(user=user, endpoint='map/named', body=body,
                      query=list(api_key=api_key), ...)
    if (status_code(r) != 200) stop('problem instantiating named map:\n',  r)
    #id <- fromJSON(content(r, as="text"))$layergroupid
    return(r)
}

#' Instantiate a named map
#'
#' @export
#' @importFrom httr status_code content
#' @return response from POST request
inst_named_map <- function(name, params=list(), user, api_key, ...) {
    #TODO check that name starts with letter or number and only contains 
    #letters, numbers, dashes, and underscores
    r <- cdb_api_post(user=user, endpoint=paste0('map/named/', name), 
                      body=params,
                      query=list(api_key=api_key), ...)
    if (status_code(r) != 200) stop('problem instantiating named map:\n',  r)
    id <- fromJSON(content(r, as="text"))$layergroupid
    return(id)
}

#' Instantiate an anonymous map
#'
#' @export
#' @importFrom httr status_code content
#' @return response from POST request
create_anon_map <- function(layers, user, api_key, raster=FALSE, ...) {
    body <- list(version="1.3.0", layers=layers)
    r <- cdb_api_post(user=user, endpoint='map', body=body, ...)
    if (status_code(r) != 200) stop('problem instantiating anonymous map:\n', r)
    id <- fromJSON(content(r, as="text"))$layergroupid
    return(id)
}

#' Delete a named map
#'
#' @export
#' @return Response from DELETE request
delete_named_map <- function(name, user, api_key, ...) {
    cdb_api_delete(user=user, endpoint=paste0('map/named/', name), 
                   api_key=api_key, ...)
}

#' List named maps for a given CartoDB account
#'
#' @export
#' @importFrom httr content
#' @return Response from GET request
list_named_maps <- function(user, api_key, ...) {
    r <- cdb_api_get(user, 'map/named/', query=list(api_key=api_key), ...)
    fromJSON(content(r, as="text"))$template_ids
}

#' Setup mapconfig for a raster layer
#'
#' @export
#' @return response from POST request
mapconfig_raster <- function(sql, cartocss, interactivity=NA, raster_band=1, 
                             type="cartodb", cartocss_version="2.3.0") {
    layers <- data.frame(type=type)
    options <- data.frame(sql=sql, cartocss=cartocss, 
                          interactivity=interactivity, 
                          cartocss_version=cartocss_version, raster=TRUE, 
                          geom_type='raster', 
                          geom_column="the_raster_webmercator", 
                          raster_band=raster_band)
    layers$options <- options
    return(layers)
}
