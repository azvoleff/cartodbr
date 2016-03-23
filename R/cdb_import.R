#' Import file into CartoDB
#'
#' Import a file into a CartoDB database.
#'
#' @export
#' @importFrom httr POST
#' @return Response from POST request
cdb_import <- function(url, api_key, server='cartodb.com') {
}

#' Import file into CartoDB as a synced table
#'
#' @export
#' @return Response from POST request
cdb_synctable_import <- function(data_url, user, api_key,
                                 sync_interval=604800) {
    cdb_api_post(user=user, endpoint='synchronizations', 
                 body=list(url=data_url, interval=sync_interval), 
                 query(api_key=api_key))
}

#' Convert sync table to regular table (remove sync)
#'
#' @export
#' @return Response from POST request
cdb_synctable_convert <- function(import_id, user, api_key, ...) {
    cdb_api_delete(user=user, endpoint=paste0('synchronizations/', import_id), 
                   api_key=api_key, ...)
}

cdb_synchronizations <- function(user, api_key, ...) {
    cdb_api_get(user, 'synchronizations', query=list(api_key=api_key), ...)
}

cdb_get_importid <- function(table, user, api_key, ...) {
    r <- cdb_synchronizations(user, api_key, ...)
}
