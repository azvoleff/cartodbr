#' Import data from local file into CartoDB
#'
#' @export
#' @importFrom httr status_code content
#' @importFrom jsonlite fromJSON
#' @return Response from POST request
cdb_import_file <- function(path, privacy='private', ...) {
    stopifnot(file_test('-f', path))
    r <- cdb_api_post(endpoint='imports/', body=upload_file(path), 
                      query=list(api_key=get_api_key()), ...)
    if (status_code(r) != 200) stop('problem instantiating named map:\n',  r)
    fromJSON(content(r, as="text"))$id
}

#' Check status of import
#'
#' @export
#' @return Response from POST request
cdb_import_status <- function(id, ...) {
    r <- cdb_api_get(endpoint=paste0('imports/', id), 
                      query=list(api_key=get_api_key()))
    fromJSON(content(r, as="text"))
}

#' Import data from URL into CartoDB
#'
#' @export
#' @return Response from POST request
cdb_import_url <- function(url, privacy='private', ...) {
    cdb_api_post(endpoint='imports/', 
                 body=list(url=url), 
                 query=list(api_key=get_api_key(), privacy=privacy), ...)
}

#' Import file into CartoDB as a synced table
#'
#' @export
#' @return response from POST request
cdb_synctable_import <- function(data_url, sync_interval=604800, ...) {
    cdb_api_post(endpoint='synchronizations', 
                 body=list(url=data_url, interval=sync_interval), 
                 query=list(api_key=get_api_key()), ...)
}

#' Check status of sync table
#'
#' @export
#' @return sync table status
cdb_synctable_status <- function(table, ...) {
    import_id <- cdb_synctable_getid(table, ...)
    cdb_api_get(endpoint=paste0('synchronizations/', import_id, '/sync_now'),
                query=list(api_key=get_api_key()), ...)
}

#' Force sync table to sync
#'
#' @export
#' @return sync table status
cdb_synctable_force <- function(table, ...) {
    import_id <- cdb_synctable_getid(table, ...)
    cdb_api_put(endpoint=paste0('synchronizations/', import_id, '/sync_now'),
                query=list(api_key=get_api_key()), ...)
}

#' Convert sync table to regular table (remove sync)
#'
#' @export
#' @return Response from DELETE request
cdb_synctable_convert <- function(table) {
    import_id <- cdb_synctable_getid(table)
    cdb_api_delete(endpoint=paste0('synchronizations/', import_id))
}

#' List sync tables for a given CartoDB account
#'
#' @export
#' @importFrom jsonlite fromJSON
#' @return Response from GET request
cdb_synchronizations <- function(...) {
    r <- cdb_api_get('synchronizations',
                     query=list(api_key=get_api_key()), ...)
    fromJSON(content(r, as="text"))$synchronizations
}

#' Check if a table is a sync table
#'
#' @export
#' @return Response from POST request
cdb_synctable_check <- function(table, ...) {
    r <- cdb_synchronizations(...)
    if (table %in% r$name) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

#' Get id for a sync table
#'
#' @export
#' @return Response from POST request
cdb_synctable_getid <- function(table, ...) {
    r <- cdb_synchronizations(...)
    if (table %in% r$name) {
        return(r$id[which(r$name == table)])
    } else {
        stop(paste(table, 'is not a sync table'))
    }
}

#' Delete a CartoDB table
#'
#' @export
#' @return Response from POST request
cdb_delete <- function(table) {
    if (cdb_synctable_check(table=table)) {
        cdb_synctable_convert(table=table)
    }
    cdb_sql_call(query=paste("DROP TABLE", table), api_key=get_api_key())
}

#' Check if a CartoDB table exists
#'
#' @export
#' @return Response from POST request
cdb_table_exists <- function(table) {
    query <- paste0("SELECT CAST(COUNT(*) AS BIT)
                     FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_NAME = '", table,  "'")
    r <- cdb_sql_call(query=query, api_key=get_api_key())
    if (r$rows$count == 0) {
        return(FALSE)
    } else {
        return(TRUE)
    }
}
