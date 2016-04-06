#' Import data from local file into CartoDB
#'
#' @export
#' @return Response from POST request
cdb_import_file <- function(path, user, api_key, privacy='private', ...) {
    stopifnot(file_test('-f', path))
    cdb_api_post(user=user, endpoint='imports/', 
                 body=list(upload_file(path)), 
                 query=list(api_key=api_key, privacy=privacy), ...)
}

#' Import data from URL into CartoDB
#'
#' @export
#' @return Response from POST request
cdb_import_url <- function(url, user, api_key, privacy='private', ...) {
    cdb_api_post(user=user, endpoint='imports/', 
                 body=list(url=url), 
                 query=list(api_key=api_key, privacy=privacy), ...)
}

#' Import file into CartoDB as a synced table
#'
#' @export
#' @return response from POST request
cdb_synctable_import <- function(data_url, user, api_key,
                                 sync_interval=604800, ...) {
    cdb_api_post(user=user, endpoint='synchronizations', 
                 body=list(url=data_url, interval=sync_interval), 
                 query=list(api_key=api_key), ...)
}

#' Check status of sync table
#'
#' @export
#' @return sync table status
cdb_synctable_status <- function(table, user, api_key, ...) {
    import_id <- cdb_synctable_getid(table, user, api_key, ...)
    cdb_api_get(user=user,
                endpoint=paste0('synchronizations/', import_id, '/sync_now'),
                query=list(api_key=api_key), ...)
}

#' Force sync table to sync
#'
#' @export
#' @return sync table status
cdb_synctable_force <- function(table, user, api_key, ...) {
    import_id <- cdb_synctable_getid(table, user, api_key, ...)
    cdb_api_put(user=user,
                endpoint=paste0('synchronizations/', import_id, '/sync_now'),
                query=list(api_key=api_key), ...)
}

#' Convert sync table to regular table (remove sync)
#'
#' @export
#' @return Response from DELETE request
cdb_synctable_convert <- function(table, user, api_key, ...) {
    import_id <- cdb_synctable_getid(table, user, api_key, ...)
    cdb_api_delete(user=user, endpoint=paste0('synchronizations/', import_id), 
                   api_key=api_key, ...)
}

#' List sync tables for a given CartoDB account
#'
#' @export
#' @return Response from GET request
cdb_synchronizations <- function(user, api_key, ...) {
    cdb_api_get(user, 'synchronizations', query=list(api_key=api_key), ...)
}

#' Check if a table is a sync table
#'
#' @export
#' @importFrom jsonlite fromJSON
#' @return Response from POST request
cdb_synctable_check <- function(table, user, api_key, ...) {
    r <- cdb_synchronizations(user, api_key, ...)
    r <- fromJSON(content(r, as="text"))$synchronizations
    if (table %in% r$name) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

#' Get id for a sync table
#'
#' @export
#' @importFrom jsonlite fromJSON
#' @return Response from POST request
cdb_synctable_getid <- function(table, user, api_key, ...) {
    r <- cdb_synchronizations(user, api_key, ...)
    r <- fromJSON(content(r, as="text"))$synchronizations
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
cdb_delete <- function(table, user, api_key, ...) {
    if(cdb_synctable_check(table=table, user=user, api_key=api_key, ...)) {
        cdb_synctable_convert(table=table, user=user, api_key=api_key, ...)
    }
    cdb_sql_call(user=user, query=paste("DROP TABLE", table),
                 api_key=api_key, ...)
}
