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
                                 sync_interval=604800, ...) {
    cdb_api_post(user=user, endpoint='synchronizations', 
                 body=list(url=data_url, interval=sync_interval), 
                 query=list(api_key=api_key), ...)
}

#' Convert sync table to regular table (remove sync)
#'
#' @export
#' @return Response from POST request
cdb_synctable_convert <- function(table, user, api_key, ...) {
    import_id <- cdb_synctable_getid(table, user, api_key, ...)
    cdb_api_delete(user=user, endpoint=paste0('synchronizations/', import_id), 
                   api_key=api_key, ...)
}

#' List sync tables for a given CartoDB account
#'
#' @export
#' @return Response from POST request
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
#' @importFrom jsonlite fromJSON
#' @return Response from POST request
cdb_delete <- function(table, user, api_key, ...) {
    if(cdb_synctable_check(table=table, user=user, api_key=api_key, ...)) {
        cdb_synctable_convert(table=table, user=user, api_key=api_key, ...)
    }
    cdb_sql_call(user=user, query=paste("DROP TABLE", table),
                 api_key=api_key, ...)
}
