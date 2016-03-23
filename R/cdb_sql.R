#' Drop a table from CartoDB
#'
#' @export
#' @return Response from POST request
cdb_sql_call <- function(user, query, api_key, ...) {
    cdb_api_get(user=user, endpoint='sql', version='v2',
                query=list(q=query, api_key=api_key), ...)
}

#' Drop a table from CartoDB
#'
#' @export
#' @return Response from POST request
cdb_sql_drop <- function(table, user, api_key, ...) {
    cdb_sql_call(user=user, query=paste("DROP TABLE", table), api_key=api_key, ...)
}
