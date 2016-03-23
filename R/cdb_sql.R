#' Make a SQL API call
#'
#' @export
#' @return Response from POST request
cdb_sql_call <- function(user, query, api_key, ...) {
    cdb_api_get(user=user, endpoint='sql', version='v2',
                query=list(q=query, api_key=api_key), ...)
}
