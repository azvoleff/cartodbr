#' Make a SQL API call
#'
#' @export
#' @importFrom jsonlite fromJSON
#' @importFrom httr status_code content
#' @return Response from POST request
cdb_sql_call <- function(query, api_key, ...) {
    r <- cdb_api_get(endpoint='sql', version='v2',
                     query=list(q=query, api_key=get_api_key()), ...)
    if (status_code(r) != 200) stop('problem running query:\n',  r)
    fromJSON(content(r, as="text"))
}
