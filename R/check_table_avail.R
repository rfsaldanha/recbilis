#' Check if a table is available in a connection and schema
#'
#' @param conn an object from \code{DBI::dbConnect}.
#' @param schema character. A schema name.
#' @param table character. A table name.
#'
#' @return Logical.
#' @export
check_table_avail <- function(conn, table){

  # Check if table is available
  res <- DBI::dbExistsTable(
    conn = conn,
    name = DBI::Id(schema = psql_schema, table = table)
  )

  # Message if not available
  if(res == FALSE){
    message(glue::glue("Table '{table}' does not exist on '{psql_schema}' schema of '{psql_db}' database on '{psql_host}' host."))
  }

  # Return boolean result
  return(res)
}
