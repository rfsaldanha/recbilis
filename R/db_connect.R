db_connect <- function(){
  # Get psql parameters from renviron
  psql_parameters <- get_psql_parameters()

  # Creates database connection
  conn <- DBI::dbConnect(
    RPostgres::Postgres(),
    dbname = psql_parameters$psql_db,
    host = psql_parameters$psql_host,
    port = psql_parameters$psql_port,
    user = psql_parameters$psql_user,
    password = psql_parameters$psql_pwd
  )

  # Return connection
  return(conn)
}
