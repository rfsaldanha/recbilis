#' Get psql connection parameters from renviron
#'
#' @return a list.
#'

get_psql_parameters <- function() {
  psql_schema <- Sys.getenv("psql_schema")
  if (psql_schema == "") {
    stop("psql_schema key not defined on renviron.")
  }

  psql_db <- Sys.getenv("psql_db")
  if (psql_db == "") {
    stop("psql_db key not defined on renviron. ")
  }

  psql_host <- Sys.getenv("psql_local_host")
  if (psql_host == "") {
    stop("psql_local_host key not defined on renviron.")
  }

  psql_port <- Sys.getenv("psql_local_port")
  if (psql_port == "") {
    stop("psql_local_port key not defined on renviron.")
  }

  psql_user <- Sys.getenv("psql_local_user")
  if (psql_user == "") {
    stop("psql_local_user key not defined on renviron.")
  }

  psql_pwd <- Sys.getenv("psql_local_psw")
  if (psql_pwd == "") {
    stop("psql_local_psw key not defined on renviron.")
  }

  return(list(
    psql_schema = psql_schema,
    psql_db = psql_db,
    psql_host = psql_host,
    psql_port = psql_port,
    psql_user = psql_user,
    psql_pwd = psql_pwd
  ))
}
