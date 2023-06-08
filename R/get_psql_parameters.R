#' Get psql connection parameters from renviron
#'
#' @return a list.
#'

get_psql_parameters <- function(){
  psql_schema <- Sys.getenv("psql_schema")
  if (psql_schema == "") {
    stop("psql_schema not found on renviron. ")
  }

  psql_db <- Sys.getenv("psql_db")
  if (psql_db == "") {
    stop("psql_db not found on renviron. ")
  }

  psql_host <- Sys.getenv("psql_host")
  if (psql_host == "") {
    stop("psql_host not found on renviron. ")
  }

  psql_port <- Sys.getenv("psql_port")
  if (psql_port == "") {
    stop("psql_port not found on renviron. ")
  }

  psql_user <- Sys.getenv("psql_user")
  if (psql_user == "") {
    stop("psql_user not found on renviron.")
  }

  psql_pwd <- Sys.getenv("psql_pwd")
  if (psql_pwd == "") {
    stop("psql_pwd not found on renviron. ")
  }

  return(list(psql_schema = psql_schema, psql_db = psql_db,
              psql_host = psql_host, psql_port = psql_port,
              psql_user = psql_user, psql_pwd = psql_pwd))
}
