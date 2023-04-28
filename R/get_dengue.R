#' Get dengue confirmed cases
#'
#' @param agg character. Spatial aggregation level. \code{mun_res} for municipality of residence.
#' @param agg_time character. Time aggregation level. \code{year} for yearly data. \code{month} for monthly data. \code{week} for weekly data. Defaults to \code{year}.
#' @param ano vector. Year of the case.
#' @param psql_user character. psql username. If not provided, the function will look for it on renviron.
#' @param psql_pwd character. psql password. If not provided, the function will look for it on renviron.
#'
#' @return A tibble.
#'
#' @importFrom rlang .data
#' @export
get_dengue <- function(agg, agg_time, ano, psql_user = NULL, psql_pwd = NULL){
  # Function argument check
  checkmate::assert_choice(x = agg, choices = c("mun_res"))
  checkmate::assert_choice(x = agg_time, choices = c("year", "month", "week"))
  checkmate::assert_vector(x = ano)

  # Try to get psql user from renviron if not provided
  if(is.null(psql_user)){
    psql_user <- get_psql_user()
  }

  # Try to get psql password from renviron if not provided
  if(is.null(psql_pwd)){
    psql_pwd <- get_psql_pwd()
  }

  # Creates database connection
  conn <- DBI::dbConnect(
    RPostgres::Postgres(),
    dbname = psql_db,
    host = psql_host,
    port = psql_port,
    user = psql_user,
    password = psql_pwd
  )

  # Table
  psql_table <- "dengue"

  # Close connection
  on.exit(DBI::dbDisconnect(conn = conn))

  # Check if table is available
  if(!DBI::dbExistsTable(
    conn = conn,
    DBI::Id(schema = psql_schema, table = psql_table)
  )){stop(glue::glue("Table '{psql_table}' does not exist on '{psql_schema}' schema of '{psql_db}' database on '{psql_host}' host."))}

  # Lazy table abstraction
  dengue_tb <- dplyr::tbl(conn, DBI::Id(schema = psql_schema, table = psql_table))

  # Variable spatial aggregation
  if (agg == "mun_res"){
    agg <- "geocod"
  }

  # Prepare request

  # Filter by year
  res <- dengue_tb %>%
    dplyr::filter(.data$year %in% ano)

  # Create date variable
  if(agg_time == "year"){
    res <- res %>%
      dplyr::mutate(date = as.character(.data$year))
  } else if(agg_time == "month"){
    res <- res %>%
      dplyr::mutate(date = paste0(.data$year, "-", .data$month))
  } else if(agg_time == "week"){
    res <- res %>%
      dplyr::mutate(date = paste0(.data$year, "-", .data$epiweek))
  }

  # Group by space and time aggregations
  res <- res %>%
    dplyr::group_by(rlang::sym(agg), .data$date) %>%
    dplyr::summarise(freq = dplyr::n()) %>%
    dplyr::ungroup()

  # Collect data from connection
  res <- res %>%
    dplyr::collect()

  # Convert variable
  res <- res %>%
    dplyr::mutate(freq = as.numeric(.data$freq))

  # Format month and week dates
  if(agg_time %in% c("month", "week")){
    res <- res %>%
      dplyr::mutate(
        p1 = substr(.data$date, 0, 4),
        p2 = stringr::str_pad(substr(.data$date, 6, 8), 2, pad = "0"),
        date = paste0(.data$p1, "-", .data$p2)
      ) %>%
      dplyr::select(-.data$p1, -.data$p2)

  }

  # Return result
  return(res)
}
