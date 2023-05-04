#' Get American Cutaneous Leishmaniasis confirmed cases
#'
#' @param agg character. Spatial aggregation level. \code{uf_res} for UF of residence. \code{regsaude_res} for regiao de saude of residence. \code{mun_res} for municipality of residence.
#' @param agg_time character. Time aggregation level. \code{year} for yearly data. \code{month} for monthly data. \code{week} for weekly data. Defaults to \code{year}.
#' @param ano vector. Year of the case.
#' @param sexo character. Sex of the case. \code{Masculino} for males, \code{Feminino} for females and \code{Ignorado} for unknown.
#' @param idade_a numeric. Minimum age of the deceased, in years.
#' @param idade_b numeric. Maximum age of the deceased, in years.
#' @param psql_user character. psql username. If not provided, the function will look for it on renviron.
#' @param psql_pwd character. psql password. If not provided, the function will look for it on renviron.
#'
#' @return A tibble.
#'
#' @importFrom rlang .data
#' @export
get_leish_teg <- function(agg, agg_time, ano, sexo = NULL, idade_a = NULL, idade_b = NULL, psql_user = NULL, psql_pwd = NULL){
  # Function argument check
  checkmate::assert_choice(x = agg, choices = c("uf_res", "regsaude_res", "mun_res"))
  checkmate::assert_choice(x = agg_time, choices = c("year", "month", "week"))
  checkmate::assert_vector(x = ano)
  checkmate::assert_choice(x = sexo, choices = c("Masculino", "Feminino", "Ignorado"), null.ok = TRUE)
  checkmate::assert_number(x = idade_a, lower = 0, null.ok = TRUE)
  checkmate::assert_number(x = idade_b, lower = 0, null.ok = TRUE)
  checkmate::assert_string(x = psql_user, null.ok = TRUE)
  checkmate::assert_string(x = psql_pwd, null.ok = TRUE)

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
  psql_table <- "leish_tegt"

  # Close connection
  on.exit(DBI::dbDisconnect(conn = conn))

  # Check if table is available
  stopifnot(check_table_avail(conn = conn, table = psql_table))

  # Lazy table abstraction
  dengue_tb <- dplyr::tbl(conn, DBI::Id(schema = psql_schema, table = psql_table))

  # Variable spatial aggregation
  if (agg == "uf_res"){
    agg <- "geocoduf"
  } else if(agg == "regsaude_res"){
    agg <- "geocodrs"
  } else if(agg == "mun_res"){
    agg <- "geocodmu"
  }

  # Prepare request

  # Filter by year
  res <- dengue_tb %>%
    dplyr::filter(.data$year %in% ano)

  # Filter by sex
  if(!is.null(sexo)){
    if(sexo == "Masculino"){
      res <- res %>%
        dplyr::filter(.data$sex == "M")
    } else if(sexo == "Feminino"){
      res <- res %>%
        dplyr::filter(.data$sex == "F")
    } else if(sexo == "Ignorado"){
      res <- res %>%
        dplyr::filter(.data$sex == "I")
    }
  }

  # Filter by age
  if(!is.null(idade_a) & is.null(idade_b)){
    res <- res %>%
      dplyr::filter(.data$age <= idade_a)
  }
  if(!is.null(idade_b) & is.null(idade_a)){
    res <- res %>%
      dplyr::filter(.data$age <= idade_b)
  }
  if(!is.null(idade_a) & !is.null(idade_b)){
    res <- res %>%
      dplyr::filter(.data$age >= idade_a & .data$age <= idade_b)
  }

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

  # Group and count records by space and time aggregations
  res <- res %>%
    dplyr::group_by(agg = rlang::sym(agg), agg_time = .data$date) %>%
    dplyr::summarise(freq = dplyr::n()) %>%
    dplyr::ungroup()

  # Collect data from connection
  res <- res %>%
    dplyr::collect()

  # Convert variable
  res <- res %>%
    dplyr::mutate(
      agg = as.numeric(.data$agg),
      freq = as.numeric(.data$freq)
    )

  # Format month and week dates
  if(agg_time %in% c("month", "week")){
    res <- res %>%
      dplyr::mutate(
        p1 = substr(.data$agg_time, 0, 4),
        p2 = stringr::str_pad(substr(.data$agg_time, 6, 8), 2, pad = "0"),
        agg_time = paste0(.data$p1, "-", .data$p2)
      ) %>%
      dplyr::select(-"p1", -"p2")

  }

  # Return result
  return(res)
}
