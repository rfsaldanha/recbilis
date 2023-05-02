test_that("check_table_avail works with true", {
  conn <- DBI::dbConnect(
    RPostgres::Postgres(),
    dbname = recbilis:::psql_db,
    host = recbilis:::psql_host,
    port = recbilis:::psql_port,
    user = get_psql_user(),
    password = get_psql_pwd()
  )

  res <- check_table_avail(conn = conn, table = "dengue")

  expect_true(res)
})

test_that("check_table_avail works with false", {
  conn <- DBI::dbConnect(
    RPostgres::Postgres(),
    dbname = recbilis:::psql_db,
    host = recbilis:::psql_host,
    port = recbilis:::psql_port,
    user = get_psql_user(),
    password = get_psql_pwd()
  )

  res <- check_table_avail(conn = conn, table = "aaaa")

  expect_false(res)
})

