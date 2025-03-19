test_that("get_chagas with mun_res and year works", {
  res <- get_chagas(agg = "mun_res", agg_time = "year", ano = 2010)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 5)
})

test_that("get_chagas with regsaude_449_res and year works", {
  res <- get_chagas(agg = "regsaude_449_res", agg_time = "year", ano = 2010)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 5)
})

test_that("get_chagas with uf_res and year works", {
  res <- get_chagas(agg = "uf_res", agg_time = "year", ano = 2010)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 5)
})

test_that("get_chagas with mun_res and month works", {
  res <- get_chagas(agg = "mun_res", agg_time = "month", ano = 2010)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 5)
})

test_that("get_chagas with mun_res and week works", {
  res <- get_chagas(agg = "mun_res", agg_time = "week", ano = 2010)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 5)
})

test_that("get_chagas with sex filter and year works", {
  res0 <- get_chagas(agg = "mun_res", agg_time = "year", ano = 2010)
  res1 <- get_chagas(
    agg = "mun_res",
    sexo = "Masculino",
    agg_time = "year",
    ano = 2010
  )
  res2 <- get_chagas(
    agg = "mun_res",
    sexo = "Feminino",
    agg_time = "year",
    ano = 2010
  )
  res3 <- get_chagas(
    agg = "mun_res",
    sexo = "Ignorado",
    agg_time = "year",
    ano = 2010
  )

  expect_equal("tbl_df", class(res0)[1])
  expect_equal("tbl_df", class(res1)[1])
  expect_equal("tbl_df", class(res2)[1])
  expect_equal("tbl_df", class(res3)[1])
})

test_that("get_chagas with age filters and year works", {
  res0 <- get_chagas(agg = "mun_res", agg_time = "year", ano = 2010)
  res1 <- get_chagas(
    agg = "mun_res",
    idade_a = 10,
    agg_time = "year",
    ano = 2010
  )
  res2 <- get_chagas(
    agg = "mun_res",
    idade_b = 50,
    agg_time = "year",
    ano = 2010
  )
  res3 <- get_chagas(
    agg = "mun_res",
    idade_a = 10,
    idade_b = 50,
    agg_time = "year",
    ano = 2010
  )

  expect_equal("tbl_df", class(res0)[1])
  expect_equal("tbl_df", class(res1)[1])
  expect_equal("tbl_df", class(res2)[1])
  expect_equal("tbl_df", class(res3)[1])
})
