test_that("get_febre_amarela with mun_res and year works", {
  res <- get_febre_amarela(agg = "mun_res", agg_time = "year", ano = 2010)

  expect_equal("tbl_df", class(res)[1])
  expect_equal(nrow(res), 2)
})

test_that("get_febre_amarela with regsaude_449_res and year works", {
  res <- get_febre_amarela(
    agg = "regsaude_449_res",
    agg_time = "year",
    ano = 2010
  )

  expect_equal("tbl_df", class(res)[1])
  expect_equal(nrow(res), 2)
})

test_that("get_febre_amarela with uf_res and year works", {
  res <- get_febre_amarela(agg = "uf_res", agg_time = "year", ano = 2010)

  expect_equal("tbl_df", class(res)[1])
  expect_equal(nrow(res), 2)
})

test_that("get_febre_amarela with mun_res and month works", {
  res <- get_febre_amarela(agg = "mun_res", agg_time = "month", ano = 2010)

  expect_equal("tbl_df", class(res)[1])
  expect_equal(nrow(res), 2)
})

test_that("get_febre_amarela with mun_res and week works", {
  res <- get_febre_amarela(agg = "mun_res", agg_time = "week", ano = 2010)

  expect_equal("tbl_df", class(res)[1])
  expect_equal(nrow(res), 2)
})

test_that("get_febre_amarela with sex filter and year works", {
  res0 <- get_febre_amarela(agg = "mun_res", agg_time = "year", ano = 2010)
  res1 <- get_febre_amarela(
    agg = "mun_res",
    sexo = "Masculino",
    agg_time = "year",
    ano = 2010
  )
  res2 <- get_febre_amarela(
    agg = "mun_res",
    sexo = "Feminino",
    agg_time = "year",
    ano = 2010
  )
  res3 <- get_febre_amarela(
    agg = "mun_res",
    sexo = "Ignorado",
    agg_time = "year",
    ano = 2010
  )

  expect_equal("tbl_df", class(res0)[1])
  expect_equal("tbl_df", class(res1)[1])
  expect_equal("tbl_df", class(res2)[1])
  expect_equal("tbl_df", class(res3)[1])

  expect_equal(sum(res0$freq, na.rm = TRUE), sum(res1$freq, na.rm = TRUE))
  expect_gt(sum(res0$freq, na.rm = TRUE), sum(res2$freq, na.rm = TRUE))
  expect_gt(sum(res0$freq, na.rm = TRUE), sum(res3$freq, na.rm = TRUE))
})

test_that("get_febre_amarela with age filters and year works", {
  res0 <- get_febre_amarela(agg = "mun_res", agg_time = "year", ano = 2010)
  res1 <- get_febre_amarela(
    agg = "mun_res",
    idade_a = 10,
    agg_time = "year",
    ano = 2010
  )
  res2 <- get_febre_amarela(
    agg = "mun_res",
    idade_b = 50,
    agg_time = "year",
    ano = 2010
  )
  res3 <- get_febre_amarela(
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

  expect_gt(sum(res0$freq, na.rm = TRUE), sum(res1$freq, na.rm = TRUE))
  expect_equal(sum(res0$freq, na.rm = TRUE), sum(res2$freq, na.rm = TRUE))
  expect_equal(sum(res0$freq, na.rm = TRUE), sum(res3$freq, na.rm = TRUE))
  expect_gt(sum(res3$freq, na.rm = TRUE), sum(res1$freq, na.rm = TRUE))
  expect_equal(sum(res2$freq, na.rm = TRUE), sum(res3$freq, na.rm = TRUE))
})
