test_that("get_leish_vis with mun_res and year works", {
  res <- get_leish_vis(agg = "mun_res", agg_time = "year", ano = 2010)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 800)
})

test_that("get_leish_vis with regsaude_449_res and year works", {
  res <- get_leish_vis(agg = "regsaude_449_res", agg_time = "year", ano = 2010)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 240)
})

test_that("get_leish_vis with uf_res and year works", {
  res <- get_leish_vis(agg = "uf_res", agg_time = "year", ano = 2010)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 20)
})

test_that("get_leish_vis with mun_res and month works", {
  res <- get_leish_vis(agg = "mun_res", agg_time = "month", ano = 2010)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 2100)
})

test_that("get_leish_vis with mun_res and week works", {
  res <- get_leish_vis(agg = "mun_res", agg_time = "week", ano = 2010)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 2900)
})

test_that("get_leish_vis with sex filter and year works", {
  res0 <- get_leish_vis(agg = "mun_res", agg_time = "year", ano = 2010)
  res1 <- get_leish_vis(agg = "mun_res", sexo = "Masculino", agg_time = "year", ano = 2010)
  res2 <- get_leish_vis(agg = "mun_res", sexo = "Feminino", agg_time = "year", ano = 2010)
  res3 <- get_leish_vis(agg = "mun_res", sexo = "Ignorado", agg_time = "year", ano = 2010)

  expect_equal("tbl_df", class(res0)[1])
  expect_equal("tbl_df", class(res1)[1])
  expect_equal("tbl_df", class(res2)[1])
  expect_equal("tbl_df", class(res3)[1])

  expect_gt(sum(res0$freq, na.rm = TRUE), sum(res1$freq, na.rm = TRUE))
  expect_gt(sum(res0$freq, na.rm = TRUE), sum(res2$freq, na.rm = TRUE))
  expect_gt(sum(res0$freq, na.rm = TRUE), sum(res3$freq, na.rm = TRUE))
})

test_that("get_leish_vis with age filters and year works", {
  res0 <- get_leish_vis(agg = "mun_res", agg_time = "year", ano = 2010)
  res1 <- get_leish_vis(agg = "mun_res", idade_a = 10, agg_time = "year", ano = 2010)
  res2 <- get_leish_vis(agg = "mun_res", idade_b = 50, agg_time = "year", ano = 2010)
  res3 <- get_leish_vis(agg = "mun_res", idade_a = 10, idade_b = 50, agg_time = "year", ano = 2010)

  expect_equal("tbl_df", class(res0)[1])
  expect_equal("tbl_df", class(res1)[1])
  expect_equal("tbl_df", class(res2)[1])
  expect_equal("tbl_df", class(res3)[1])

  expect_gt(sum(res0$freq, na.rm = TRUE), sum(res1$freq, na.rm = TRUE))
  expect_gt(sum(res0$freq, na.rm = TRUE), sum(res2$freq, na.rm = TRUE))
  expect_gt(sum(res0$freq, na.rm = TRUE), sum(res3$freq, na.rm = TRUE))
  expect_gt(sum(res1$freq, na.rm = TRUE), sum(res3$freq, na.rm = TRUE))
  expect_gt(sum(res2$freq, na.rm = TRUE), sum(res3$freq, na.rm = TRUE))
})
