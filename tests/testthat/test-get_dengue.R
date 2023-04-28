test_that("get_dengue with mun_res and year works", {
  res <- get_dengue(agg = "mun_res", agg_time = "year", ano = 2010)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 3700)
})

test_that("get_dengue with mun_res and month works", {
  res <- get_dengue(agg = "mun_res", agg_time = "month", ano = 2010)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 18800)
})

test_that("get_dengue with mun_res and week works", {
  res <- get_dengue(agg = "mun_res", agg_time = "week", ano = 2010)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 48500)
})
