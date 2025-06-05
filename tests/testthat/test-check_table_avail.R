test_that("check_table_avail works with true", {
  res <- check_table_avail(conn = db_connect(), table = "dengue_sinan")

  expect_true(res)
})

test_that("check_table_avail works with false", {
  res <- check_table_avail(conn = db_connect(), table = "aaaa")

  expect_false(res)
})
