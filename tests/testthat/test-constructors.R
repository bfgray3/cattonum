context("constructors")


test_that("cattonum_df() works", {
  expect_silent(missing_arg <- cattonum_df())
  expect_s3_class(missing_arg, c("cattonum_df", "data.frame"))

  expect_error(cattonum_df(list()), regexp = "`x` must be a data.frame or a tibble.", fixed = TRUE)

  expect_silent(tibble_arg <- cattonum_df(tibble::tibble(x = 1L)))
  expect_s3_class(tibble_arg, c("cattonum_df", "tbl_df", "tbl", "data.frame"))

  expect_silent(df_arg <- cattonum_df(data.frame(x = 1L)))
  expect_s3_class(df_arg, c("cattonum_df", "data.frame"))
})

test_that("cattonum_dfs() works", {

})
