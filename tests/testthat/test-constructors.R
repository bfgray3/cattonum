context("constructors")


test_that("cattonum_df() works", {
  expect_silent(missing_arg <- cattonum_df())
  expect_s3_class(missing_arg, c("cattonum_df", "data.frame"))

  expect_error(cattonum_df(list()), regexp = "`x` must be a data.frame or a tibble.", fixed = TRUE)

  expect_silent(tibble_arg <- cattonum_df(tbl_fact))
  expect_s3_class(tibble_arg, c("cattonum_df", "tbl_df", "tbl", "data.frame"))

  expect_silent(df_arg <- cattonum_df(df_fact))
  expect_s3_class(df_arg, c("cattonum_df", "data.frame"))
})

test_that("cattonum_dfs() works", {
  expect_silent(missing_args <- cattonum_dfs())
  expect_s3_class(missing_args, "cattonum_dfs")

  expect_error(cattonum_dfs(1L), regexp = "If passing a single argument to cattonum_dfs(), it must be a data.frame or a tibble.", fixed = TRUE)

  expect_silent(unnamed_tibble_arg <- cattonum_dfs(tbl_fact))
  expect_s3_class(unnamed_tibble_arg, "cattonum_dfs")
  expect_length(unnamed_tibble_arg, 1L)
  expect_s3_class(unnamed_tibble_arg[[1]], c("tbl_df", "tbl", "data.frame"))

  expect_silent(named_tibble_arg <- cattonum_dfs(foo = tbl_fact))
  expect_s3_class(named_tibble_arg, "cattonum_dfs")
  expect_length(named_tibble_arg, 1L)
  expect_named(named_tibble_arg, "foo")
  expect_s3_class(named_tibble_arg[[1]], c("tbl_df", "tbl", "data.frame"))

  expect_silent(mixed_tibble_args <- cattonum_dfs(foo = tbl_fact, tbl_char))
  expect_s3_class(mixed_tibble_args, "cattonum_dfs")
  expect_length(mixed_tibble_args, 2L)
  expect_named(mixed_tibble_args, c("foo", ""))
  for (x in mixed_tibble_args)
    expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
})
