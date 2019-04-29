test_that("cattonum_df() works", {
  expect_silent(null_arg <- cattonum_df())
  expect_s3_class(null_arg, c("cattonum_df", "data.frame"))

  expect_error(
    cattonum_df(list()),
    regexp = "If `x` is not NULL, it must be a data.frame or a tibble.",
    fixed = TRUE
  )

  expect_silent(tibble_arg <- cattonum_df(tbl_fact))
  expect_s3_class(tibble_arg, c("cattonum_df", "tbl_df", "tbl", "data.frame"))
  expect_equivalent(tibble_arg, tbl_fact)

  expect_silent(df_arg <- cattonum_df(df_fact))
  expect_s3_class(df_arg, c("cattonum_df", "data.frame"))
  expect_equivalent(df_arg, df_fact)
})

test_that("cattonum_df2() works", {
  expect_error(
    cattonum_df2(NULL, iris),
    regexp = "`test` cannot be non-NULL if `train` is NULL.",
    fixed = TRUE
  )

  expect_silent(
    tibble_args <- cattonum_df2(
      tbl_fact[seq(3), ],
      tbl_fact[seq(3, nrow(tbl_fact)), ]
    )
  )
  expect_s3_class(tibble_args, "cattonum_df2")
  expect_equivalent(
    tibble_args,
    list(
      tbl_fact[seq(3), ],
      tbl_fact[seq(3, nrow(tbl_fact)), ]
    )
  )
})
