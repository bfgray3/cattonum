test_that("print.cattonum_df works correctly", {
  verify_output(test_path("test-print-cattonum-df.txt"), print(cattonum_df(head(iris))))
})

test_that("print.cattonum_df2 works correctly", {
  verify_output(
    test_path("test-print-cattonum-df2.txt"),
    print(cattonum_df2(head(iris), tail(iris)))
  )
})

test_that("summary.cattonum_df works correctly", {
  verify_output(
    test_path("test-summary-cattonum-df.txt"),
    summary(cattonum_df(head(iris)))
  )
})

test_that("summary.cattonum_df2 works correctly", {
  verify_output(
    test_path("test-summary-cattonum-df2.txt"),
    summary(cattonum_df2(head(iris), tail(iris)))
  )
})
