# TODO

test_that(
  "catto_aggregate()...",
  expect_silent(
    catto_aggregate(df_fact, aggregate_fun = mean, response = y, test = test_df)
  )
)
