incr_df <- data.frame(
  y = y,
  x1 = c(2, 1, NA, 1, 2, 2),
  x2 = c(2, 2, 2, 1, 1, 2)
)

decr_df <- data.frame(
  y = y,
  x1 = c(1, 2, NA, 2, 1, 1),
  x2 = c(1, 1, 1, 2, 2, 1)
)

order_df <- data.frame(
  y = y,
  x1 = c(1, 2, NA, 2, 1, 1),
  x2 = c(1, 1, 1, 2, 2, 1)
)

encoded_test <- data.frame(
  y = y[seq(5)],
  x1 = c(NA, NA, 2, 1, 1),
  x2 = c(1, NA, NA, 2, 2)
)

test_that("catto_label() correctly encodes train data.", {
  order_options <- c(
    "increasing",
    "decreasing",
    "observed",
    "random"
  )

  for (ordering_method in order_options) {

    ### ALL CATEGORICAL COLUMNS ###
    col <- c("x1", "x2")

    label_fact1 <- catto_label(df_fact,
      ordering = ordering_method
    )
    label_fact2 <- catto_label(df_fact, x1, x2,
      ordering = ordering_method
    )
    label_fact3 <- catto_label(df_fact, c(x1, x2),
      ordering = ordering_method
    )
    label_fact4 <- catto_label(df_fact, c("x1", "x2"),
      ordering = ordering_method
    )
    label_fact5 <- catto_label(df_fact, col,
      ordering = ordering_method
    )
    label_char1 <- catto_label(df_char,
      ordering = ordering_method
    )
    label_char2 <- catto_label(df_char, x1, x2,
      ordering = ordering_method
    )
    label_char3 <- catto_label(df_char, c(x1, x2),
      ordering = ordering_method
    )
    label_char4 <- catto_label(df_char, c("x1", "x2"),
      ordering = ordering_method
    )
    label_char5 <- catto_label(df_char, col,
      ordering = ordering_method
    )

    expected_df_both <- switch(
      ordering_method,
      increasing = incr_df,
      decreasing = decr_df,
      # nolint start
      observed = ,
      # nolint end
      random = order_df
    )

    result_names <- c(
      paste0("label_fact", seq(5)),
      paste0("label_char", seq(5))
    )
    char_and_fact <- mget(result_names)

    if (ordering_method != "random") {
      for (m in char_and_fact) expect_equal(m, cattonum_df(expected_df_both))
    } else {
      for (j in seq(ncol(expected_df_both))) {
        expect_equal(
          sort(m[, 1], na.last = TRUE),
          sort(expected_df_both[, 1], na.last = TRUE)
        )
      }
    }

    ### SUBSET OF CATEGORICAL COLUMNS ###

    expected_x1_only <- data.frame(y = y, x1 = c(2, 1, NA, 1, 2, 2), x2 = x2)
    expect_equal(catto_label(df_fact, "x1"), cattonum_df(expected_x1_only))
  }
})

test_that("catto_label() correctly encodes test data.", {
  expected <- cattonum_df2(train = incr_df, test = encoded_test)
  expect_equal(catto_label(df_fact, test = test_df), expected)
})

test_that("catto_label() handles different column-level encodings.", {
  encoded <- catto_label(df_fact, ordering = c("increasing", "decreasing"))
  expected <- data.frame(
    y = y,
    x1 = c(2, 1, NA, 1, 2, 2),
    x2 = c(1, 1, 1, 2, 2, 1)
  )

  expect_equal(encoded, cattonum_df(expected))
})


test_that("catto_label() handles user-specified orderings.", {
  encoded <- catto_label(df_fact,
    ordering = list(c("b", "a"), c("c", "d"))
  )
  expected <- data.frame(
    y = y,
    x1 = c(2, 1, NA, 1, 2, 2),
    x2 = c(1, 1, 1, 2, 2, 1)
  )

  expect_equal(encoded, cattonum_df(expected))
})
