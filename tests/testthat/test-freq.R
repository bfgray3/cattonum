expected_df_both <- data.frame(
  y = y,
  x1 = c(3L, 2L, NA_integer_, 2L, 3L, 3L),
  x2 = as.integer(c(4, 4, 4, 2, 2, 4))
)

expected_tbl_both <- as_tibble(expected_df_both)

expected_x1_df_fact <- data.frame(y, x1 = c(3L, 2L, NA_integer_, 2L, 3L, 3L), x2)

expected_x1_df_char <- data.frame(
  y,
  x1 = as.integer(c(3, 2, NA, 2, 3, 3)),
  x2,
  stringsAsFactors = FALSE
)

expected_x1_tbl_char <- tibble(y, x1 = c(3L, 2L, NA_integer_, 2L, 3L, 3L), x2)

expected_x1_tbl_fact <- tibble(
  y,
  x1 = c(3L, 2L, NA_integer_, 2L, 3L, 3L),
  x2 = factor(x2)
)

test_df <- data.frame(
  y = y[seq(5)],
  x1 = c(NA, NA, "a", "b", "b"),
  x2 = c("d", NA, NA, "c", "c")
)

encoded_test <- data.frame(
  y = y[seq(5)],
  x1 = c(NA, NA, 3, 2, 2),
  x2 = c(2, NA, NA, 4, 4)
)

# TODO: check class of input, num of columns to encode

test_that("catto_freq(): multiple data.frame training columns.", {
  # TODO
})

test_that("catto_freq(): multiple tibble training columns.", {
  # TODO
})

test_that("catto_freq(): one data.frame training column.", {
  # TODO
})

test_that("catto_freq(): one tibble training column.", {
  # TODO
})

test_that("catto_freq2() works for TODO.", {
  # TODO
})
