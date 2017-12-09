context("no_new_columns")

#############
### SETUP ###
#############

y <- 2 ^ seq(from = 0, to = 4)
x1 <- c("a", "b", "a", "b", "a")
x2 <- c("c", "c", "c", "d", "d")

df_fact <- data.frame(y, x1, x2)
df_char <- data.frame(y, x1, x2, stringsAsFactors = FALSE)

mean_fact <- catto_mean(df_fact, response = "y")
mean_char <- catto_mean(df_char, response = "y")

###############
### RUNTIME ###
###############

test_that("catto_mean works.", {

  expected_df <- data.frame(y = y,
                            x1 = c(7, 5, 7, 5, 7),
                            x2 = c(7 / 3, 7 / 3, 7 / 3, 12, 12))
  expected <- as.matrix(expected_df)

  for (m in list(mean_fact, mean_char)) {

    expect_is(m, "matrix")
    expect_equal(dim(m), c(5, 3))
    expect_identical(colnames(m), c("y", "x1", "x2"))
    expect_equal(m, expected)

  }


})