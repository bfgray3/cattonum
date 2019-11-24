library(tibble)

resp_name <- "y"
y <- 2^seq(from = 0, to = 5)
x1 <- c("a", "b", NA, "b", "a", "a")
x2 <- c("c", "c", "c", "d", "d", "c")

df_fact <- data.frame(y, x1, x2)
df_char <- data.frame(y, x1, x2, stringsAsFactors = FALSE)

tbl_fact <- tibble(y, x1 = factor(x1), x2 = factor(x2))
tbl_char <- tibble(y, x1, x2)

test_df <- data.frame(
  y = y[seq(5)],
  x1 = c(NA, NA, "a", "b", "b"),
  x2 = c("d", NA, NA, "c", "c")
)

df_logi <- data.frame(
  y = y,
  x1 = c(TRUE, TRUE, FALSE, FALSE, FALSE, TRUE),
  x2 = c(TRUE, TRUE, NA, FALSE, FALSE, TRUE)
)

tbl_logi <- as_tibble(df_logi)

check_x1_x2 <- function(.encoder, .data_class, .resp) {
  if (.data_class == "data.frame") {
    fact <- df_fact
    char <- df_char
  } else {
    fact <- tbl_fact
    char <- tbl_char
  }

  list(
    .encoder(fact),
    .encoder(fact, x1, x2),
    .encoder(fact, c(x1, x2)),
    .encoder(fact, c("x1", "x2")),
    .encoder(fact, tidyselect::one_of(c("x1", "x2"))),
    .encoder(fact, tidyselect::one_of("x1", "x2")),
    .encoder(char),
    .encoder(char, x1, x2),
    .encoder(char, c(x1, x2)),
    .encoder(char, c("x1", "x2")),
    .encoder(char, tidyselect::one_of(c("x1", "x2"))),
    .encoder(char, tidyselect::one_of("x1", "x2"))
  )
}

check_x1_x2_resp <- function(.encoder, .data_class, .resp) {
  if (.data_class == "data.frame") {
    fact <- df_fact
    char <- df_char
  } else {
    fact <- tbl_fact
    char <- tbl_char
  }

  list(
    .encoder(fact, response = .resp),
    .encoder(fact, x1, x2, response = .resp),
    .encoder(fact, c(x1, x2), response = .resp),
    .encoder(fact, c("x1", "x2"), response = .resp),
    .encoder(fact, tidyselect::one_of(c("x1", "x2")), response = .resp),
    .encoder(fact, tidyselect::one_of("x1", "x2"), response = .resp),
    .encoder(char, response = .resp),
    .encoder(char, x1, x2, response = .resp),
    .encoder(char, c(x1, x2), response = .resp),
    .encoder(char, c("x1", "x2"), response = .resp),
    .encoder(char, tidyselect::one_of(c("x1", "x2")), response = .resp),
    .encoder(char, tidyselect::one_of("x1", "x2"), response = .resp)
  )
}

check_x1 <- function(.encoder, .data_class) {
  if (.data_class == "data.frame") {
    fact <- df_fact
    char <- df_char
  } else {
    fact <- tbl_fact
    char <- tbl_char
  }

  list(
    .encoder(fact, "x1"),
    .encoder(fact, x1),
    .encoder(fact, tidyselect::one_of("x1")),
    .encoder(char, "x1"),
    .encoder(char, x1),
    .encoder(char, tidyselect::one_of("x1"))
  )
}

check_x1_resp <- function(.encoder, .data_class, .resp) {
  if (.data_class == "data.frame") {
    fact <- df_fact
    char <- df_char
  } else {
    fact <- tbl_fact
    char <- tbl_char
  }

  list(
    .encoder(fact, "x1", response = .resp),
    .encoder(fact, x1, response = .resp),
    .encoder(fact, tidyselect::one_of("x1"), response = .resp),
    .encoder(char, "x1", response = .resp),
    .encoder(char, x1, response = .resp),
    .encoder(char, tidyselect::one_of("x1"), response = .resp)
  )
}
