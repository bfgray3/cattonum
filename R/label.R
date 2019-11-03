ordering_fun <- function(.method) {
  switch(.method,
    increasing = function(...) ordered_by_freq(..., .decr = FALSE),
    decreasing = function(...) ordered_by_freq(..., .decr = TRUE),
    observed = function(...) unique(stats::na.omit(...)),
    random = function(...) sample(unique(stats::na.omit(...)))
  )
}


ordered_labels <- function(.x, .how) {
  if (is.factor(.x)) .x <- as.character(.x)
  order_fun <- ordering_fun(.how)
  ordered_labs <- order_fun(.x)
  data.frame(
    new_lab = seq_along(ordered_labs),
    row.names = ordered_labs
  )
}


parse_ordering <- function(.ordering, ...) UseMethod("parse_ordering")

parse_ordering.character <- function(.ordering, .n_cats, ...) {
  stopifnot(is.element(length(.ordering), c(1L, .n_cats)))

  valid_orderings <- c(
    "increasing",
    "decreasing",
    "observed",
    "random"
  )

  vapply(.ordering,
    match.arg,
    character(1L),
    choices = valid_orderings
  )
}

parse_ordering.list <- function(.ordering, ...) {
  .ordering
}

parse_ordering.default <- function(.ordering, ...) {
  stop("`parse_ordering` can't handle class", class(.ordering), ".",
    call. = FALSE
  )
}


make_lkp_tables <- function(.order, ...) UseMethod("make_lkp_tables")

make_lkp_tables.list <- function(.order, .dat, ...) {
  Map(lkp_from_list, .ord = .order, .orig_col = .dat)
}

make_lkp_tables.character <- function(.order, .dat, .seed, ...) {
  if (any(.order == "random")) set.seed(.seed)
  Map(ordered_labels, .x = .dat, .how = .order)
}

make_lkp_tables.default <- function(.order, ...) {
  stop("`make_lkp_tables` can't handle class", class(.order), ".",
    call. = FALSE
  )
}


lkp_from_list <- function(.ord, .orig_col) {
  stopifnot(setequal(.ord, stats::na.omit(.orig_col)))
  data.frame(new_lab = seq_along(.ord), row.names = .ord)
}


#' Label encoding
#'
#' @param train The training data, in a `data.frame` or `tibble`.
#' @param ... The columns to be encoded.  If none are specified, then
#'   all character and factor columns are encoded.
#' @param test The test data, in a `data.frame` or `tibble`.
#' @param ordering How should labels be assigned to levels?  There are
#'   three different ways to pass this argument.  First, a length one
#'   character vector with value "increasing", "decreasing", "observed",
#'   or "random" will apply that ordering to each column being encoded.
#'   Second, a character vector of length greater than one may be passed,
#'   specifying one of the above four options for each column being encoded.
#'   Finally, a list may be passed specifying a user-defined ordering for each
#'   column being encoded.
#' @param verbose Should informative messages be printed?  Defaults to
#'   `TRUE` (not yet used).
#' @param seed The random seed set before all random ordering encodings
#'   if there are any.
#' @return The encoded dataset in a `data.frame` or `tibble`,
#'   whichever was input.  If a test dataset was provided, a list with names
#'   "train" and "test" is returned holding the encoded training and
#'   test datasets.
#' @examples
#' catto_label(iris)
#'
#' y <- 2^(0:5)
#' x1 <- c("a", "b", NA, "b", "a", "a")
#' x2 <- c("c", "c", "c", "d", "d", "c")
#' df_fact <- data.frame(y, x1, x2)
#'
#' catto_label(df_fact,
#'   ordering = list(c("b", "a"), c("c", "d"))
#' )
#'
#' catto_label(df_fact, ordering = c("increasing", "decreasing"))
#' @export
catto_label <- function(train,
                        ...,
                        test,
                        ordering = "increasing",
                        verbose = TRUE,
                        seed = 4444) {
  UseMethod("catto_label")
}

#' @export
catto_label.data.frame <- function(train,
                                   ...,
                                   test = NULL,
                                   ordering = "increasing",
                                   verbose = TRUE,
                                   seed = 4444) {
  validate_col_types(train)
  test_also <- !is.null(test)
  if (test_also) check_train_test(train, test)

  cats <- pick_cols(train, deparse(substitute(train)), ...)

  ordering <- parse_ordering(ordering, length(cats))

  encoding_lkps <- make_lkp_tables(ordering, train[cats], seed)

  train[cats] <- encode_from_lkp(train[cats], encoding_lkps)

  if (!test_also) {
    train
  } else {
    test[cats] <- encode_from_lkp(test[cats], encoding_lkps)
    list(train = train, test = test)
  }
}
