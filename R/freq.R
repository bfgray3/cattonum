freq_labeler <- function(.grouping) {
  results <- as.data.frame(table(.grouping))
  rownames(results) <- results[[".grouping"]]
  results[names(results) != ".grouping"]
}


#' Frequency encoding
#'
#' @param train The training data, in a `data.frame` or `tibble`.
#' @param ... The columns to be encoded.  If none are specified, then
#'   all character and factor columns are encoded.
#' @param test The test data, in a `data.frame` or `tibble`.
#' @param verbose Should informative messages be printed?  Defaults to
#'   `TRUE` (not yet used).
#' @return The encoded dataset in a `cattonum_df` if no test dataset was
#'   provided, and the encoded datasets in a `cattonum_df2` otherwise.
#' @examples
#' catto_freq(iris)
#' @export
catto_freq <- function(train,
                       ...,
                       test,
                       verbose = TRUE) {
  UseMethod("catto_freq")
}

#' @export
# nolint start
catto_freq.data.frame <- function(train,
                                  # nolint end
                                  ...,
                                  test = NULL,
                                  verbose = TRUE) {
  validate_col_types(train)
  test_also <- !is.null(test)
  if (test_also) check_train_test(train, test)

  cats <- pick_cols(train, deparse(substitute(train)), ...)

  freq_lkps <- lapply(train[cats], freq_labeler)

  train[cats] <- encode_from_lkp(train[cats], freq_lkps)

  if (!test_also) {
    cattonum_df(train)
  } else {
    test[cats] <- encode_from_lkp(test[cats], freq_lkps)
    cattonum_df2(train = train, test = test)
  }
}
