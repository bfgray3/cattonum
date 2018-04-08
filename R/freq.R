####################
### freq_labeler ###
####################

freq_labeler <- function(.grouping) {
  results <- as.data.frame(table(.grouping))
  rownames(results) <- results[[".grouping"]]
  results[names(results) != ".grouping"]
}

##################
### catto_freq ###
##################

#' Frequency encoding
#'
#' @param train The training data, in a \code{data.frame} or \code{tibble}.
#' @param ... The columns to be encoded.  If none are specified, then
#'   all character and factor columns are encoded.
#' @param test The test data, in a \code{data.frame} or \code{tibble}.
#' @param verbose Should informative messages be printed?  Defaults to
#'   \code{TRUE} (not yet used).
#' @return The encoded dataset in a \code{data.frame} or \code{tibble},
#'   whichever was input.  If a test dataset was provided, a list with names
#'   "train" and "test" is returned holding the encoded training and
#'   test datasets.
#' @examples
#' catto_freq(iris, Sepal.Length)
#' @export
catto_freq <-  function(train,
                        ...,
                        test,
                        verbose = TRUE) {

  validate_col_types(train)
  test_also <- ! missing(test)
  if (test_also) check_train_test(train, test)

  nms <- names(train)

  cats <- pick_cols(train, ...)

  freq_lkps <- lapply(train[cats], freq_labeler)

  train[cats] <- encode_from_lkp(train[cats], freq_lkps)

  if (! test_also) {
    train
  } else {
    test[cats] <- encode_from_lkp(test[cats], freq_lkps)
    list(train = train, test = test)
  }

}

###