####################
### ordering_fun ###
####################

ordering_fun <- function(.method) {

  switch(.method,
         increasing = function(...) ordered_by_freq(..., .decr = FALSE),
         decreasing = function(...) ordered_by_freq(..., .decr = TRUE),
         observed = function(...) unique(stats::na.omit(...)),
         random = function(...) sample(unique(stats::na.omit(...))))

}

######################
### ordered_labels ###
######################

ordered_labels <- function(.x, .type) {

  if (is.factor(.x)) .x <- as.character(.x)
  order_fun <- ordering_fun(.type)
  ordered_labs <- order_fun(.x)
  data.frame(new_lab = seq_along(ordered_labs),
             row.names = ordered_labs)

}

###################
### catto_label ###
###################

#' Label encoding
#'
#' @param train The training data, in a \code{data.frame} or \code{tibble}.
#' @param ... The columns to be encoded.  If none are specified, then
#'   all character and factor columns are encoded.
#' @param ordering How should labels be assigned to levels?  Options are
#'   "increasing", "decreasing", "observed", and "random".
#' @param test The test data, in a \code{data.frame} or \code{tibble}.
#' @param seed To be used in the future.
#' @param verbose Should informative messages be printed?  Defaults to
#'   \code{TRUE}.
#' @return The encoded dataset in a \code{matrix} if all character and
#'   factor columns have been encoded, otherwise the encoded dataset in
#'   a \code{data.frame} or \code{tibble}, whichever was input.  If a test
#'   dataset was provided, a named list is returned holding the encoded
#'   training and test datasets.
#' @examples
#' catto_label(iris, response = Sepal.Length)
#' @export
catto_label <- function(train,
                        ...,
                        ordering = c("increasing",
                                     "decreasing",
                                     "observed",
                                     "random"),
                        test,
                        seed = 4444,
                        verbose = TRUE) {

  validate_col_types(train)
  test_also <- ! missing(test)
  if (test_also) check_train_test(train, test)

  nms <- names(train)

  ordering <- match.arg(ordering)

  cats <- pick_cols(train, ...)

  encoding_lkps <- lapply(train[cats], ordered_labels, .type = ordering)

  train[cats] <- encode_from_lkp(train[cats], encoding_lkps)

  if (! test_also) {
    mat_or_df(train)
  } else {
    test[cats] <- encode_from_lkp(test[cats], encoding_lkps)
    lapply(list(train = train, test = test), mat_or_df)
  }

}

###