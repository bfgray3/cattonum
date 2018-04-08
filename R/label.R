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

ordered_labels <- function(.x, .how) {

  if (is.factor(.x)) .x <- as.character(.x)
  order_fun <- ordering_fun(.how)
  ordered_labs <- order_fun(.x)
  data.frame(new_lab = seq_along(ordered_labs),
             row.names = ordered_labs)

}

###########################
### parse_char_ordering ###
###########################

parse_char_ordering <- function(.ordering) {

  valid_orderings <- c("increasing",
                       "decreasing",
                       "observed",
                       "random")

  if (length(.ordering) == 1) {
    match.arg(.ordering, choices = valid_orderings)
  } else {
    vapply(.ordering,
           match.arg,
           character(1),
           choices = valid_orderings)
  }

}

###################
### catto_label ###
###################

#' Label encoding
#'
#' @param train The training data, in a \code{data.frame} or \code{tibble}.
#' @param ... The columns to be encoded.  If none are specified, then
#'   all character and factor columns are encoded.
#' @param test The test data, in a \code{data.frame} or \code{tibble}.
#' @param ordering How should labels be assigned to levels?  There are
#'   three different ways to pass this argument.  First, a length one character
#'   vector with value "increasing", "decreasing", "observed", or "random"
#'   will apply that ordering to each column being encoded.  Second, a character
#'   vector of length greater than one may be passed, specifying one of the above four
#'   options for each column being encoded.  Finally, a list may be passed specifying
#'   a user-defined ordering for each column being encoded.
#' @param verbose Should informative messages be printed?  Defaults to
#'   \code{TRUE}.
#' @param seed To be used in the future.
#' @return The encoded dataset in a \code{data.frame} or \code{tibble},
#'   whichever was input.  If a test dataset was provided, a named list
#'   is returned holding the encoded training and test datasets.
#' @examples
#' catto_label(iris, response = Sepal.Length)
#' @export
catto_label <- function(train,
                        ...,
                        test,
                        ordering,
                        verbose = TRUE,
                        seed = 4444) {

  # TODO: clean this up

  validate_col_types(train)
  test_also <- ! missing(test)
  if (test_also) check_train_test(train, test)

  nms <- names(train)

  cats <- pick_cols(train, ...)

  if (missing(ordering)) {
    ordering <- "increasing"
  } else if (is.character(ordering)) {
    ordering <- parse_char_ordering(ordering)
  }

  # check that the levels are good when a list is passed
  stopifnot(is.element(length(ordering), c(1, length(cats))))

  encoding_lkps <- if (is.list(ordering)) {
                     lapply(ordering,
                            function(.ord) data.frame(new_lab = seq_along(.ord),
                                                      row.names = .ord))
                   } else {
                     Map(ordered_labels, .x = train[cats], .how = ordering)
                   }

  train[cats] <- encode_from_lkp(train[cats], encoding_lkps)

  if (! test_also) {
    train
  } else {
    test[cats] <- encode_from_lkp(test[cats], encoding_lkps)
    list(train = train, test = test)
  }

}

###