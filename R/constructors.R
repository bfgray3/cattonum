#' Constructor for class cattonum_df
#'
#' @param x `NULL` (the default), or a tibble or data.frame.
#' @return Either an empty data frame (if `x` is `NULL`), or `x`.
#'   In both cases, the class is `c("cattonum_df", "data.frame")`.
#' @examples
#' cattonum_df(iris)
#' cattonum_df()
#' @export
cattonum_df <- function(x = NULL) {
  if (!is.null(x)) {
    if (!is.data.frame(x)) {
      stop("If `x` is not NULL, it must be a data.frame or a tibble.", call. = FALSE)
    }
    structure(x, class = c("cattonum_df", class(x)))
  } else {
    structure(data.frame(), class = c("cattonum_df", "data.frame"))
  }
}

#' Constructor for class cattonum_df2
#'
#' @param train `NULL` (the default), or a tibble or data.frame.
#' @param test `NULL` (the default), or a tibble or data.frame
#'   with the same names as `train`.
#' @return A list of class `cattonum_df2` with names "train"
#'   and "test".
#' @examples
#' cattonum_df2()
#' @export
cattonum_df2 <- function(train = NULL, test = NULL) {
  if (!(is.null(train) || is.null(test))) {
    check_train_test(.train = train, .test = test)
    cattonum_df2_constructor(.x = train, .y = test)
  } else if (!is.null(train) && is.null(test)) {
    # TODO: should test have same names as train here?
    cattonum_df2_constructor(
      .x = train,
      .y = if (!is_tibble(train)) data.frame() else tibble()
    )
  } else if (is.null(train) && is.null(test)) {
    cattonum_df2_constructor(.x = data.frame(), .y = data.frame())
  } else {
    stop("`test` cannot be non-NULL if `train` is NULL.", call. = FALSE)
  }
}

cattonum_df2_constructor <- function(.x, .y) {
  structure(list(train = .x, test = .y), class = "cattonum_df2")
}
