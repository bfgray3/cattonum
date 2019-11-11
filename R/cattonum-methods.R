#' @export
print.cattonum_df <- function(x, ...) {
  cat("# A cattonum_df with the following data:\n")
  NextMethod()
}


#' @export
print.cattonum_df2 <- function(x, ...) {
  if (!any(dim(x[["train"]]))) {
    cat("# A cattonum_df2 with no 'train' or 'test' data")
  }

  cat("# A cattonum_df2 with the following data:\n'train':\n")
  print(x[["train"]])

  if (any(dim(x[["test"]]))) {
    cat("'test':\n")
    print(x[["test"]])
  }
}


#' @export
summary.cattonum_df <- function(object, ...) {

}


#' @export
summary.cattonum_df2 <- function(object, ...) {

}
