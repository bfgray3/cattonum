has_data <- function(.x, .data_set) {
  any(dim(.x[[.data_set]]))
}


#' @export
print.cattonum_df <- function(x, ...) {
  cat("# A cattonum_df with the following data:\n")
  NextMethod()
}


#' @export
print.cattonum_df2 <- function(x, ...) {
  if (!has_data(x, "train")) {
    cat("# A cattonum_df2 with no 'train' or 'test' data")
  } else {
    cat("# A cattonum_df2 with the following data:\n'train':\n")
    print(x[["train"]])

    if (has_data(x, "test")) {
      cat("'test':\n")
      print(x[["test"]])
    }
  }
  invisible(x)
}


#' @export
summary.cattonum_df <- function(object, ...) {
  NextMethod()
}


#' @export
summary.cattonum_df2 <- function(object, ...) {
  if (!has_data(object, "train")) {
    return()
  } else {
    train_summary <- summary(object[["train"]])

    if (has_data(object, "test")) {
      test_summary <- summary(object[["test"]])
      return(list(train = train_summary, test = test_summary))
    }
  }
  list(train = train_summary)
}
