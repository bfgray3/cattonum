#' Title
#'
#' @param x
#'
#' @return
#'
#' @examples
#' cattonum_df(iris)
#' cattonum_df()
#' @export
cattonum_df <- function(x) {
  if (!missing(x)) {
    if (!is.data.frame(x)) {
      stop("`x` must be a data.frame or a tibble.", call. = FALSE)
    }
    structure(x, class = c("cattonum_df", class(x)))
  } else {
    structure(data.frame(), class = c("cattonum_df", "data.frame"))
  }
}

#' Title
#'
#' @param ...
#'
#' @return
#'
#' @examples
#' cattonum_dfs(iris)
#' cattonum_dfs(iris, mtcars)
#' cattonum_dfs()
#' @export
cattonum_dfs <- function(...) {
  num_args <- nargs()

  if (!num_args) {
    return(structure(list(), class = "cattonum_dfs"))
  }

  x <- list(...)

  if (identical(num_args, 1L)) {
    # TODO: could be a list holding one df???
    if (!is.data.frame(x[[1L]])) {
      stop("If passing a single argument to cattonum_dfs(), it must be a data.frame or a tibble.",
        call. = FALSE
      )
    }
    structure(x, class = "cattonum_dfs")
  } else {
    # TODO: check class of elements of x?
    structure(x, class = "cattonum_dfs")
  }
}
