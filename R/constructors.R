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
    if (!is.data.frame(x))
      stop("`x` must be a data.frame or a tibble.", call. = FALSE)
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

  # could pass list of dfs
  # individual dfs
  # or nothing


}
