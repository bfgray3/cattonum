#' cattonum: Encode Categorical Features
#'
#' Functions for dummy encoding, frequency encoding,
#' label encoding, leave-one-out encoding, mean encoding,
#' median encoding, and one-hot encoding.
#'
#' @docType package
#' @name cattonum
NULL

#' @useDynLib cattonum, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL

#' @importFrom tibble tibble is_tibble
NULL

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "cattonum is seeking a new maintainer; please respond if interested",
    ": https://github.com/bfgray3/cattonum/issues/40"
  )
}
