####################
### colname_list ###
####################

colname_list <- function(.nms) {
  paste(sQuote(.nms), collapse = ", ")
}

#################
### pick_cols ###
#################

pick_cols <- function(.df, ...) {
  if (length(substitute(alist(...))) == 1L) {
    all_cats(.df)
  } else {
    nms <- names(.df)
    extra_cols <- nonexistent_cols(nms, ...)
    if (! length(extra_cols)) {
      tidyselect::vars_select(nms, ...)
    } else {
      plural <- length(extra_cols) > 1L
      stop("Cannot encode column", if (plural) "s", " ",
           colname_list(extra_cols), ".", call. = FALSE)
    }
  }
}

########################
### nonexistent_cols ###
########################

nonexistent_cols <- function(.nms, ...) {
  supplied <- vapply(eval(substitute(alist(...))), as.character, character(1L))
  setdiff(supplied, .nms)
}

################
### all_cats ###
################

all_cats <- function(.df) {
  nms <- names(.df)
  cats <- vapply(.df, Negate(is.numeric), logical(1L))
  nms[cats]
}

#################
### ignore_na ###
#################

ignore_na <- function(.f) {
  if (! identical(.f, length)) function(...) .f(...,  na.rm = TRUE)
  else .f
}

###