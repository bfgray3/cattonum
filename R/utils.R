#################
### pick_cols ###
#################

pick_cols <- function(.df, ...) {
  if (length(substitute(alist(...))) == 1L) all_cats(.df)
  else tidyselect::vars_select(names(.df), ...)
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