#################
### pick_cols ###
#################

pick_cols <- function(.df, ...) {
  if (length(substitute(alist(...))) == 1) all_cats(.df)
  else intersect(names(.df), tidyselect::vars_select(...))
}

################
### all_cats ###
################

all_cats <- function(.df) {
  nms <- names(.df)
  cats <- vapply(.df, Negate(is.numeric), logical(1))
  nms[cats]
}

#################
### ignore_na ###
#################

ignore_na <- function(f) {
  if (! identical(f, length)) function(...) f(...,  na.rm = TRUE)
  else f
}

###