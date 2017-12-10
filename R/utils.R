#################
### mat_or_df ###
#################

mat_or_df <- function(.df) {
  all_numerics <- all(vapply(.df, is.numeric, logical(1)))
  if (all_numerics) as.matrix(.df) else .df
}

#####################
### validate_cols ###
#####################

validate_cols <- function(.df) {
  good_cols <- vapply(.df, good_col_type, logical(1L))
  if (! all(good_cols)) {
    bad_cols <- names(df)[! good_cols]
    stop("All columns must be numeric, character, or factor. ",
         colname_list(bad_cols), " are not", call. = FALSE)
  }
}

#####################
### good_col_type ###
#####################

good_col_type <- function(.column) {
  is.numeric(.column) || is.factor(.column) || is.character(.column)
}

####################
### colname_list ###
####################

colname_list <- function(.nms) {
  paste(paste0("'", .nms, "'"), collapse = ", ")
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
  supplied_symbols <- eval(substitute(alist(...)))
  supplied_nms <- vapply(supplied_symbols,
                         as.character,
                         character(1L))
  setdiff(supplied_nms, .nms)
}

################
### all_cats ###
################

all_cats <- function(.df) {
  nms <- names(.df)
  cats <- vapply(.df, Negate(is.numeric), logical(1L))
  nms[cats]
}

###