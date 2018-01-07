#################
### mat_or_df ###
#################

mat_or_df <- function(.df) {
  all_numerics <- all(vapply(.df, is.numeric, logical(1)))
  if (all_numerics) as.matrix(.df) else .df
}

##########################
### validate_col_types ###
##########################

validate_col_types <- function(.df) {
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

good_col_type <- function(.x) {
  is.numeric(.x) || is.factor(.x) || is.character(.x)
}

####################
### colname_list ###
####################

colname_list <- function(.nms) {
  paste(paste0("'", .nms, "'"), collapse = ", ")
}

####################
### dots_to_char ###
####################

dots_to_char <- function(...) {
  deparse(substitute(...))
}

#################
### pick_cols ###
#################

pick_cols <- function(.df, ...) {
  if (length(substitute(alist(...))) == 1L) {
    all_cats(.df)
  } else {
    nms <- names(.df)
    col_spec <- dots_to_char(...)
    df_name <- deparse(substitute(.df))
    bad_cols_error <- function(e) {
      stop(col_spec, " is not a valid column specification for ",
           df_name, ".", call. = FALSE)
    }
    tryCatch(tidyselect::vars_select(nms, ...),
             error = bad_cols_error)
  }
}

################
### all_cats ###
################

all_cats <- function(.df) {
  nms <- names(.df)
  cats <- vapply(.df, Negate(is.numeric), logical(1L))
  nms[cats]
}

#######################
### ordered_by_freq ###
#######################

ordered_by_freq <- function(.x, .decr = FALSE) {
  names(sort(table(.x), decreasing = .decr))
}

########################
### check_train_test ###
########################

check_train_test <- function(.train, .test) {
  if (!identical(names(.train), names(.test))) {
    stop("`train` and `test` data frames must have the same names.",
         call. = FALSE)
  }
}

##################
### expand_lkp ###
##################

expand_lkp <- function(.dat, .lookup_table) {
  .lookup_table[as.character(.dat), ]
}

#######################
### encode_from_lkp ###
#######################

encode_from_lkp <- function(.d, .l) {
  Map(expand_lkp, .dat = .d, .lookup_table = .l)
}

###