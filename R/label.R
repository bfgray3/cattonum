# TODO: add support for reverse ordered, random, observed ordered labels

######################
### ordered_labels ###
######################

ordered_labels <- function(.decreasing) {

  function(.x) {

    if (is.factor(.x)) .x <- as.character(.x)
    ordered_labs <- ordered_by_freq(.x, .decreasing)
    lkp <- data.frame(new_lab = seq_along(ordered_labs),
                      row.names = ordered_labs)
    lkp[.x, ]

  }

}

#####################
### random_labels ###
#####################

random_labels <- function(.x, seed = 4444) {

  if (is.factor(.x)) .x <- as.character(.x)
  ordered_labs <- sample(.x)
  lkp <- data.frame(new_lab = seq_along(ordered_labs),
                    row.names = ordered_labs)
  lkp[.x, ]

}

#########################
### appearance_labels ###
#########################

random_labels <- function(.x) {

  if (is.factor(.x)) .x <- as.character(.x)
  ordered_labs <- unique(.x)
  lkp <- data.frame(new_lab = seq_along(ordered_labs),
                    row.names = ordered_labs)
  lkp[.x, ]

}

###################
### incr_labels ###
###################

incr_labels <- ordered_labels(FALSE)

###################
### decr_labels ###
###################

decr_labels <- ordered_labels(TRUE)

###################
### catto_label ###
###################

catto_label <- function(dataframe,
                        ...,
                        ordering = c("increasing",
                                     "decreasing",
                                     "observed",
                                     "random"),
                        verbose = TRUE) {

  validate_col_types(dataframe)

  nms <- names(dataframe)

  ordering <- match.arg(ordering)

  label_maker <- switch(ordering,
                        increasing = incr_labels,
                        decreasing = decr_labels,
                        observed = appearance_labels,
                        random = random_labels)

  cats <- pick_cols(dataframe, ...)
  dataframe[cats] <- lapply(dataframe[cats], label_maker)

  mat_or_df(dataframe)

}

###