####################
### ordering_fun ###
####################

ordering_fun <- function(.method) {

  switch(.method,
         increasing = function(...) ordered_by_freq(..., .decr = FALSE),
         decreasing = function(...) ordered_by_freq(..., .decr = TRUE),
         observed = unique,
         random = sample)

}

######################
### ordered_labels ###
######################

ordered_labels <- function(.type) {

  function(.x) {

    if (is.factor(.x)) .x <- as.character(.x)
    order_fun <- ordering_fun(.type)
    ordered_labs <- order_fun(.x)
    lkp <- data.frame(new_lab = seq_along(ordered_labs),
                      row.names = ordered_labs)
    lkp[.x, ]

  }

}

###################
### catto_label ###
###################

catto_label <- function(train,
                        ...,
                        ordering = c("increasing",
                                     "decreasing",
                                     "observed",
                                     "random"),
                        test,
                        verbose = TRUE) {

  validate_col_types(train)

  nms <- names(train)

  ordering <- match.arg(ordering)

  cats <- pick_cols(train, ...)
  train[cats] <- lapply(train[cats], ordered_labels(ordering))

  mat_or_df(train)

}

###