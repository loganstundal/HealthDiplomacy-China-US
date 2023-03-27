#-----------------------------------------------------------------------------#
#
# Author:        Logan Stundal
# Date:          September 24, 2022
# Purpose:       Function to generate L time-lag matrix operator for
#                unbalanced panels. Function is generalizable to generate
#                L-matrices which produce 1:t lags of y for (L %*% y) operations
#
# Copyright (c): Logan Stundal, 2022
# Email:         logan.stundal@gmail.com
#
#-----------------------------------------------------------------------------#


require(dplyr)
require(tidyr)
require(rlang)

#-----------------------------------------------------------------------------#
panel_l <- function(data,
                    id_unit, id_time,
                    n_lag         = 1,
                    first_diff    = FALSE,
                    first_zero    = TRUE,
                    sort_timeunit = TRUE){

  # ----------------------------------- #
  # Options description
  # ----------------------------------- #
  # Complete options
  # data    = a panel data frame (balanced or unbalanced)
  # id_unit = a string identifying variable name for units in the data
  # id_time = a string identifying variable name for times in the data
  # n_lag = the number of lags the matrix should compute (default = 1)
  # first_diff = logical - whether to return a first difference operator
  #              matrix (true) or return a lag matrix (false).
  # first_zero = logical - how the matrix should code the first observation
  #              in operations. E.g., output unit first-lag as zero, or NA when
  #              first_zero = FALSE
  # sort_timeunit = Return L matrix organized by time then unit as common in
  #                 spatial panel analysis (TRUE), or by unit and then
  #                 time (FALSE).

  # Incomplete options
  # force_sequential = INCOMPLETE. Intended to force sequential ordering of
  #                    observations for panels which have dropped NAs units.
  #                    (e.g., for a panel: MN.2001, MN.2004, MN.2005) prevent
  #                    matrix from incorrectly assigning MN.2001 as first lag
  #                    for MN.2004 values.
  # ----------------------------------- #

  id_unit <- rlang::syms(id_unit)
  id_time <- rlang::syms(id_time)

  data <- data %>% arrange(!!!id_time, !!!id_unit) %>%
    mutate(pid = paste(!!!id_unit, !!!id_time, sep = "."))

  # if(force_sequential){
  #   data <- expand_grid("{id_unit}" := data %>% pull(!!!id_unit),
  #                       "{id_time}" := data %>% pull(!!!id_time)) %>%
  #     left_join(data, ., by = c(!!!id_unit, !!!id_time))
  # }

  nd <- data %>%
    arrange(!!!id_unit, !!!id_time)

  n  <- nrow(nd)

  l <- diag(1, nrow = (n - n_lag))
  l <- rbind(matrix(0, nrow = n_lag, ncol = (n- n_lag)), l)
  l <- cbind(l, matrix(0, nrow = n, ncol = n_lag))
  rownames(l) <- colnames(l) <- nd$pid

  # NEED - indices of every FIRST (NAs) or LAST (ZEROs) group index value
  id_last <- nd %>%
    mutate(r_number = row_number()) %>%
    group_by(!!! id_unit)

  if(first_diff){
    if(n_lag > 1){warning(
      "First difference accuracy only tested for n_lag = 1")}
    l <- diag(n) - l
  }

  if(first_zero){
    # LAST  ~ matrix  0s
    if(first_diff){
      id_last <- id_last %>% slice_head(n = n_lag) %>% pull(r_number)
      l[id_last, ] <- 0
    } else{
      id_last <- id_last %>% slice_tail(n = n_lag) %>% pull(r_number)
      l[, id_last] <- 0
    }
  } else{
    # FIRST ~ matrix NAs
    id_last <- id_last %>% slice_head(n = n_lag) %>% pull(r_number)
    l[id_last, id_last] <- NA
  }

  if(sort_timeunit){
    tgt <- data$pid

    l <- l[tgt, tgt]
  }

  return(l)
}
#-----------------------------------------------------------------------------#
