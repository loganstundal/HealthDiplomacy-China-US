require(dplyr)
require(tidyr)

# ----------------------------------- #
# Response curve
# ----------------------------------- #
# Function to compute response curve
response_curve <- function(time_periods,
                           coef){

  coef_lag <- coef[1]
  coef_x   <- coef[2]

  # Create empty vectors to store data
  temporary_shock <- rep(0,length(time_periods))
  permanent_shock <- rep(0,length(time_periods))

  # Calculate responses
  temporary_shock <- coef_x * coef_lag^(time_periods-1) # beta shock
  permanent_shock <- cumsum(temporary_shock)    # beta shift

  # Store results in data frame and return
  res <- data.frame("time" = time_periods,
                    temporary_shock, permanent_shock)

  # res <- bind_rows(res, data.frame("time" = 0,
  #                                  temporary_shock = 0,
  #                                  permanent_shock = 0))
  # res <- res %>% arrange(time)
  return(res)
}
# ----------------------------------- #


# ----------------------------------- #
# Dynamic response w/ parametric simulation
# ----------------------------------- #
dynamics <- function(model, dvl, iv,
                     model_vcv= NULL,
                     sims     = 1000,
                     max_time = 10,
                     ci       = 0.95,
                     quiet    = FALSE,
                     tidy     = TRUE){

  if(is.null(model_vcv)){model_vcv = vcov(model)}

  # Create empty matrices to store simulated values:
  simulated_temp <- matrix(NA, nrow = sims, ncol = max_time)
  simulated_perm <- matrix(NA, nrow = sims, ncol = max_time)

  # Time periods
  time   <- seq(from = 1, to = max_time, by = 1)

  # Coefficients
  coeffs <- coef(model)[c(dvl, iv)]

  # Point estimates of the response curve
  response <- response_curve(time = time,
                             coef = coeffs)

  # Simulation for confidence intervals
  for(i in 1:sims){
    if(!quiet){cat(sprintf("\14Simulation %s of %s", i, sims))}
    sim_coeffs <- MASS::mvrnorm(n     = 1,
                                mu    = coeffs,
                                Sigma = model_vcv)

    # Estimate response curve using simulated coefficients
    tmp_response = response_curve(time = time,
                                  coef = sim_coeffs)

    # Store simulated values in previously created matrices
    simulated_temp[i,] = tmp_response$temporary_shock
    simulated_perm[i,] = tmp_response$permanent_shock
  }

  # Compute confidence intervals
  temporary_se <- apply(simulated_temp, 2, sd)
  permanent_se <- apply(simulated_perm, 2, sd)

  # Critical value
  crit <- qnorm((1 - ci)/2 + ci)

  # Construct return data object
  plt_data <- (cbind(response,
                     temporary_se,
                     permanent_se)) %>%
    mutate(temporary_lb = temporary_shock - crit * temporary_se,
           temporary_ub = temporary_shock + crit * temporary_se,
           permanent_lb = permanent_shock - crit * permanent_se,
           permanent_ub = permanent_shock + crit * permanent_se) %>%
    dplyr::select(time,
                  temporary_lb, temporary_shock, temporary_ub, temporary_se,
                  permanent_lb, permanent_shock, permanent_ub, permanent_se)

  # Tidy for ggplot grouping
  if(tidy){
    plt_data <- plt_data %>%
      pivot_longer(.,
                   cols      = !time,
                   names_to  = c("effect", "var"),
                   names_sep = "_",
                   values_to = "value") %>%
      pivot_wider(.,
                  id_cols     = c(time, effect),
                  names_from  = "var",
                  values_from = "value")
  }
  return(plt_data)
}
# ----------------------------------- #


# ----------------------------------- #
# LRSS & Half-Life calculations
# ----------------------------------- #
lrss <- function(b_dvl, b_iv, sims = 1e3, vcv, half_life = NULL,
                 ci = 0.95,
                 log_transform = FALSE){

  crit <- qnorm(ci + (1-ci)/2)

  res <- MASS::mvrnorm(n     = sims,
                       mu    = c(b_dvl, b_iv),
                       Sigma = vcv)
  res2 <- res %>%
    as.data.frame %>%
    rename(dvl = 1, iv = 2) %>%
    mutate(lrss = iv / (1 - dvl)) %>%
    summarize(lrss_lb   = mean(lrss) - ci * sd(lrss),
              lrss_mean = mean(lrss),
              lrss_ub   = mean(lrss) + ci * sd(lrss))

  if(log_transform){
    res2 <- lapply(res2, function(b){(exp(b) - 1) * 100})
  }

  if(!is.null(half_life)){
    log(1-half_life)/log(mean(res[,1])) # around 2.3 years

    hl <- log(1 - half_life) / log(res[,1])

    hl <- data.frame(
      "hl_lb" = mean(hl) - ci * sd(hl),
      "hl"    = mean(hl),
      "hl_ub" = mean(hl) + ci * sd(hl)
    )

    return(list("lrss"      = res2,
                "half_life" = hl))
  } else{
    return(res2)
  }
}
# ----------------------------------- #


# ----------------------------------- #
# Quadratic effect for model sqare terms
# ----------------------------------- #
quadratic <- function(model, variable,
                      sims = 1e3,
                      ci   = 0.95,
                      vcv){

  crit <- qnorm(ci + (1-ci)/2)

  # Collect parameters
  bs <- coef(model)
  bs <- bs[str_detect(names(bs), variable)]
  # Ensure squared term is in spot 2
  bs <- bs[names(bs) %>% sort %>% rev]

  # Collect variances and covariances
  vcv <- vcv[names(bs), names(bs)]

  # Collect independent variable
  x <- model.matrix(model)[,variable]
  x <- seq(min(x), max(x), length.out = 100)

  # Simulation
  y_pred <- matrix(NA, nrow = sims, ncol = 100)
  dydx   <- matrix(NA, nrow = sims, ncol = 100)
  vertex <- matrix(NA, nrow = sims, ncol = 1)
  for(i in 1:sims){
    b_sim <- mvrnorm(n = 1, mu = bs, Sigma = vcv)
    yp    <- b_sim[1] * x + b_sim[2] * x^2
    dx    <- b_sim[1] + 2 * b_sim[2] * x
    vtx   <- -b_sim[1] / (2 * b_sim[2])

    y_pred[i, ] <- yp
    dydx[i, ]   <- dx
    vertex[i, 1]<- vtx
  }

  # Tidy simulation results
  res_pred <- data.frame(
    "x"      = x,
    "y_pred" = apply(y_pred, 2, mean),
    "se"     = apply(y_pred, 2, sd)
  ) %>%
    mutate(lb = y_pred - crit * (se / sqrt(100)),
           ub = y_pred + crit * (se / sqrt(100)))

  res_dydx <- data.frame(
    "x"    = x,
    "dydx" = apply(dydx, 2, mean),
    "se"   = apply(dydx, 2, sd)
  ) %>%
    mutate(lb = dydx - crit * se,
           ub = dydx + crit * se)

  vertex <- data.frame(
    "lb"   = mean(vertex) - crit * sd(vertex),
    "mean" = mean(vertex),
    "ub"   = mean(vertex) + crit * sd(vertex)
  )

  res <- list(
    "res_pred" = res_pred,
    "res_dydx" = res_dydx,
    "vertex"   = vertex
  )

  return(res)
}
# ----------------------------------- #
