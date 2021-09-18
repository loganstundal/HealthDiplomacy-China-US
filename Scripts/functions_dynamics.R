require(dplyr)
require(tidyr)

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


dynamics <- function(model, dv, iv,
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

