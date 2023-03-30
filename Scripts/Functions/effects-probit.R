#-----------------------------------------------------------------------------#
# Marginal effect functions for health diplomacy analysis - probit models
#-----------------------------------------------------------------------------#
# Author : Logan Stundal
# Email  : stundal@virginia.edu
# Date   : March 28, 2023
#-----------------------------------------------------------------------------#


#-----------------------------------------------------------------------------#
# gen_x - return design matrix FOR THIS PROJECT ONLY at mean or medians
#-----------------------------------------------------------------------------#
gen_x <- function(m, xvar = xvar, x_at = x_at){
  x <- model.matrix(m)
  x <- apply(x, 2, mean)
  x[str_starts(names(x), "region")] <- 0
  x["regionSub-Saharan Africa"]     <- 1
  x[str_starts(names(x), "atop")]   <- 0
  x["atop_nonaggUS NonAgg"]         <- 1
  x[str_starts(names(x), "poly")]   <- 1
  x["ged_major"] <- 0
  x[str_starts(names(x), "mil_health")] <- 0

  # Address all squared terms
  x["un_gdppc2"] <- x["un_gdppc"]^2
  x["un_lifeexpec_yrs2"] <- x["un_lifeexpec_yrs"]^2
  x["vdem2"] <- x["vdem"]^2

  xl <- length(x_at)

  # Check for quadratic term
  chk <- names(x)[str_detect(names(x), paste0(xvar, "2"))]

  if(xl > 1){
    x <- replicate(xl, x) %>% rbind %>% t
    x[,xvar] <- x_at
    if(length(chk) > 0){
      x[,paste0(xvar, "2")] <- x_at^2
    }
  } else{
    x[xvar] <- x_at
    if(length(chk) > 0){
      x[paste0(xvar, "2")] <- x_at^2
    }
  }
  return(x)
}
#-----------------------------------------------------------------------------#


#-----------------------------------------------------------------------------#
# probit_dydx ~ calculate derivative on response (probability) scale for x
#-----------------------------------------------------------------------------#
probit_dydx <- function(m, xvar,
                        x_log = FALSE,
                        x_at  = NULL,
                        x_mat = NULL,
                        v     = NULL){

  if(!is.null(x_at) & !is.null(x_mat)){
    message("Both x_at and pre-built x_mat supplied. x_at values in x_mat take priority.")
  }

  if(!is.null(x_mat)){
    if(is.vector(x_mat)){
      x_at <- as.vector(x_mat[xvar])
    } else{
      x_at <- as.vector(x_mat[,xvar])
    }
  } else{
    x_at  <- as.vector(x_at)
    x_mat <- gen_x(m = m, xvar = xvar, x_at = x_at)
  }

  b <- coef(m)
  chk <- names(b)[str_detect(names(b), paste0(xvar, "2"))]

  # NOTE - this is fine here since sampling is wrapped outside the fn call.
  #        However, inefficient - repeats above x,b,chk steps
  if(!is.null(v)){b <- MASS::mvrnorm(n = 1, mu = b, Sigma = v)}

  xb <- x_mat %*% b

  # Calculate xvar coefficient
  if(length(chk)>0){
    bx <- b[xvar] + 2 * b[chk] * x_at
  } else{
    bx <- b[xvar] * 1
  }

  dydx <- dnorm(xb) * bx * 100

  if(x_log){dydx <- dydx * log(1.1)}

  return(dydx)
}
#-----------------------------------------------------------------------------#


#-----------------------------------------------------------------------------#
# probit_prob - calculate the probability of outcome for ob at mean/median in X
#-----------------------------------------------------------------------------#
probit_prob <- function(m, xvar, x_at,
                        x_vec = NULL,
                        v = NULL){
  if(is.null(x_vec)){
    x_vec <- gen_x(m = m, xvar = xvar, x_at = x_at)
  }

  b <- coef(m)

  # NOTE - works since simulation wrapped outside function call
  if(!is.null(v)){b <- MASS::mvrnorm(n = 1, mu = b, Sigma = v)}

  chk <- names(b)[str_detect(names(b), paste0(xvar, "2"))]

  x_vec[xvar] <- x_at

  if(length(chk) > 0){
    x_vec[paste0(xvar, "2")] <- x_at^2
  }

  return(pnorm(x_vec %*% b) * 100)
}
#-----------------------------------------------------------------------------#

