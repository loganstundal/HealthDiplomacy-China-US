#-----------------------------------------------------------------------------#
# Dynamic effect functions for health diplomacy analysis
#-----------------------------------------------------------------------------#
# Author : Logan Stundal
# Email  : stundal@virginia.edu
# Date   : March 20, 2023
#-----------------------------------------------------------------------------#


#-----------------------------------------------------------------------------#
# Conditional effects
#-----------------------------------------------------------------------------#
conditional_eff <- function(m, z, vcv = NULL){

  bs <- coef(m)
  bs <- bs[c(names(bs)[str_detect(names(bs), "_lag")], z)]

  if(!is.null(vcv)){
    vcv <- vcv[names(bs), names(bs)]
    bs  <- MASS::mvrnorm(n = 1, mu = bs, Sigma = vcv)
  }

  phi <- bs[str_detect(names(bs), "_lag") & !str_detect(names(bs), ":")]
  bz  <- bs[str_detect(names(bs), ":")]

  if(class(m) == "lm"){
    zv <- range(model.matrix(m)[,z])
  } else{
    zv <- range(m$X[, z])
  }

  zv <- seq(zv[1], zv[2], length.out = 100)

  return(phi + bz * zv)
}
#-----------------------------------------------------------------------------#


#-----------------------------------------------------------------------------#
# Half life decay
#-----------------------------------------------------------------------------#
half_life <- function(m, x, phi, context = NULL, vcv = NULL, decay = 0.5){
  bs <- coef(m)
  z  <- paste(phi, x, sep = ":")
  bs <- bs[c("rho", phi, x, z)]
  bs <- bs[!is.na(bs)]

  if(!is.null(vcv)){
    vcv <- vcv[names(bs), names(bs)]
    bs  <- MASS::mvrnorm(n = 1, mu = bs, Sigma = vcv)
  }

  if(!is.null(context)){
    phi <- bs[[phi]] + bs[[z]] * context
  } else{
    phi <- bs[[phi]]
  }

  if(class(m) == "Sarlm"){
    dependence <- bs[["rho"]] + phi
  } else{
    dependence <- phi
  }

  res <- log(1 - decay) / log(dependence)

  return(res)
}
#-----------------------------------------------------------------------------#


#-----------------------------------------------------------------------------#
# Long run steady state
#-----------------------------------------------------------------------------#
#---------------------------#
# temporal model
#---------------------------#
lrss_time <- function(m, x, phi, context = NULL, vcv = NULL){

  bs <- coef(m)

  if(!is.null(context)){
    bs <- bs[c(phi, x, paste(phi, x, sep = ":"))]
  } else{
    bs <- bs[c(phi, x)]
  }

  if(!is.null(vcv)){
    vcv <- vcv[names(bs), names(bs)]
    bs  <- MASS::mvrnorm(n = 1, mu = bs, Sigma = vcv)
  }

  if(!is.null(context)){
    bz  <- bs[[paste(phi, x, sep = ":")]]
    phi <- bs[[phi]]
    phi <- phi + bz * context
  } else{
    phi <- bs[[phi]]
  }

  bx <- bs[[x]]

  return( bx / (1 - phi))
}
#---------------------------#

#---------------------------#
# spacetime model
#---------------------------#
lrss_star <- function(m, x, phi,
                      context = NULL,
                      ev_w, ev_l,
                      sim_se = FALSE){
  require(spatialreg)

  if(class(m) != "Sarlm"){stop("Dumbass - STAR model!")}

  bs <- coef(m)
  bs <- bs[c("rho", phi, x, paste(phi, x, sep = ":"))]

  if(sim_se){
    vcv <- vcov(m)[names(bs), names(bs)]
    bs  <- MASS::mvrnorm(n = 1, mu = bs, Sigma = vcv)
  }

  bz  <- bs[[paste(phi, x, sep = ":")]]
  phi <- bs[[phi]]
  phi <- phi + bz * context
  bx  <- bs[[x]]
  rho <- bs[["rho"]]

  res <- tibble(
    "direct"   = (bx * sum(1 / (1 - (rho * ev_w + phi * ev_l)))) / length(ev_w),
    "total"    = direct / (1 - rho - phi),
    "indirect" = total - direct) %>%
    select(direct, indirect, total)

  return(res)
}
#---------------------------#
#-----------------------------------------------------------------------------#


#-----------------------------------------------------------------------------#
# marginal response
#-----------------------------------------------------------------------------#
#---------------------------#
# temporal model
#---------------------------#
mr_time <- function(m, x, phi,
                    context = NULL,
                    steps   = 0:10,
                    vcv     = NULL,
                    cumulative = FALSE){
  bs <- coef(m)

  if(!is.null(context)){
    bs <- bs[c(phi, x, paste(phi, x, sep = ":"))]
  } else{
    bs <- bs[c(phi, x)]
  }

  if(!is.null(vcv)){
    vcv <- vcv[names(bs), names(bs)]
    bs  <- MASS::mvrnorm(n = 1, mu = bs, Sigma = vcv)
  }

  if(!is.null(context)){
    bz  <- bs[[paste(phi, x, sep = ":")]]
    phi <- bs[[phi]]
    phi <- phi + bz * context
  } else{
    phi <- bs[[phi]]
  }

  bx  <- bs[[x]]

  res <- (phi^steps) * bx

  if(cumulative){res <- cumsum(res)}

  return(res)
}
#---------------------------#

#---------------------------#
# spacetime model - full response matrix
#---------------------------#
mr_star <- function(m, x, phi,
                    context = NULL,
                    wmat, lmat,
                    sim_se  = FALSE,
                    cumulative = FALSE){
  require(spatialreg)

  if(class(m) != "Sarlm"){stop("Dumbass - STAR model!")}

  bs <- coef(m)
  bs <- c("rho" = as.numeric(m$rho), bs[c(phi, x, paste(phi, x, sep = ":"))])

  if(sim_se){
    vcv <- vcov(m)[names(bs), names(bs)]
    bs  <- MASS::mvrnorm(n = 1, mu = bs, Sigma = vcv)
  }

  bz  <- bs[[paste(phi, x, sep = ":")]]
  phi <- bs[[phi]]
  phi <- phi + bz * context
  bx  <- bs[[x]]
  rho <- bs[["rho"]]

  bm <- bx * solve(diag(nrow(wmat)) - rho * wmat - phi * lmat)
  rownames(bm) <- colnames(bm) <- rownames(wmat)

  get_me <- function(tgt, bm){
    tgt0 <- str_remove(tgt, "[0-9]{4}")
    direct <- bm[str_starts(rownames(bm), tgt0), tgt]
    indirect <- bm[!str_starts(rownames(bm), tgt0),
                   (str_detect(colnames(bm), "_2005") & (colnames(bm) != tgt))] %>%
      apply(., 1, sum)

    res <- tibble(
      res = indirect,
      id  = names(indirect)) %>%
      separate_wider_delim(., cols = id, delim = "_", names = c("ccode", "year")) %>%
      group_by(year) %>%
      summarize(indirect = median(res), .groups = "keep") %>%
      ungroup %>%
      mutate(direct = direct, total = direct + indirect)


    res <- res %>% pivot_longer(.,
                                cols = !year,
                                names_to = "effect", values_to = "val")

    return(res)
  }

  bm <- get_me(tgt = "20_2005", bm = bm) %>%
    pivot_wider(., id_cols = year, names_from = effect, values_from = val)

  if(cumulative){bm <- bm %>% mutate(across(.cols = !year, cumsum))}

  return(bm)
}
#---------------------------#

#---------------------------#
# spacetime model - eigenvalue method
#---------------------------#
mr_star_ev <- function(m, x, phi,
                       context = NULL,
                       evw, evl,
                       sim_se  = FALSE,
                       t_units, t1obs,
                       cumulative = FALSE){
  require(spatialreg)

  if(class(m) != "Sarlm"){stop("Dumbass - STAR model!")}

  bs <- coef(m)
  bs <- c("rho" = as.numeric(m$rho), bs[c(phi, x, paste(phi, x, sep = ":"))])

  if(sim_se){
    vcv <- vcov(m)[names(bs), names(bs)]
    bs  <- MASS::mvrnorm(n = 1, mu = bs, Sigma = vcv)
  }

  bz  <- bs[[paste(phi, x, sep = ":")]]
  phi <- bs[[phi]]
  phi <- phi + bz * context

  bx  <- bs[[x]]
  rho <- bs[["rho"]]

  ev_calc    <- bx * (1 - rho * evw - phi * evl)^-1
  adjustment <-  (phi - rho)^(0:(t_units-1))

  # direct mr approximation
  direct <- sum(ev_calc) / t_units / t1obs
  direct <- adjustment * direct

  # total mr approximation
  # total <- adjustment * (sum(ev_calc / (1 - phi - rho)) / t_units / t1obs)
  total <- adjustment * (sum(ev_calc / (1 - (phi - rho))) / t_units / t1obs)

  # indirect mr approximation
  indirect <- total - direct

  res <- tibble(
    years    = 1:t_units,
    direct   = direct,
    indirect = indirect,
    total    = total
  )

  if(cumulative){
    res <- res %>% mutate(across(.cols = !years, cumsum))
  }
  return(res)
}
#---------------------------#

#---------------------------#
# probit cubic polynomial response
#---------------------------#
probit_response <- function(m, vcv = NULL, sims = NULL, ci = 0.95){
  tt <- range(model.frame(m)["poly1"])
  tt <- tt[1]:tt[2]

  b <- coef(m)

  x <- model.matrix(m)
  x <- apply(x, 2, median) %>% replicate(length(tt), .) %>% t

  x[,"regionSub-Saharan Africa"] <- 1
  x[,"poly1"] <- tt
  x[,"poly2"] <- tt^2
  x[,"poly3"] <- tt^3

  x0 <- x
  x1 <- x

  xvar <- names(b)[str_detect(names(b), "mil_health")]
  x0[,xvar] <- 0
  # x1[,xvar] <- c(rep(0, 4), rep(1, length(tt)-4))
  x1[,xvar] <- 1

  if(!is.null(sims)){

    if(is.null(vcv)){stop("Need variance-covariance matrix.")}

    res <- matrix(data = NA, nrow = length(tt) * 3, ncol = sims)

    for(i in 1:sims){
      bs <- MASS::mvrnorm(n = 1, mu = b, Sigma = vcv)

      xb0 <- x0 %*% bs
      xb1 <- x1 %*% bs

      p0 <- as.vector(pnorm(xb0))
      p1 <- as.vector(pnorm(xb1))
      fd <- p1 - p0

      res[,i] <- c(p0,p1,fd)
    }
    a  <- (1 - ci) / 2
    a2 <- 1 - a

    res <- apply(res, 1, quantile, probs = c(a, 0.5, a2)) %>% t
    colnames(res) <- c("lb", "md", "ub")

    out <- bind_cols(
      tt = rep(tt, 3),
      vr = rep(c("p0", "p1", "fd"), each = length(tt)),
      res
    )
  } else{

    xb0 <- x0 %*% b
    xb1 <- x1 %*% b

    p0  <- as.vector(pnorm(xb0))
    p1  <- as.vector(pnorm(xb1))
    fd <- p1 - p0

    out <- data.frame(
      tt = rep(tt, 3),
      vr = rep(c("p0", "p1", "fd"), each = length(tt)),
      vl = c(p0, p1, fd))
  }
  return(out)
}
#---------------------------#
#-----------------------------------------------------------------------------#



