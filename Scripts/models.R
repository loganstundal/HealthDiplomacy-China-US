#-----------------------------------------------------------------------------#
#
# Author:        Logan Stundal
# Date:          September 16, 2021
# Purpose:       Analysis
#   - For APSA 2021
#
# Copyright (c): Logan Stundal, 2021
# Email:         stund005@umn.edu
#
#-----------------------------------------------------------------------------#
#
# Notes:
#
#
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# ADMINISTRATIVE                                                          ----
#-----------------------------------------------------------------------------#
#---------------------------#
# Clear working environment
#---------------------------#
rm(list = ls())
#---------------------------#

#---------------------------#
# Load required packages
#---------------------------#
library(tidyverse)
# library(sf)
# library(spdep)
# library(spatialreg)
library(sandwich)
library(texreg)
library(lmtest)
# library(urca)
library(MASS)
#---------------------------#

#---------------------------#
# Load data
#---------------------------#
load("Data/HealthDiplomacy.Rdata")
#---------------------------#

#---------------------------#
# Local functions
#---------------------------#
# knn <- function(mat, nn){
#   minx <- apply(mat, 1, function(x){sort(as.numeric(x))[nn]})
#   for(i in 1:length(minx)){
#     tgt <- minx[[i]]
#     mat[i,] <- as.numeric(mat[i, ] <= tgt)
#   }
#   return(mat)
# }
source("Scripts/functions-dynamics.R")
#---------------------------#
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# DATA ADMIN                                                              ----
#-----------------------------------------------------------------------------#
d <- d %>%
  filter(!cname %in% c("United States", "China"),
         !year  %in% c(as.character(2000:2002)))
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# MODELS                                                                  ----
#-----------------------------------------------------------------------------#
# ----------------------------------- #
# DAH Formulas
# ----------------------------------- #
forms <- list(
  "base" = . ~ 1 + cregion + year,
  "full" = . ~ 1 + cregion + year +
    vdem + I(vdem^2) +
    ally_nonagg +
    # ally_US.nonagg + ally_CN.nonagg +
    # wealth_exports_ln + wealth_imports_ln +
    wealth_gdp_ln + pop_ln +
    dem_life_expec_ln + dem_inf_death_rate_ln +
    dis_daly_ncd_percent + dis_daly_cd_percent
)
# ----------------------------------- #


# ----------------------------------- #
# Testing autocorrelation
# ----------------------------------- #
tmp <- lapply(c("dah_per_chn", "dah_per_usa"), function(dv){
  f <- update(forms$full, sprintf("%s ~ .", dv))
  lm(f, d)
  # rlm(formula = f,
  #     data    = d,
  #     psi     = psi.huber,
  #     maxit   = 200)
})
names(tmp) <- c("chn","usa")
screenreg(tmp, omit.coef = "cregion|cname|year")

# Durbin Watson -- null hypothesis that autocorrelation in model residuals is 0.
lapply(tmp, dwtest) # China - no; USA - yes

# Breusch-Godfrey Lagrange Multiplier test
# Tests a null of no serial correlation against alternative hypotheses of
# serial correlation of some other order (lag structure)
lapply(tmp, function(x){
  coeftest(bgtest(x, order = 1, fill = NA)) %>% tail(n = 1)
})
# China - no; USA - yes

# Augmented Dicky Fueller - integrated series test
  # lapply(tmp, function(x){
  #   ur.df(x$model[,1], type = "none", lags = 1) %>%
  #     summary
  #   })

rm(tmp)
# ----------------------------------- #


# ----------------------------------- #
# Models
# ----------------------------------- #
# Note - although the DW and BG tests do not support temporal autoregressive
# processes in the China aid data, I include a lag here for comparability and
# consistency with the US model. Including this lag does not substantively
# change the results.

# Note2 -- the lags of other power spending are not significant but included to
# account for possible delayed responses. Dropping these does not substantively
# change the results.
chn <- lapply(forms, function(f){
  lm(formula = update(f, dah_per_chn ~ dah_per_chn_lag + dah_per_usa + dah_per_usa_lag + .),
     data    = d)
  # rlm(formula = update(f, dah_per_chn ~ dah_per_chn_lag + dah_per_usa_lag + .),
  #     data    = d,
  #     psi     = psi.huber,
  #     maxit   = 200)
})

usa <- lapply(forms, function(f){
  lm(formula = update(f, dah_per_usa ~ dah_per_usa_lag + dah_per_chn + dah_per_chn_lag + .),
     data    = d)
  # rlm(formula = update(f, dah_per_usa ~ dah_per_usa_lag + dah_per_chn_lag + .),
  #     data    = d,
  #     psi     = psi.huber,
  #     maxit   = 200)
})

mods <- c(usa, chn)
names(mods) <- expand.grid(c("base","full"), c("usa","chn")) %>%
  mutate(nm = paste(Var2, Var1, sep = "_")) %>% pull(nm)

res <- lapply(mods, function(mod){
  cfs <- coefficients(mod)
  vcv <- vcovPC(mod, cluster = ~ cname + year)
  ses <- vcv %>% diag %>% sqrt
  # ses <- NeweyWest(mod, adjust = T, lag = 1) %>% sqrt %>% diag
  # ses <- vcovCL(mod, cluster = ~ cname + year, type = "HC0") %>% sqrt %>% diag
  # ses <- vcovHC(mod, type = "HC0") %>% sqrt %>% diag
  # ses <- vcovHAC(mod) %>% sqrt %>% diag
  tst <- cfs / ses
  n <- nrow(model.matrix(mod))
  k <- ncol(model.matrix(mod))
  pvl <- 2*pt(abs(tst), df=n-k,lower.tail= FALSE)

  colnames(vcv) <- rownames(vcv) <- names(cfs)

  return(list("ses" = ses,
              "pvl" = pvl,
              "vcv" = vcv))
})

ses <- res %>% map("ses")
pvl <- res %>% map("pvl")
vcv <- res %>% map("vcv")

screenreg(mods,
          omit.coef        = c("cregion|cname|year"),
          override.se      = ses,
          override.pvalues = pvl,
          # ci.force         = TRUE,
          custom.header    = list("United States" = 1:2, "China" = 3:4),
          custom.gof.rows  = list("Region FE" = rep("Yes", 4),
                                  "Year FE"   = rep("Yes", 4)),
          custom.note      = paste0("%stars\n",
                                    "Panel corrected standard errors clustered on country and year in parentheses."))

htmlreg(mods,
        omit.coef        = c("cregion|cname|year"),
        override.se      = ses,
        override.pvalues = pvl,
        custom.header    = list("United States" = 1:2, "China" = 3:4),
        file             = "Results/Tables/APSA-2021_DAH-Models.doc",
        custom.gof.rows  = list("Region FE" = rep("Yes", 4),
                                "Year FE"   = rep("Yes", 4)),
        custom.note      = paste0("%stars\n",
                                  "Panel corrected standard errors clustered on country and year in parentheses."))
# ----------------------------------- #
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# DYNAMICS & INTERPRETATION                                               ----
#-----------------------------------------------------------------------------#
# ----------------------------------- #
# Interpreting democracy parabola:
# ----------------------------------- #
# NOTE - these calculations do NOT take the temporal dynamics of these models
# into account.
dem <- quadratic(model    = mods$usa_full,
                 variable = "vdem",
                 sims     = 1e3,
                 ci       = 0.95,
                 vcv      = vcv$usa_full)

head(dem$res_pred)
head(dem$res_dydx)
dem$vertex

# ggplot(data = dem$res_pred, aes(x=x)) +
#   geom_ribbon(aes(ymin = lb, ymax = ub), fill = "blue", alpha = 0.2) +
#   geom_line(aes(y = y_pred)) +
#   theme_minimal() +
#   geom_vline(aes(xintercept = dem$vertex$mean), linetype = "dotted")
#
# ggplot(data = dem$res_dydx, aes(x=x)) +
#   geom_ribbon(aes(ymin = lb, ymax = ub), fill = "blue", alpha = 0.2) +
#   geom_line(aes(y = dydx)) +
#   theme_minimal()
# ----------------------------------- #


# ----------------------------------- #
# Dynamic responses - setup:
# ----------------------------------- #
sim_years <- 10
sim_ci    <- 0.90
# ----------------------------------- #

# ----------------------------------- #
# China responding to US
# ----------------------------------- #
dvl_chn <- "dah_per_chn_lag"
iv_chn  <- "dah_per_usa"
vcv_chn <- vcv$chn_full[c(dvl_chn, iv_chn), c(dvl_chn, iv_chn)]

dyn_chn_usa <- dynamics(model     = mods$chn_full,
                        dvl       = dvl_chn,
                        iv        = iv_chn,
                        ci        = sim_ci,
                        max_time  = sim_years,
                        model_vcv = vcv_chn,
                        sims      = 1000,
                        tidy      = TRUE)

# Cumulative response - LRSS:
b_chn    <- coef(mods$chn_full)

lrss_chn <- lrss(b_dvl     = b_chn[dvl_chn],
                 b_iv      = b_chn[iv_chn],
                 sims      = 1e3,
                 ci        = 0.95,
                 vcv       = vcv_chn,
                 half_life = 0.9)

# Long-run-steady-state response of Chinese DAH to 1% contemporaneous increase
# in US DAH in the same year
lrss_chn$lrss

# Temporary shocks eventually fade to 0; permanent shocks eventually accumulate
# to the LRSS response. So how long this eventually?
# Most common: time until 90% has faded/accumulated:
# Calculate the 90% life of effect: i.e., time until 90% of the effect of a
# shock fades away:
lrss_chn$half_life
# ----------------------------------- #


# ----------------------------------- #
# US responding to China
# ----------------------------------- #
dvl_usa <- "dah_per_usa_lag"
iv_usa  <- "dah_per_chn"
vcv_usa <- vcv$usa_full[c(dvl_usa, iv_usa), c(dvl_usa, iv_usa)]

dyn_usa_chn <- dynamics(model     = mods$usa_full,
                        dvl       = dvl_usa,
                        iv        = iv_usa,
                        ci        = 0.90,
                        max_time  = sim_years,
                        model_vcv = vcv_usa,
                        sims      = 1000,
                        tidy      = TRUE)

# Cumulative response - LRSS:
b_usa    <- coef(mods$usa_full)

lrss_usa <- lrss(b_dvl     = b_usa[dvl_usa],
                 b_iv      = b_usa[iv_usa],
                 sims      = 1e3,
                 ci        = 0.95,
                 vcv       = vcv_usa,
                 half_life = 0.9)

# Long-run-steady-state response of Chinese DAH to 1% contemporaneous increase
# in US DAH in the same year
lrss_usa$lrss

# Temporary shocks eventually fade to 0; permanent shocks eventually accumulate
# to the LRSS response. So how long this eventually?
# Most common: time until 90% has faded/accumulated:
# Calculate the 90% life of effect: i.e., time until 90% of the effect of a
# shock fades away:
lrss_usa$half_life
# ----------------------------------- #


# ----------------------------------- #
# Figure
# ----------------------------------- #
plt_data <- bind_rows("US" = dyn_usa_chn, "China" = dyn_chn_usa, .id = "model") %>%
  mutate(effect = str_to_sentence(effect))

plt_main <- ggplot(data = plt_data, aes(x = time)) +
  geom_ribbon(aes(ymin = lb, ymax = ub, fill = effect), alpha = 0.2) +
  geom_line(aes(y = shock, color = effect), size = 0.2) +
  facet_wrap(~model) +

  scale_color_manual(values = c("gray40","gray40")) +

  scale_x_continuous(name   = "Years",
                     breaks = seq(1,sim_years,1),
                     labels = as.character(seq(1,sim_years,1))) +

  theme(
    panel.background   = element_rect(fill = NA,       color = "black", size = 0.1),
    strip.background   = element_rect(fill = "gray95", color = "black", size = 0.1),
    # axis.title.x       = element_blank(),
    panel.grid         = element_blank(),
    panel.grid.major.y = element_line(color = "gray80", size = 0.1, linetype = "dotted"),
    legend.title       = element_blank(),
    legend.key.size    = unit(3, "mm"),
    legend.position    = "bottom",
    legend.direction   = "horizontal",
    legend.margin      = margin(-3, 0, 0, 1, "mm"),
    legend.text        = element_text(size = 12/.pt),
    axis.text          = element_text(size = 12/.pt),
    axis.title         = element_text(size = 14/.pt),
    plot.title         = element_text(size = 16/.pt),
    plot.subtitle      = element_text(size = 14/.pt),
    plot.caption       = element_text(size = 12/.pt),
    strip.text         = element_text(size = 14/.pt)
  ) +

  labs(title    = "Great Power: Health aid responsiveness",
       y        = "Health aid allocation change (%)",
       subtitle = "Ten year predicted response to 1% increase in allocations of the other power",
       caption  = "Based on estimates of full model with controls.\nConfidence intervals computed with parametric simulation.")

plt_main

ggsave(plot     = plt_main,
       filename = "Results/Figures/APSA-2021_DAH-Response.png",
       width    = 6.5,
       height   = 3.0,
       units    = "in",
       dpi      = 350)
# ----------------------------------- #
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# HEALTH OUTCOME MODELS                                                   ----
#-----------------------------------------------------------------------------#
# ----------------------------------- #
# Setup: DVs and formula
# ----------------------------------- #
dvs <- c("dem_life_expec", "dem_inf_death_rate",
         "dis_daly_ncd_rate", "dis_daly_cd_rate") %>%
  paste0(., "_ln_diff")

health <- . ~
  vdem + I(vdem^2) + ally_nonagg +
  # gdp_ln + exports_ln + imports_ln +
  wealth_gdp_ln + I(wealth_gdp_ln^2) + pop_ln +
  dah_per_chn +
  dah_per_usa +
  cregion + year
# ----------------------------------- #


# ----------------------------------- #
# Health models - testing autocorrelation
# ----------------------------------- #
# Test for autocorrelation
tmp <- lapply(dvs, function(dv){
  f <- update(health, sprintf("%s ~ .", dv))
  m <- lm(f, data = d)
  return(m)
})

# Durbin Watson -- null hypothesis that autocorrelation in model residuals is 0.
lapply(tmp, dwtest) # All - yes

# Breusch-Godfrey Lagrange Multiplier test
# Tests a null of no serial correlation against alternative hypotheses of
# serial correlation of some other order (lag structure)
lapply(tmp, function(x){
  coeftest(bgtest(x, order = 1, fill = NA)) %>% tail(n = 1)
})
# All - yes
# ----------------------------------- #


# ----------------------------------- #
# Health Models
# ----------------------------------- #
# Fit health models with one dv lag
health_mods <- lapply(dvs, function(dv){
  f <- update(health, sprintf("%s ~ %s_lag + .", dv, dv))
  m <- lm(f, data = d)
  return(m)
})
names(health_mods) <- dvs

res <- lapply(health_mods, function(mod){
  cfs <- coefficients(mod)
  vcv <- vcovPC(mod, cluster = ~ cname + year)
  ses <- vcv %>% diag %>% sqrt
  tst <- cfs / ses
  n <- nrow(model.matrix(mod))
  k <- ncol(model.matrix(mod))
  pvl <- 2*pt(abs(tst), df=n-k,lower.tail= FALSE)

  colnames(vcv) <- rownames(vcv) <- names(cfs)

  return(list("ses" = ses,
              "pvl" = pvl,
              "vcv" = vcv))
})

ses <- res %>% map("ses")
pvl <- res %>% map("pvl")
vcv <- res %>% map("vcv")

screenreg(health_mods,
          digits           = 4,
          omit.coef        = c("cregion|cname|year"),
          override.se      = ses,
          override.pvalues = pvl,
          custom.gof.rows  = list("Region FE" = rep("Yes", 4),
                                  "Year FE"   = rep("Yes", 4)),
          custom.note      = paste0("%stars\n",
                                    "Panel corrected standard errors clustered on country and year in parentheses."))

htmlreg(health_mods,
        digits           = 4,
        omit.coef        = c("cregion|cname|year"),
        override.se      = ses,
        override.pvalues = pvl,
        file             = "Results/Tables/APSA-2021_Health-Models.doc",
        custom.gof.rows  = list("Region FE" = rep("Yes", 4),
                                "Year FE"   = rep("Yes", 4)),
        custom.note      = paste0("%stars\n",
                                  "Panel corrected standard errors clustered on country and year in parentheses."))
# ----------------------------------- #


# ----------------------------------- #
# Health model effects
# ----------------------------------- #
mod <- "dem_inf_death_rate_ln_diff"
m   <- health_mods[[mod]]
dvl <- paste0(mod, "_lag")
iv  <- "dah_per_usa"

health_bs  <- coef(m)[c(dvl, iv)]
health_vcv <- vcv[[mod]][c(dvl,iv),c(dvl,iv)]


lrss_life <- lrss(b_dvl     = abs(health_bs[1]),
                  b_iv      = health_bs[2],
                  sims      = 1e3,
                  ci        = 0.95,
                  vcv       = life_vcv,
                  half_life = 0.9,
                  log_transform = TRUE)

lrss_life

# These are suspiciously small confidence intervals. Likely need to perform
# log_transform PRIOR to post-sampling summarize()


# ALTHOUGH IT WORKS BELOW....?
# ... why do I always, always, ALWAYS forget this.... because I suck.
# https://data.library.virginia.edu/interpreting-log-transformations-in-a-linear-model/
# Log Dv only:
# lapply(health_mods, function(mod){
#   b <- coef(mod)["dah_per_usa"]
#   (exp(b) - 1) * 100
# })

lrss <- function(model, dvl, iv, vcv, sims = 1e3){
  bs     <- coef(model)
  b_sims <- MASS::mvrnorm(n  = sims,
                          mu = c(bs[dvl], bs[iv]),
                          Sigma = vcv)
  res <- b_sims %>%
    as.data.frame %>%

    mutate(lrss = !!sym(iv) / (1 - !!sym(dvl))) %>%
    summarize(lrss_lb   = mean(lrss) - 1.96 * sd(lrss),
              lrss_mean = mean(lrss),
              lrss_ub   = mean(lrss) + 1.96 * sd(lrss))
  res <- lapply(res, function(b){(exp(b) - 1) * 100})
  return(res)
}

health_effects <- sapply(dvs, function(dv){
  dvl <- paste0(dv, "_lag")
  iv  <- "dah_per_usa"

  m <- health_mods[[dv]]
  v <- vcv[[dv]][c(dvl, iv), c(dvl, iv)]
  r <- lrss(model = m, dvl = dvl, iv = iv, vcv = v)
  return(r)
})
# ----------------------------------- #
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# SAVE                                                                    ----
#-----------------------------------------------------------------------------#
#save.image()
rm(list = ls())
#-----------------------------------------------------------------------------#





#-----------------------------------------------------------------------------#
# SPATIAL ERROR MODELS - THIS IS WHAT WE SHOULD DO!                       ----
#-----------------------------------------------------------------------------#
# ----------------------------------- #
# Setup
# ----------------------------------- #
# Nb. DAH questionable for China before 2002
yrs <- as.character(unique(d$year))
# ----------------------------------- #


# ----------------------------------- #
# Create W & LW
# ----------------------------------- #
w <- lapply(yrs, function(yr){
  ids  <- d %>% filter(year == yr) %>% pull(ccode) %>% as.character()
  wtmp <- cshapes::distmatrix(date  = as.Date(sprintf("%s-06-01",yr)),
                              type  = "centdist",
                              useGW = FALSE)
  wtmp <- wtmp[ids, ids]
  wtmp <- knn(mat = wtmp, nn = 3)
  return(wtmp)
})

w <- Matrix::bdiag(w)

lw <- mat2listw(w, style = "W")
# ----------------------------------- #


# ----------------------------------- #
# Spatial Error Models
# ----------------------------------- #
lmtests <- lapply(mods[c(2,4)], function(m){
  lm.LMtests(model = m, listw = lw, test = "all")
})
lapply(lmtests, summary)

# Interesting... the China model supports spatial lag process while the US
# model supports a spatial error process.

forms_sp <- list(
  "chn" = update(forms$full,
                 dah_per_chn ~ dah_per_chn_lag + dah_per_usa + dah_per_usa_lag + .),
  "usa" = update(forms$full,
                 dah_per_usa ~ dah_per_usa_lag + dah_per_chn + dah_per_chn_lag + .)
)

spe <- lapply(forms_sp, function(x){
  errorsarlm(formula = x, data = d, listw = lw)
})

screenreg(spe, omit.coef = "cregion|cname|year")
# ----------------------------------- #
#-----------------------------------------------------------------------------#

rm(list=ls())
