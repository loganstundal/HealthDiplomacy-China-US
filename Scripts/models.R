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
# library(dynsim)
library(sandwich)
library(texreg)
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
source("Scripts/functions_dynamics.R")
#---------------------------#
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# DATA ADMIN                                                              ----
#-----------------------------------------------------------------------------#
d   <- d %>%
  filter(!cname %in% c("United States", "China"),
         !year  %in% c(as.character(2000:2002))) %>%
  arrange(year, ccode)
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# MODELS                                                                  ----
#-----------------------------------------------------------------------------#
# ----------------------------------- #
# Formulas
# ----------------------------------- #
# forms <- list(
#   "base" = . ~ -1 + dah_per_usa_lag + dah_per_chn_lag + cname,
#   "full" = . ~ -1 + dah_per_usa_lag + dah_per_chn_lag + cname +
#     vdem +
#     exports_ln + imports_ln + pop_ln +
#     life_expec_ln + daly_ncd_percent + daly_cd_percent
# )

forms <- list(
  "base" = . ~ -1 + cname + year,
  "full" = . ~ -1 + cname + year +
    vdem + I(vdem^2) +
    exports_ln + imports_ln + pop_ln +
    life_expec_ln + daly_ncd_percent + daly_cd_percent
)

# health <- inf_death_ln ~
#   -1 + cname + year +
#   vdem + I(vdem^2) +
#   exports_ln + imports_ln + pop_ln
#   # dah_per_chn + dah_per_usa +
#   # dah_per_chn_lag + dah_per_usa_lag
#
# m <-lm(health, data = d)
# screenreg(m, omit.coef = "cname|year")
# ----------------------------------- #


# ----------------------------------- #
# Models
# ----------------------------------- #
chn <- lapply(forms, function(f){
  lm(formula = update(f, dah_per_chn ~ dah_per_chn_lag + dah_per_usa + dah_per_usa_lag + .),
     data    = d)
})

usa <- lapply(forms, function(f){
  lm(formula = update(f, dah_per_usa ~ dah_per_usa_lag + dah_per_chn + dah_per_chn_lag + .),
     data    = d)
})

mods <- c(usa, chn)
names(mods) <- expand.grid(c("base","full"), c("usa","chn")) %>%
  mutate(nm = paste(Var2, Var1, sep = "_")) %>% pull(nm)

res <- lapply(mods, function(mod){
  cfs <- coefficients(mod)
  vcv <- vcovPC(mod, cluster = ~ cname + year)
  ses <- vcv %>% diag %>% sqrt
  # ses <- NeweyWest(mod, adjust = T, lag = 1) %>% sqrt %>% diag
  # ses <- vcovCL(mod, cluster = ~ cname, type = "HC1") %>% sqrt %>% diag
  tst <- cfs / ses
  n <- nrow(model.matrix(mod))
  k <- ncol(model.matrix(mod))
  pvl <- 2*pt(abs(tst), df=n-k,lower.tail= FALSE)

  return(list("ses" = ses,
              "pvl" = pvl,
              "vcv" = vcv))
})

ses <- res %>% map("ses")
pvl <- res %>% map("pvl")
vcv <- res %>% map("vcv")

screenreg(mods,
          omit.coef        = c("cname|year"),
          override.se      = ses,
          override.pvalues = pvl,
          # ci.force         = TRUE,
          custom.header    = list("United States" = 1:2, "China" = 3:4))

# htmlreg(mods,
#         omit.coef        = c("cname|year"),
#         override.se      = ses,
#         override.pvalues = pvl,
#         custom.header    = list("United States" = 1:2, "China" = 3:4),
#         file             = "Results/Tables/APSA-2021_NewModels.doc")
# ----------------------------------- #
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# DYNAMICS                                                                ----
#-----------------------------------------------------------------------------#
# ----------------------------------- #
# China responding to US
# ----------------------------------- #
dvl     <- "dah_per_chn_lag"
iv      <- "dah_per_usa_lag"
vcv_chn <- vcv$chn_full[c(dvl, iv), c(dvl, iv)]

dyn_chn_usa <- dynamics(model      = mods$chn_full,
                        dv        = dvl,
                        iv        = iv,
                        ci        = 0.90,
                        max_time  = 5,
                        model_vcv = vcv_chn,
                        sims      = 1000,
                        tidy      = TRUE)
# head(dyn_chn_usa)

# Cumulative response - LRSS:
b_chn    <- coef(mods$chn_full)
lrss_chn <- MASS::mvrnorm(n     = 1e3,
                          mu    = c(b_chn[dvl], b_chn[iv]),
                          Sigma = vcv_chn)

lrss_chn2 <- lrss_chn %>% as.data.frame %>%
  mutate(lrss = !!sym(iv) / (1 - !!sym(dvl))) %>%
  summarize(lrss_lb   = mean(lrss) - 1.96 * sd(lrss),
            lrss_mean = mean(lrss),
            lrss_ub   = mean(lrss) + 1.96 * sd(lrss))

lrss_chn2
# lrss_lb  lrss_mean     lrss_ub
# -0.467612 -0.2792118 -0.09081166

# In the long run, Chinese aid will decrease by 0.285 (% of yearly allocation)
# given a one-percent increase in US health aid allocation to a country the year
# prior. (NB, value varies by simulation. set seed)

# Cumulative response half-life:
# Calculate the 90% life of effect: i.e., time until 90% of the effect of a
# shock fades away:
log(1-0.9)/log(mean(lrss_chn[,dvl])) # around 1.3 years

hl_chn <- lapply(lrss_chn[,dvl], function(x){
  log(1-0.9) / log(x)
}) %>% unlist
# ----------------------------------- #


# ----------------------------------- #
# US responding to China
# ----------------------------------- #
dvl     <- "dah_per_usa_lag"
iv      <- "dah_per_chn_lag"
vcv_usa <- vcv$usa_full[c(dvl, iv), c(dvl, iv)]

dyn_usa_chn <- dynamics(model     = mods$usa_full,
                        dv        = dvl,
                        iv        = iv,
                        ci        = 0.90,
                        max_time  = 5,
                        model_vcv = vcv_usa,
                        sims      = 1000,
                        tidy      = TRUE)
# head(dyn_usa_chn)

# Cumulative response - LRSS:
b_usa    <- coef(mods$usa_full)
lrss_usa <- MASS::mvrnorm(n     = 1e3,
                          mu    = c(b_usa[dvl], b_usa[iv]),
                          Sigma = vcv_usa)

lrss_usa <- lrss_usa %>% as.data.frame %>%
  mutate(lrss = !!sym(iv) / (1 - !!sym(dvl))) %>%
  summarize(lrss_lb   = mean(lrss) - 1.96 * sd(lrss),
            lrss_mean = mean(lrss),
            lrss_ub   = mean(lrss) + 1.96 * sd(lrss))

lrss_usa
# lrss_lb   lrss_mean     lrss_ub
# -0.08814593 -0.04151185 0.005122227
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
                     breaks = seq(1,5,1),
                     labels = as.character(seq(1,5,1))) +

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
       y        = "Aid allocation change (%)",
       subtitle = "Five year predicted response to 1% increase in allocations of the other power in the year prior",
       caption  = "Based on estimates of full model with controls.\nConfidence intervals computed with parametric simulation.")

ggsave(plot     = plt_main,
       filename = "Results/Figures/APSA-2021_main.png",
       width    = 6.5,
       height   = 3.0,
       units    = "in",
       dpi      = 350)
# ----------------------------------- #
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# HEALTH OUTCOME MODELS                                                   ----
#-----------------------------------------------------------------------------#
dvs <- c("life_expec", "inf_death", "daly_ncd_rate", "daly_cd_rate")

# Sample: only countries receiving US or Chinese health aid:
d2 <- d %>% filter(dah_per_usa != 0, dah_per_chn != 0,
                   !year %in% as.character(2000:2002)) %>%
  mutate(across(.cols = all_of(dvs),
                .fns  = ~log(.x),
                .names= "{col}_ln")) %>%
  group_by(cname) %>%
  mutate(across(.cols = all_of(c(dvs, paste0(dvs, "_ln"))),
                .fns  = list("lag"      = ~lag(.x, n = 1),
                             "diff"     = ~.x - lag(.x, n = 1),
                             "diff_lag" = ~lag(lag(.x, n = 1), n = 1)),
                .names= "{.col}_{.fn}")) %>%
  ungroup


health <- . ~
  vdem + I(vdem^2) +
  gdp_ln + exports_ln + imports_ln +
  dah_per_chn +
  dah_per_usa +
  cname + year

dvs <- paste0(dvs, "_ln_diff")

health_mods <- lapply(dvs, function(dv){
  f <- update(health, sprintf("%s ~ %s_lag + .", dv, dv))
  # f <- update(health, sprintf("%s ~ .", dv))
  m <- lm(f, data = d2)
  return(m)
})

names(health_mods) <- dvs

res <- lapply(health_mods, function(mod){
  cfs <- coefficients(mod)
  vcv <- vcovCL(mod, cluster = ~ cname)
  ses <- vcv %>% diag %>% sqrt
  tst <- cfs / ses
  n <- nrow(model.matrix(mod))
  k <- ncol(model.matrix(mod))
  pvl <- 2*pt(abs(tst), df=n-k,lower.tail= FALSE)

  return(list("ses" = ses,
              "pvl" = pvl,
              "vcv" = vcv))
})

ses <- res %>% map("ses")
pvl <- res %>% map("pvl")
vcv <- res %>% map("vcv")

screenreg(health_mods,
          digits           = 5,
          omit.coef        = c("cname|year"),
          override.se      = ses,
          override.pvalues = pvl
)


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
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# SAVE                                                                    ----
#-----------------------------------------------------------------------------#
#save.image()
#rm(list = ls())
#-----------------------------------------------------------------------------#





#-----------------------------------------------------------------------------#
# SPATIAL ERROR MODELS - THIS IS WHAT WE SHOULD DO!                       ----
#-----------------------------------------------------------------------------#
# ----------------------------------- #
# Setup
# ----------------------------------- #
d   <- d %>%
  filter(!cname %in% c("United States", "China"),
         !year  %in% c(as.character(2000:2002))) %>%
  arrange(year, ccode)

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

m <- errorsarlm(formula = f_us, data = d, listw = lw)
screenreg(m, omit.coef = "cname|year")
# ----------------------------------- #
#-----------------------------------------------------------------------------#

rm(list=ls())
