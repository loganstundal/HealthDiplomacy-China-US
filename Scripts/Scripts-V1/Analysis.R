#-----------------------------------------------------------------------------#
#
# Author:        Logan Stundal
# Date:          March 18, 2021
# Purpose:       Analysis - Early
#
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
# Load required packages
#---------------------------#
library(tidyverse)
library(texreg)
library(kableExtra)
library(sandwich)
library(plm)

#---------------------------#
# Load data
#---------------------------#
# load("data/data-hd-tidy.rdata")
setwd("C:/Users/logan/GoogleDrive/UMN/CLASSES/2021 SPRING/RA - Nisha/Health-Diplomacy")
load("data/data-hd-tidy-new.rdata")

#---------------------------#
# Local functions
#---------------------------#
      # mod_format <- function(mods, save_vcv = TRUE){
      #   vcv  <- lapply(mods, function(x){
      #     ses <- vcovPC(x, cluster = ~ ccode)
      #   })
      #
      #   ses  <- lapply(vcv, function(x){
      #     ses <- sqrt(diag(x))
      #   })
      #
      #   tstats <- lapply(1:length(mods), function(x){
      #     tstat <- abs(coef(mods[[x]]) / ses[[x]])
      #   })
      #
      #   pvals <- lapply(1:length(mods), function(x){
      #     n      <- nrow(mods[[x]][["model"]])
      #     tstats <- tstats[[x]]
      #     k      <- length(tstats)
      #     pvals  <- 2 * pt(tstats, df = n - k, lower.tail = FALSE)
      #   })
      #
      #   if(save_vcv){
      #     res <- list("mods" = mods,
      #                 "ses"  = ses,
      #                 "pvals"= pvals,
      #                 "vcvs" = vcv)
      #   } else{
      #     res <- list("mods" = mods,
      #                 "ses"  = ses,
      #                 "pvals"= pvals)
      #   }
      #   return(res)
      # }

return_p <- function(mod, ses){
  n      <- nrow(mod[["model"]])
  tstats <- abs(coef(mod) / ses)
  k      <- length(tstats)
  pvals  <- 2 * pt(tstats, df = n - k, lower.tail = FALSE)
}

#-----------------------------------------------------------------------------#
# BIVARIATE - GHDx vs AidData                                             ----
#-----------------------------------------------------------------------------#
# cor.test(hd$health.usaid_US, hd$health.ghdx_US)
# cor.test(hd$health.aiddata_China, hd$health.ghdx_China)
#
# # So, based on a very simple model, Aid Data appears to capture about 55 cents
# # on the dollar relative to GHDx.
# summary(lm(health.ghdx_US ~ health.usaid_US, data = hd))
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# MODEL DATA TIDY                                                         ----
#-----------------------------------------------------------------------------#

"
Note - to prevent parameter instability in the models, the health aid data were
normalized based on the scale of the largest value. Ex. largest US is in 100
millions so US series is divided by 1e8, largest China is millions, so China
series is divided by 1e6.
"

# ----------------------------------- #
# ORIGINAL "DAT"
# ----------------------------------- #
dat <- hd %>%
  select(!contains(c("_pht","aiddata","usaid"))) %>%
  mutate(across(starts_with("health"), ~replace_na(.x, 0))) %>%
  mutate(health.ghdx_China = health.ghdx_China / 1e6,
         health.ghdx_US    = health.ghdx_US / 1e7) %>%
  group_by(ccode) %>%
  mutate(across(starts_with("health"), ~.x - dplyr::lag(.x, 1), .names = "{.col}_diff")) %>%
  mutate(across(contains("_diff"), ~dplyr::lag(.x, 1), .names = "{.col}_lag")) %>%
  mutate(across(c(health.ghdx_China, health.ghdx_US), ~dplyr::lag(.x, 1), .names = "{.col}_lag")) %>%
  select(cname, ccode, year, region, starts_with("health"), everything()) %>%
  ungroup() %>%
  filter(ccode!= 625)
# ----------------------------------- #

# ----------------------------------- #
# yoy fuckery
# ----------------------------------- #
# dat <- hd %>%
#   select(!contains(c("_pht","aiddata","usaid"))) %>%
#   mutate(across(starts_with("health"), ~replace_na(.x, 0))) %>%
#   # mutate(health.ghdx_China = health.ghdx_China / 1e6,
#   #        health.ghdx_US    = health.ghdx_US / 1e7) %>%
#   group_by(ccode) %>%
#
#   mutate(across(starts_with("health"), ~dplyr::lag(.x,1), .names = "{.col}_lag")) %>%
#   mutate(across(c(health.ghdx_China, health.ghdx_US), ~100*( (.x - dplyr::lag(.x,1))/dplyr::lag(.x,1) ),
#                 .names = "{.col}_yoy")) %>%
#   mutate(health.ghdx_China_yoy = case_when(health.ghdx_China_lag == 0 ~ 0, TRUE ~ health.ghdx_China_yoy),
#          health.ghdx_US_yoy = case_when(health.ghdx_US_lag == 0 ~ 0, TRUE ~ health.ghdx_US_yoy)) %>%
#   mutate(across(contains("_yoy"), ~dplyr::lag(.x, 1), .names = "{.col}_lag")) %>%
#   mutate(health.ghdx_China_yoy_diff = health.ghdx_China_yoy - health.ghdx_China_yoy_lag,
#          health.ghdx_US_yoy_diff = health.ghdx_US_yoy - health.ghdx_US_yoy_lag) %>%
#   ungroup() %>%
#   filter(ccode!= 625)

# dat <- dat %>% filter(health.ghdx_US_yoy < 1e3)
# ----------------------------------- #


# ----------------------------------- #
# NO BIG AID OUTLIERS FOR US "DAT"
# ----------------------------------- #
  # dat <- dat %>%
  #   filter(health.ghdx_US < 1,
  #          health.ghdx_US < 1)
# ----------------------------------- #


# ----------------------------------- #
# Difference all the things "DAT"
# ----------------------------------- #
  # dat <- dat %>%
  #   mutate(ccode = fct_drop(ccode)) %>%
  #   group_by(ccode) %>%
  #   mutate(across(c(pop_log, vdem_index, gdp_pc_log,
  #                 DALY_NCD_Percent, DALY_CD_Percent), ~.x - dplyr::lag(.x,1), .names = "{.col}"))
# ----------------------------------- #

f   <- . ~ pop_log + vdem_index + gdp_pc_log + DALY_NCD_Percent + DALY_CD_Percent

pdat <- dat %>% select(cname, ccode, year, region, starts_with("health"),
                      pop_log, vdem_index, vdem_index2, gdp_pc_log,
                      DALY_NCD_Percent, DALY_CD_Percent) %>%
  drop_na()
pdat <- pdata.frame(pdat, index = c("ccode","year"))
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# Panel data models - first-differences with fixed effects                ----
#-----------------------------------------------------------------------------#

# pdat <- pdat[abs(residuals(m1)) < 2 * sd(resid(m1)),]
# pdat <- pdat[abs(residuals(m3))<0.17,]
# ----------------------------------- #
# Helpful panel data regression resources
# ----------------------------------- #
# https://www.econometrics-with-r.org/10-rwpd.html
# ----------------------------------- #

# ----------------------------------- #
# United States DV models
# ----------------------------------- #
m1 <- plm(health.ghdx_US_diff ~ health.ghdx_US_diff_lag + health.ghdx_China_diff,
          data   = pdat,
          model  = "within",
          effect = "twoways")
m1     <- within_intercept(m1, return.model = T)
m1_vcv <- vcovBK(m1, type = "HC1", cluster = "group")
m1_se  <- sqrt(diag(m1_vcv))
m1_pv  <- return_p(m1, m1_se)

m2 <- plm(update(f, health.ghdx_US_diff ~ . + health.ghdx_US_diff_lag + health.ghdx_China_diff),
          data   = pdat,
          model  = "within",
          effect = "twoways")
m2     <- within_intercept(m2, return.model = T)
m2_vcv <- vcovBK(m2, type = "HC1", cluster = "group")
m2_se  <- sqrt(diag(m2_vcv))
m2_pv  <- return_p(m2, m2_se)
# ----------------------------------- #


# ----------------------------------- #
# China DV models
# ----------------------------------- #
m3 <- plm(health.ghdx_China_diff ~ health.ghdx_China_diff_lag + health.ghdx_US_diff,
          data   = pdat,
          model  = "within",
          effect = "twoways")
m3    <- within_intercept(m3, return.model = T)
m3_vcv <- vcovBK(m3, type = "HC1", cluster = "group")
m3_se <- sqrt(diag(m3_vcv))
m3_pv <- return_p(m3, m3_se)

m4 <- plm(update(f, health.ghdx_China_diff ~ . + health.ghdx_China_diff_lag + health.ghdx_US_diff),
          data   = pdat,
          model  = "within",
          effect = "twoways")
m4     <- within_intercept(m4, return.model = T)
m4_vcv <- vcovBK(m4, type = "HC1", cluster = "group")
m4_se  <- sqrt(diag(m4_vcv))
m4_pv  <- return_p(m4, m4_se)
# ----------------------------------- #


# ----------------------------------- #
# By hand model...
# ----------------------------------- #
# now comparable "by hand" so I can verify I know what plm is doing...
  # m22 <- lm(update(f, health.ghdx_US_diff ~ . -1 + health.ghdx_US_diff_lag + health.ghdx_China_diff + ccode + year),
  #           data = dat)
  # m22_se <- sqrt(diag(vcovPC(m22, type = "HC1", cluster = ~ ccode)))
  # m22_pv <- return_p(m22, m22_se)
  #
  #
  # screenreg(list(m2,m22),
  #           omit.coef = "(ccode)|(year)",
  #           override.se      = list(m2_se,m22_se),
  #           override.pvalues = list(m2_pv,m22_pv))
  #
  # data.frame("plm" = coef(m2), "lm" = coef(m22)[1:8])
# ----------------------------------- #
#-----------------------------------------------------------------------------#


#-----------------------------------------------------------------------------#
# Model format                                                            ----
#-----------------------------------------------------------------------------#

# Model names for table output
var_names = list("health.ghdx_US_diff_lag"    = "First diff., lag",
                 "health.ghdx_China_diff_lag" = "First diff., lag",

                 "health.ghdx_US_diff"        = "US health aid diff.",
                 "health.ghdx_China_diff"     = "China health aid diff.",

                 "health.ghdx_US"             = "US health aid level",
                 "health.ghdx_China"          = "China health aid level",

                 "health.ghdx_US_lag"             = "US health aid level",
                 "health.ghdx_China_lag"          = "China health aid level",

                 "pop_log"                    = "Pop. (log)",
                 "vdem_index"                 = "Democracy",
                 "vdem_index2"                = "Democracy^2",
                 "gdp_log"                    = "GDP,  (log)",
                 "gdp_pc_log"                 = "GDP, per capita (log)",
                 "gdp_pc_log2"                = "GDP, per capita^2 (log)",
                 "DALY_NCD_Percent"           = "DALY - NCD (%)",
                 "DALY_CD_Percent"            = "DALY - CD (%)",
                 "Deaths_NCD_Percent"         = "Deaths - NCD (%)",
                 "Deaths_CD_Percent"          = "Deaths - CD (%)",
                 "(Intercept)"                = "Intercept")

screenreg(list(m1,m2, m3,m4),
          custom.header    = list("US" = 1:2, "China" = 3:4),
          override.se      = list(m1_se,m2_se,m3_se,m4_se),
          override.pvalues = list(m1_pv,m2_pv,m3_pv,m4_pv),
          custom.model.names = sprintf("(%s)", 1:4),
          custom.coef.map    = var_names,
          include.rsquared   = FALSE,
          stars              = c(0.001, 0.01, 0.05, 0.1),
          custom.gof.rows    = list("Fixed Effects - Country" = rep("Yes",4),
                                    "Fixed Effects - Year"    = rep("Yes",4)),
          custom.note        = "%stars.\nPanel corrected standard errors in parentheses.")

"
M2 : a 1 million dollar increase in Chinese aid corresponds to a 20.9 million dollar increase in US aid
M4 : a 100 million dollar increase in US aid corresponds to a 40000 dollar increase in Chinese aid.

These are vasdly different numbers, but they reflct the underlying priorities that each state has
separately placed on funding global health projects [note - our model does not seek to explain the
overall level of health aid each state provides, but rather, conditional on the allocated amount,
how the allocation decisions of the rival power impact their allocations to individual recipients.].

Each of these values are highly significant and substantive relative to the distribution of health
aid from each state [us mean funding = 21.58 million; china mean funding = 38187]
"

htmlreg(list(m1,m2, m3,m4),
        custom.header    = list("US" = 1:2, "China" = 3:4),
        override.se      = list(m1_se,m2_se,m3_se,m4_se),
        override.pvalues = list(m1_pv,m2_pv,m3_pv,m4_pv),
        custom.model.names = sprintf("(%s)", 1:4),
        custom.coef.map    = var_names,
        include.rsquared   = FALSE,
        stars              = c(0.001, 0.01, 0.05, 0.1),
        custom.gof.rows    = list("Fixed Effects - Country" = rep("Yes",4),
                                  "Fixed Effects - Year"    = rep("Yes",4)),
        custom.note        = "%stars.\nPanel corrected standard errors in parentheses.",
        file               = sprintf("Tables/Models_%s.html", format(Sys.Date(),"%Y%m%d")))


# Additional notes:
"
This model:
update(f, health.ghdx_China_diff ~ . + health.ghdx_China_diff_lag + health.ghdx_US_diff + health.ghdx_US_lag + health.ghdx_China_lag)

and its US equivalent seem to suggest ... that I'm too tired to keep going tonight.

Just going to stick with the one not including levels for now. I can discuss a health/wealth story that way.
"
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# PARAMETRIC SIMULATION                                                   ----
#-----------------------------------------------------------------------------#

# ----------------------------------- #
# Setup
# ----------------------------------- #
preds <- 100
sims  <- 1000
x     <- range(c(as.vector(pdat$health.ghdx_China_diff),
                 as.vector(pdat$health.ghdx_US_diff)))
# x    <- range(pdat$health.ghdx_China_diff)

res_m1 <- data.frame()
res_m2 <- data.frame()

format_sim <- function(sim_data,
                       x_vals = x,
                       model){
  tmp <- lapply(sim_data, function(x){

    m  <- median(x)
    ci <- quantile(x, probs = c(0.025, 0.975))

    d  <- data.frame(lci = ci[1], median = m, uci = ci[2])

  }) %>% bind_rows()

  tmp$x_vals <- seq(min(x_vals), max(x_vals), length.out = 100)
  tmp$mod    <- model

  rownames(tmp) <- 1:nrow(tmp)

  return(tmp)
}
# ----------------------------------- #


# ----------------------------------- #
# US Model - M2
# ----------------------------------- #
bs     <- coef(m2)
newdat <- lapply(pdat, prediction::median_or_mode) %>% as.data.frame() %>%
  dplyr::select(pop_log, vdem_index, gdp_pc_log, DALY_NCD_Percent,
                DALY_CD_Percent, health.ghdx_US_diff_lag, health.ghdx_China_diff)
newdat <- do.call("bind_rows", replicate(100, newdat, simplify = FALSE))

newdat$health.ghdx_China_diff <- seq(min(x), max(x), length.out = 100)
newdat <- as.matrix(cbind(1,newdat))

for(i in 1:sims){
  bsims <- MASS::mvrnorm(n  = 1,
                         mu = bs,
                         Sigma = m2_vcv)

  tmp_res <- data.frame(pred = t(newdat %*% bsims))

  res_m1 <- bind_rows(res_m1, tmp_res)
};rm(i, tmp_res, bsims)

pltdat1 <- format_sim(sim_data = res_m1, x_vals = x, model = "US Aid Response (10 millions)")

rm(bs, newdat)
# ----------------------------------- #


# ----------------------------------- #
# China Model - M4
# ----------------------------------- #
bs     <- coef(m4)
newdat <- lapply(pdat, prediction::median_or_mode) %>% as.data.frame() %>%
  dplyr::select(pop_log, vdem_index, gdp_pc_log, DALY_NCD_Percent,
                DALY_CD_Percent, health.ghdx_China_diff_lag, health.ghdx_US_diff)
newdat <- do.call("bind_rows", replicate(100, newdat, simplify = FALSE))

newdat$health.ghdx_US_diff <- seq(min(x), max(x), length.out = 100)
newdat <- as.matrix(cbind(1,newdat))

for(i in 1:sims){
  bsims <- MASS::mvrnorm(n  = 1,
                         mu = bs,
                         Sigma = m4_vcv)

  tmp_res <- data.frame(pred = t(newdat %*% bsims))

  res_m2 <- bind_rows(res_m2, tmp_res)
};rm(i, tmp_res, bsims)

pltdat2 <- format_sim(sim_data = res_m2, x_vals = x, model = "China Aid Response (millions)")

rm(bs, newdat)
# ----------------------------------- #


# ----------------------------------- #
# Plot
# ----------------------------------- #
pltdat <- bind_rows(pltdat1, pltdat2) %>%
  mutate(mod = as_factor(mod))

plt <- ggplot(data = pltdat, aes(x = x_vals, y = median)) +
  geom_ribbon(aes(ymin = lci, ymax = uci, fill = mod), alpha = 0.3) +
  geom_line(aes(color = mod)) +
  scale_y_continuous(name   = "Health aid funding response",
                     breaks = seq(-5,15, 2.5)/10,
                     labels = function(x){x * 10}) +
  scale_x_continuous(name   = "Health aid funding change (other power)",
                     breaks = c(seq(-2.5, 5.0, 1))) +
  # facet_wrap(~mod) +
  theme_bw() +
  theme(panel.grid         = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted", color = "gray80", size = 0.4),
        legend.position    = "bottom",
        legend.direction   = "horizontal",
        legend.title       = element_blank()) +
  labs(title = "US and Chinese Health Aid",
       subtitle = "Response to funding changes by the other",
       caption  = sprintf("%s",
                          "Note: Chinese aid measured in USD millions, US aid measured in USD 10 millions."))
# ----------------------------------- #


# ----------------------------------- #
# Save
# ----------------------------------- #
ggsave(plot     = plt,
       filename = sprintf("plots/model_responses_%s.png", format(Sys.Date(), "%Y%m%d")),
       width    = 6.5,
       height   = 6.5,
       units    = "in",
       dpi      = 350)
# ----------------------------------- #
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# TABLE - DESCRIPTIVE STATISTICS                                          ----
#-----------------------------------------------------------------------------#
dvs <- pdat %>%
  select(health.ghdx_US_diff, health.ghdx_China_diff,
         health.ghdx_US,      health.ghdx_China) %>%
  summarize(across(
    c(health.ghdx_US_diff, health.ghdx_China_diff,
      health.ghdx_US,      health.ghdx_China),
    list("Min."     = ~min(.x),
         "Mean"     = ~mean(.x),
         "Max."     = ~max(.x),
         "St. Dev." = ~sd(.x)),
    .names = "{.col}-{.fn}")) %>%
  t %>%
  as.data.frame() %>%
  rownames_to_column(var = "var") %>%
  mutate(source = case_when(str_detect(var, "_US") ~ "US", TRUE ~ "China"),
         type   = case_when(str_detect(var, "_diff") ~"Diff", TRUE ~ "Level"),
         var = gsub("^.*\\-", "", var)) %>%
  pivot_wider(.,
              id_cols     = c(source, var, type),
              names_from  = var,
              values_from = V1) %>%
  mutate(source = case_when(source == "US" ~ "US - Health Aid (10 millions)",
                            TRUE ~ "China - Health Aid (millions)"))

dvs %>%
  kbl(digits = 3) %>%
  kable_classic_2(full_width = T) %>%
  save_kable(file = sprintf("Tables/DVs_%s.html", format(Sys.Date(),"%Y%m%d")))

rm(dvs)
#-----------------------------------------------------------------------------#






















































# this is all so fucked. for whatever reason predict() and prediction::predict()
# do NOT work on plm models or simply refuse to return standard errors and
# give no warning message or indication as to why.

# Will resolve by brute forcing results via parametric simulation.

# ----------------------------------- #
# Setup
# ----------------------------------- #
bs     <- coef(m2)
newdat <- lapply(pdat, mean_or_mode) %>% as.data.frame() %>%
  dplyr::select(pop_log, vdem_index, gdp_pc_log, DALY_NCD_Percent,
         DALY_CD_Percent, health.ghdx_US_diff_lag, health.ghdx_China_diff)
newdat <- bind_rows(newdat, newdat)

# newdat$health.ghdx_China_diff <- quantile(pdat$health.ghdx_China_diff, probs = c(0.25, 0.75))
newdat$health.ghdx_China_diff <- range(as.vector(pdat$health.ghdx_China_diff))
newdat <- as.matrix(cbind(1,newdat))

# Single draw...
# newdat %*% bs

sims <- 2e3
vcv_m2 <- vcovBK(m2, cluster = "group", type = "HC1")

res <- data.frame()

library(MASS)
for(i in 1:sims){
  bsims <- mvrnorm(n  = 1,
                   mu = bs,
                   Sigma = vcv_m2)

  tmp_res <- as.data.frame(t(newdat %*% bsims)) %>%
    rename("china_low" = 1, "china_high" = 2)

  res <- bind_rows(res, tmp_res)

}


pltdat <- lapply(res, function(x){

  ci <- quantile(x, probs = c(0.025, 0.975))
  m  <- median(x)

  d  <- data.frame('lci' = ci[1], "median" = m, "uci" = ci[2])

  }) %>%
  bind_rows() %>%
  mutate(eff = factor(c("china_low","china_high"), levels = c("china_low","china_high")),
         x   = c(1,2))

ggplot(data = pltdat, aes(x = x, y = median, color = eff)) +
  geom_point() +
  geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.1) +
  scale_x_continuous(breaks = c(1,2),
                     labels = pltdat$eff) +
  scale_y_continuous(breaks = seq(-0.3, 0.7, by = 0.2),
                     limits = c(-0.3, 0.7),
                     labels = function(x){x * 10},
                     name   = "USD (10 millions)") +
  geom_hline(aes(yintercept = 0), color = "black", size = 0.4) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted", color = "gray80", size = 0.5),
        legend.position = "bottom",
        legend.direction = "horizontal")

# ----------------------------------- #
# Biggest change in us health aid - Iraq bt 2004 to 2005 (war "ended", reconstruction began)
summary(as.vector(pdat$health.ghdx_US_diff))
View(dat %>% filter(cname == "Iraq", year %in% 2004:2006) %>% dplyr::select(cname, year, starts_with("health")))
# ----------------------------------- #


bs     <- coef(m4)
newdat <- lapply(pdat, mean_or_mode) %>% as.data.frame() %>%
  dplyr::select(pop_log, vdem_index, gdp_pc_log, DALY_NCD_Percent,
         DALY_CD_Percent, health.ghdx_China_diff_lag, health.ghdx_US_diff)
newdat <- bind_rows(newdat, newdat)

newdat$health.ghdx_US_diff <- range(as.vector(pdat$health.ghdx_US_diff))
newdat <- as.matrix(cbind(1,newdat))

# Single draw...
# newdat %*% bs

sims <- 2e3
vcv_m4 <- vcovBK(m4, cluster = "group", type = "HC1")

res <- data.frame()

library(MASS)
for(i in 1:sims){
  bsims <- mvrnorm(n  = 1,
                   mu = bs,
                   Sigma = vcv_m4)

  tmp_res <- as.data.frame(t(newdat %*% bsims)) %>%
    rename("us_low" = 1, "us_high" = 2)

  res <- bind_rows(res, tmp_res)

}


pltdat <- lapply(res, function(x){

  ci <- quantile(x, probs = c(0.025, 0.975))
  m  <- median(x)

  d  <- data.frame('lci' = ci[1], "median" = m, "uci" = ci[2])

}) %>%
  bind_rows() %>%
  mutate(eff = factor(c("us_low","us_high"), levels = c("us_low","us_high")),
         x   = c(1,2))

ggplot(data = pltdat, aes(x = x, y = median, color = eff)) +
  geom_point() +
  geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.1) +
  scale_x_continuous(breaks = c(1,2),
                     labels = pltdat$eff) +
  scale_y_continuous(breaks = seq(-0.3, 0.7, by = 0.2),
                     limits = c(-0.3, 0.7),
                     labels = function(x){x * 10},
                     name   = "USD (millions)") +
  geom_hline(aes(yintercept = 0), color = "black", size = 0.4) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted", color = "gray80", size = 0.5),
        legend.position = "bottom",
        legend.direction = "horizontal")
#-----------------------------------------------------------------------------#









# agh! For some reason sudan is duplicated. likely due to handling s. sudan in
# tidy script. NEed to fix!!!                           --------------------------------------
# Secondary df with missing values replaced as 0
d <- hd %>%
  mutate(across(starts_with("health"), ~replace_na(.x, 0))) %>%
  filter(ccode != 625) %>%
  mutate(across(starts_with("health"), ~.x/1e6))
#   mutate(across(starts_with("health"), ~log(.x + 1)))

# d2 <- hd %>%
#   mutate(across(starts_with("health"), ~replace_na(.x, 0))) %>%
#   group_by(cname) %>%
#   mutate(never_aid_China = case_when(max(health.ghdx_China) == 0 ~ 1, TRUE ~ 0),
#          never_aid_US    = case_when(max(health.ghdx_US) == 0 ~ 1, TRUE ~ 0)) %>%
#   ungroup()


# Model formula [population, democracy, wealth, health]
f <- . ~
  pop_log +
  vdem_index +
  vdem_index2 +
  gdp_pc_log +
  # gdp_pc2_log +
  DALY_NCD_Percent + DALY_CD_Percent

# Model names for table output
var_names = list("log(health.ghdx_China + 1)" = "Health Aid - China",
                 "log(health.ghdx_US + 1)"    = "Health Aid - US",
                 "health_pht.ghdx_China"      = "Health Aid - China",
                 "health_pht.ghdx_US"         = "Health Aid - US",
                 "pop_log"                    = "Pop. (log)",
                 "vdem_index"                 = "Democracy",
                 "vdem_index2"                = "Democracy^2",
                 "gdp_log"                    = "GDP,  (log)",
                 "gdp_pc_log"                 = "GDP, per capita (log)",
                 "gdp_pc_log2"                = "GDP, per capita^2 (log)",
                 "DALY_NCD_Percent"           = "DALY - NCD (%)",
                 "DALY_CD_Percent"            = "DALY - CD (%)",
                 "Deaths_NCD_Percent"         = "Deaths - NCD (%)",
                 "Deaths_CD_Percent"          = "Deaths - CD (%)",
                 "(Intercept)"                = "Intercept")

# Data specification (so I only need to chang here),
# choice of data as-is or with NAs on DV recoded to 0 (i.e., no reported
# health aid indicates no health aid in those models)
# dat <- hd
dat <- model.frame(update(f, health.ghdx_US ~ . + health.ghdx_China + health_pht.ghdx_China + health_pht.ghdx_US +
                            health.usaid_US + health.aiddata_China + ccode + year), d)
# dat <- d

#-----------------------------------------------------------------------------#

# Drop Afghansitan and Iraq as outliers
# dat <- dat %>% filter(!ccode %in% c(700, 645))

#-----------------------------------------------------------------------------#
# TABLE - DESCRIPTIVE STATISTICS                                          ----
#-----------------------------------------------------------------------------#
dollars <- 1e3
dvs <- dat %>%
  summarize(across(
    # c("health.usaid_US", "health.aiddata_China"),
    c("health.ghdx_US", "health.ghdx_China"),
                   list("Min."     = ~min(.x)/dollars,
                        "Mean"     = ~mean(.x)/dollars,
                        "Max."     = ~max(.x)/dollars,
                        "St. Dev." = ~sd(.x)/dollars),
                   .names = "{.col}-{.fn}")) %>%
  t %>%
  as.data.frame() %>%
  rownames_to_column(var = "var") %>%
  mutate(var = gsub("^.*\\_", "", var)) %>%
  mutate(variable = str_split(var, pattern = "-") %>% map(., 1) %>% unlist,
         stat     = str_split(var, pattern = "-") %>% map(., 2) %>% unlist) %>%
  pivot_wider(.,
              id_cols     = variable,
              names_from  = stat,
              values_from = V1) %>%
  mutate(variable = paste("Health Aid (1000s USD) - ", variable))

controls <- dat %>%
  summarize(across(c("pop", "vdem_index", "gdp_pc", "DALY_NCD_Percent", "DALY_CD_Percent"),
                   list("Min."     = ~min(.x, na.rm = T),
                        "Mean"     = ~mean(.x, na.rm = T),
                        "Max."     = ~max(.x, na.rm = T),
                        "St. Dev." = ~sd(.x, na.rm = T)),
                   .names = "{.col}-{.fn}")) %>%
  t %>%
  as.data.frame() %>%
  rownames_to_column(var = "var") %>%
  mutate(variable  = str_split(var, pattern = "-") %>% map(., 1) %>% unlist,
         stat      = str_split(var, pattern = "-") %>% map(., 2) %>% unlist) %>%
  pivot_wider(.,
              id_cols     = variable,
              names_from  = stat,
              values_from = V1) %>%
  mutate(variable = case_when(variable == "pop"              ~ "Pop. (1000s)",
                              variable == "vdem_index"       ~ "Democracy",
                              variable == "gdp_pc"           ~ "GDP, per capita",
                              variable == "DALY_NCD_Percent" ~ "DALY - NCD (%)",
                              variable == "DALY_CD_Percent"  ~ "DALY - CD (%)"))


descriptives <- bind_rows(dvs, controls) %>%
  mutate(across(c(`Min.`:`St. Dev.`), ~format(.x, digits = 2, scientific = F)))

descriptives %>%
  kbl(digis = 2) %>%
  kable_classic_2(full_width = T) %>%
  save_kable(file = sprintf("Draft/Results/Descriptives_%s.html", format(Sys.Date(),"%Y%m%d")))


rm(dollars, dvs, controls, descriptives)
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# ANALYSIS                                                                ----
#-----------------------------------------------------------------------------#

# ----------------------------------- #
# plm
# ----------------------------------- #
dat2 <- pdata.frame(dat, index = c("ccode", "year"))

m1 <- plm(formula = update(f, diff(health.ghdx_US) ~ . + lag(diff(health.ghdx_US)) + diff(health.ghdx_China)),
          data = dat2)
summary(m1)

m2 <- plm(formula = update(f, diff(health.ghdx_China) ~ . + lag(diff(health.ghdx_China)) + diff(health.ghdx_US), model = "fd", effect = "individual"),
          data = dat)
summary(m2)

coef(m1) / sqrt(diag(vcovBK(m1, cluster = "group")))
coef(m1) / sqrt(diag(vcov(m1)))

dat1 <- dat %>%
  group_by(ccode) %>%
  mutate(
    health.ghdx_US_diff = health.ghdx_US - dplyr::lag(health.ghdx_US, 1),
    health.ghdx_China_diff = health.ghdx_China - dplyr::lag(health.ghdx_China, 1)
) %>%
  mutate(
    health.ghdx_US_diff_lag = dplyr::lag(health.ghdx_US_diff, 1),
    health.ghdx_China_diff_lag = dplyr::lag(health.ghdx_China_diff, 1)
  ) %>%
  ungroup()

m0 <- lm(formula = update(f, health.ghdx_US_diff ~ . + health.ghdx_US_diff_lag + health.ghdx_China_diff + ccode + year),
         data = dat1)
# summary(m0)
screenreg(m1)
screenreg(m0, omit.coef = "(ccode)|(year)")


m3 <- lm(formula = update(f, health.ghdx_China_diff ~ . + health.ghdx_China_diff_lag + health.ghdx_US_diff + ccode + year),
         data = dat1)
screenreg(m3, omit.coef = "(ccode)|(year)")



dat1 <- dat %>%
  group_by(ccode) %>%
  mutate(
    health_pht.ghdx_US_diff = health_pht.ghdx_US - dplyr::lag(health_pht.ghdx_US, 1),
    health_pht.ghdx_China_diff = health_pht.ghdx_China - dplyr::lag(health_pht.ghdx_China, 1)
  ) %>%
  mutate(
    health_pht.ghdx_US_diff_lag = dplyr::lag(health_pht.ghdx_US_diff, 1),
    health_pht.ghdx_China_diff_lag = dplyr::lag(health_pht.ghdx_China_diff, 1)
  ) %>%
  ungroup()


m0 <- lm(formula = update(f, health_pht.ghdx_US_diff ~ . + health_pht.ghdx_US_diff_lag + health_pht.ghdx_China_diff + ccode + year),
         data = dat1)
screenreg(m0, omit.coef = "(ccode)|(year)", digits = 5)

m3 <- lm(formula = update(f, health_pht.ghdx_China_diff ~ . + health_pht.ghdx_China_diff_lag + health_pht.ghdx_US_diff + ccode + year),
         data = dat1)
screenreg(m3, omit.coef = "(ccode)|(year)")


dat1 <- dat %>%
  group_by(ccode) %>%
  mutate(across(!contains(c("ccode","year")), ~.x - dplyr::lag(.x, 1), .names = "{.col}_diff")) %>%
  mutate(across(!contains(c("ccode","year")), ~dplyr::lag(.x, 1), .names = "{.col}_lag"))


m0 <- lm(formula = health_pht.ghdx_US_diff ~ health_pht.ghdx_US_diff_lag + health_pht.ghdx_China_diff_lag +
           vdem_index_lag + vdem_index2_lag +
           gdp_pc_log_lag +
           DALY_CD_Percent_lag + DALY_NCD_Percent_lag +
           ccode + year,
           # vdem_index_diff + vdem_index2_diff +
           # gdp_pc_log_diff +
           # DALY_CD_Percent_diff + DALY_NCD_Percent_diff +
           # ccode + year,
         data = dat1)
screenreg(m0, omit.coef = "(ccode)|(year)")

m0 <- lm(formula = health_pht.ghdx_China_diff ~ health_pht.ghdx_China_diff_lag + health_pht.ghdx_US_diff_lag +
           # vdem_index_lag + vdem_index2_lag +
           # gdp_pc_log_lag +
           # DALY_CD_Percent_lag + DALY_NCD_Percent_lag +
           # ccode + year,
           vdem_index_diff + vdem_index2_diff +
           gdp_pc_log_diff +
           DALY_CD_Percent_diff + DALY_NCD_Percent_diff +
           ccode + year,
         data = dat1)
screenreg(m0, omit.coef = "(ccode)|(year)")


# ----------------------------------- #


# ----------------------------------- #
# Levels - logged
# ----------------------------------- #

# NB - ADDED "never_aid_X" to models below - this var is NOT in the table in the paper.
# There is some fuckery in pred val... billions in aid. something in the model is off.

m1 <- lm(formula = health.ghdx_US ~ health.ghdx_China + ccode + year,
         data    = dat)

m2 <- lm(formula = update(f, health.ghdx_US ~ . + health.ghdx_China),
         data    = dat)

m3 <- lm(formula = health.ghdx_China ~ health.ghdx_US + ccode + year,
         data    = dat)

m4 <- lm(formula = update(f, health.ghdx_China ~ . + health.ghdx_US),
         data    = dat)

screenreg(l                  = list(m1,m2,m3,m4),
          omit.coef          = "(ccode)|(year)",
          digits             = 3)

mods <- mod_format(list(m1,m2,m3,m4))
screenreg(l                  = mods$mods,
          omit.coef          = "(ccode)|(year)",
          override.se        = mods$ses,
          override.pvalues   = mods$pvals,
          custom.coef.map    = var_names,
          custom.header      = list("US" = 1:2, "China" = 3:4),
          # custom.model.names = c("US","China"),
          include.rsquared   = FALSE,
          custom.gof.rows    = list("Fixed Effects - Country" = rep("Yes",length(mods$mods)),
                                    "Fixed Effects - Year"    = rep("Yes",length(mods$mods))),
          custom.note        = "%stars.\nPanel corrected standard errors in parentheses.")

htmlreg(l                  = mods$mods,
        omit.coef          = "(ccode)|(year)",
        override.se        = mods$ses,
        override.pvalues   = mods$pvals,
        custom.coef.map    = var_names,
        custom.header      = list("US" = 1:2, "China" = 3:4),
        # custom.model.names = c("US","China"),
        include.rsquared   = FALSE,
        custom.gof.rows    = list("Fixed Effects - Country" = rep("Yes",length(mods$mods)),
                                  "Fixed Effects - Year"    = rep("Yes",length(mods$mods))),
        custom.note        = "%stars.\nPanel corrected standard errors in parentheses.",
        file               = sprintf("Draft/Results/Models_%s.html", format(Sys.Date(),"%Y%m%d")))


dat2 <- dat %>%
  mutate(res = residuals(m2)) %>%
  filter(abs(res) < 2)

m22 <- lm(formula = update(f, log(health.ghdx_US + 1) ~ . + log(health.ghdx_China + 1)),
         data    = dat2)
m42 <- lm(formula = update(f, log(health.ghdx_China + 1) ~ . + log(health.ghdx_US + 1)),
         data    = dat2)
screenreg(l                  = list(m22,m42),
          omit.coef          = "(ccode)|(year)")
# ----------------------------------- #


# ----------------------------------- #
# Percentages
# ----------------------------------- #
m5 <- lm(formula = health_pht.ghdx_US ~ health_pht.ghdx_China + ccode + year,
         data    = dat)

m6 <- lm(formula = update(f, health_pht.ghdx_US ~ . + health_pht.ghdx_China),
         data    = dat)

m7 <- lm(formula = health_pht.ghdx_China ~ health_pht.ghdx_US + ccode + year,
         data    = dat)

m8 <- lm(formula = update(f, health_pht.ghdx_China ~ . + health_pht.ghdx_US),
         data    = dat)

mods <- mod_format(list(m5,m6,m7,m8))

screenreg(l                  = mods$mods,
          omit.coef          = "(ccode)|(year)",
          override.se        = mods$ses,
          override.pvalues   = mods$pvals,
          custom.coef.map    = var_names,
          custom.header      = list("US" = 1:2, "China" = 3:4),
          # custom.model.names = c("US","China"),
          include.rsquared   = FALSE,
          custom.gof.rows    = list("Fixed Effects - Country" = rep("Yes",length(mods$mods)),
                                    "Fixed Effects - Year"    = rep("Yes",length(mods$mods))),
          custom.note        = "%stars.\nPanel corrected standard errors in parentheses.")
# ----------------------------------- #
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# MARGINAL EFFECT PLOTS [LEVEL MODELS]
#-----------------------------------------------------------------------------#

# first difference would be much better than these bullshit values predicted here...
# predicting billions of dollars in aid for china due to extreme values...
# use 1 sd shift in us / chinese aid instead

dat %>% select(starts_with("health.ghdx_")) %>% summary
dat %>% select(starts_with("health.ghdx_")) %>% lapply(., sd)


sort(coef(m4)[str_detect(names(coef(m4)), "ccode")])
sort(coef(m4)[str_detect(names(coef(m4)), "year")])
# ----------------------------------- #
# Setup
# ----------------------------------- #
res       <- data.frame()
preds     <- 100

max(dat$health.ghdx_China)
pred_vals <- seq(0, 14.7, length.out = preds)

# Since observations are identical across models, only need one "newdat" obj.
newdat <- model.frame(formula(m2), dat)
newdat <- newdat %>%
  select(-1, -ncol(.)) %>%
  mutate(across(pop_log:DALY_CD_Percent, mean),
         ccode = factor(660),
         year  = factor(2002)) %>%
  slice(1:preds)

newdat$health.ghdx_China <- pred_vals
newdat$health.ghdx_US    <- pred_vals
# ----------------------------------- #


# ----------------------------------- #
# M2 - United States aid model
# ----------------------------------- #
pred <- prediction::prediction(m2, data = newdat,
                               vcov = mods$vcvs[[2]], type = "response")
pred <- as.data.frame(pred) %>%
  mutate(lci = fitted - (qnorm(0.975) * se.fitted),
         uci = fitted + (qnorm(0.975) * se.fitted),
         mod = "US") %>%
  rename(aid = health.ghdx_China) %>%
  select(aid, lci, fitted, uci, se.fitted, mod)

res <- bind_rows(res, pred)
# ----------------------------------- #


# ----------------------------------- #
# M4 - China aid model
# ----------------------------------- #
pred <- prediction::prediction(m4, data = newdat,
                               vcov = mods$vcvs[[4]], type = "response")
pred <- as.data.frame(pred) %>%
  mutate(lci = fitted - (qnorm(0.975) * se.fitted),
         uci = fitted + (qnorm(0.975) * se.fitted),
         mod = "China") %>%
  rename(aid = health.ghdx_US) %>%
  select(aid, lci, fitted, uci, se.fitted, mod)

res <- bind_rows(res, pred)

rm(pred, newdat, preds)
# ----------------------------------- #

res %>% select(mod,lci, fitted, uci) %>% group_by(mod) %>%
  summarize(across(c(lci, fitted, uci), ~c(min  = min(.x),
                                           mean = mean(.x),
                                           max  = max(.x),
                                           sd   = sd(.x))))

res %>% select(mod,lci, fitted, uci) %>% group_by(mod) %>%
  summarize(across(c(lci, fitted, uci), ~c(min  = exp(min(.x)),
                                           mean = exp(mean(.x)),
                                           max  = exp(max(.x)),
                                           sd   = exp(sd(.x)))))


# ----------------------------------- #
# Plot
# ----------------------------------- #
ggplot(data = res, aes(x = aid, y = fitted)) +
  geom_ribbon(aes(ymin = lci,
                  ymax = uci,
                  fill = mod),
              alpha = 0.3) +
  geom_line(aes(color = mod))
  # scale_x_continuous(breaks = c(0,log(1e2), log(1e3), log(1e4), log(1e5), log(1e6)),
  #                    labels = c(0, "100", "10000","10k","100k", "1mil")) +
  # scale_y_continuous(breaks = c(0,log(1e2), log(1e3), log(1e4), log(1e5), log(1e6)),
  #                    labels = c(0, "100", "10000","10k","100k", "1mil"))

# ----------------------------------- #

                            # tst <- predict(m2,
                            #                newdata = newdat,
                            #                interval="confidence",
                            #                level = 0.95) %>%
                            #   as.data.frame() %>%
                            #   mutate(x = 1:n())
                            #
                            # ggplot(data = tst, aes(x = x, y = fit)) +
                            #   geom_ribbon(aes(ymin = lwr,
                            #                   ymax = upr)) +
                            #   geom_line()
#-----------------------------------------------------------------------------#

newdat <- model.frame(formula(m2), dat)
newdat <- newdat %>%
  select(-1, -ncol(.)) %>%
  mutate(across(pop_log:DALY_CD_Percent, ~quantile(.x, probs = c(0.5))),
         ccode = factor(450),
         year  = factor(2008)) %>%
  slice(1:2)

# newdat$health.ghdx_China <- quantile(dat$health.ghdx_China, probs = c(0.25, 0.75))
# newdat$health.ghdx_US    <- quantile(dat$health.ghdx_US,    probs = c(0.25, 0.75))

newdat$health.ghdx_China <- c(mean(dat$health.ghdx_China), mean(dat$health.ghdx_China) + sd(dat$health.ghdx_China))
newdat$health.ghdx_US    <- c(mean(dat$health.ghdx_US), mean(dat$health.ghdx_US) + sd(dat$health.ghdx_US))


tst <- prediction::prediction(m4, data = newdat, vcov = mods$vcvs[[4]], type = "response") %>%
  mutate(lci = fitted - (qnorm(0.975) * se.fitted),
         uci = fitted + (qnorm(0.975) * se.fitted)) %>%
  as.data.frame()

ggplot(data = tst, aes(x = c(1,2), y = fitted)) +
  geom_point() +
  geom_errorbar(aes(ymin = lci, ymax = uci))
#-----------------------------------------------------------------------------#


#-----------------------------------------------------------------------------#

newdat <- model.frame(formula(m2), dat)
newdat <- newdat %>%
  select(-1, -ncol(.)) %>%
  mutate(across(pop_log:DALY_CD_Percent, ~quantile(.x, probs = c(0.5))),
         ccode = factor(450),
         year  = factor(2008)) %>%
  slice(1:2)
newdat$health.ghdx_China <- c(mean(dat$health.ghdx_China), mean(dat$health.ghdx_China) + sd(dat$health.ghdx_China))
newdat$health.ghdx_US    <- c(mean(dat$health.ghdx_US), mean(dat$health.ghdx_US) + sd(dat$health.ghdx_US))

bs <- c(coef(m4)[c(1:7)],
        mean(as.numeric(coef(m4)[str_detect(names(coef(m4)), "ccode")])),
        mean(as.numeric(coef(m4)[str_detect(names(coef(m4)), "year")])),
        coef(m4)[189])

X <- as.matrix(cbind(1, newdat[1,c(1:6)], 1,1, newdat[1,10]))
as.numeric(X %*% matrix(bs, nrow = length(bs))) / 1e6

X <- as.matrix(cbind(1, newdat[2,c(1:6)], 1,1, newdat[2,10]))
as.numeric(X %*% matrix(bs, nrow = length(bs))) / 1e6

X <- as.matrix(cbind(1, newdat[2,c(1:6,10)]))
as.numeric(X %*% matrix(bs, nrow = 8)) / 1e6


#-----------------------------------------------------------------------------#

# what-fucking-ever https://stats.stackexchange.com/questions/363066/predicting-dependent-variable-with-fixed-effects-model
newdat <- model.frame(m2, dat)
# x1 <- mean(newdat$health.ghdx_US)
# x2 <- mean(newdat$health.ghdx_US) + sd(newdat$health.ghdx_US)
# x1 <- quantile(newdat$health.ghdx_US[newdat$health.ghdx_US>0], probs = c(0.25))
# x2 <- quantile(newdat$health.ghdx_US[newdat$health.ghdx_US>0], probs = c(0.75))
x1 <- min(newdat$health.ghdx_US)
x2 <- max(newdat$health.ghdx_US)


newdat$health.ghdx_US <- x1

pred <- predict(m4, newdata = newdat)
p1   <- mean(pred)

newdat$health.ghdx_US <- x2
pred <- predict(m4, newdata = newdat)
p2   <- mean(pred)

exp(p2) - exp(p1)

#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# SAVE                                                                    ----
#-----------------------------------------------------------------------------#
#save.image()
#rm(list = ls())
