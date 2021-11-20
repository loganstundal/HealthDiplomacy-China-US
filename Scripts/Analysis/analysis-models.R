#-----------------------------------------------------------------------------#
#
# Author:        Logan Stundal
# Date:          November 18, 2021
# Purpose:       Models for Chinese-US health diplomacy
#
#
# Copyright (c): Logan Stundal, 2021
# Email:         stund005@umn.edu
#
#-----------------------------------------------------------------------------#
#
# Notes:
#  - because I always forget...
#  https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faqhow-do-i-interpret-a-regression-model-when-some-variables-are-log-transformed/
#
#
# the code below is a real mess...
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
library(sf)
library(sandwich)
library(texreg)
#---------------------------#

#---------------------------#
# Load data
#---------------------------#
load("Data/health-diplomacy_20211118.Rdata")
#---------------------------#
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# SUBSET DATA                                                             ----
#-----------------------------------------------------------------------------#
d <- d %>%
  st_drop_geometry %>%
  rename(usa     = health_usa_dah,
         usa_lag = health_usa_dah_lag,
         chn     = health_chn_dah,
         chn_lag = health_chn_dah_lag,
         usa_mil = health_usa_usaidm_bin,
         chn_mil = health_chn_aidatm_binp,
         chn_med = health_chn_aidatmt_bin) %>%
  select(ccode, year, region, cname,
         usa, usa_lag, chn, chn_lag, usa_mil, chn_mil,
         wealth_gdp_pc_ln, pop_ln, dem_life_expec_ln, dem_inf_death_rate_ln,

         wealth_gdp_ln, ally_usa_nonagg, ally_chn_nonagg, dis_deaths_cd_percent,
         chn_med) %>%
  drop_na %>%
  mutate( across(.cols = usa:chn_lag,
                 .fns  = ~log(.x + 1)),
          across(.cols = contains("_mil"),
                 .fns  = ~forcats::fct_relevel(.x, "No military", "Military"))) %>%
  mutate(usa_mil = ifelse(usa_mil == "Military",1, 0),
         chn_mil = ifelse(chn_mil == "Military",1, 0)) %>%

  filter(chn >0) %>%
  # filter(year %in% as.character(2004:2019)) %>% # Makes no real diff...
  mutate(year = as_factor(year)) %>%
  filter(!ccode %in% c(2, 710))

  # mutate(usa = usa - usa_lag,
  #        chn = chn - chn_lag) %>%


  # filter(year %in% as.character(2004:2019))
  # mutate(year = forcats::fct_drop(year))
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# MODEL                                                                   ----
#-----------------------------------------------------------------------------#
qm <- function(mods, vcv = NULL){
  res <- lapply(mods, function(mod){
    cfs <- coefficients(mod)
    vcv <- vcovPL(mod, cluster = ~cname + year)
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

  screenreg(mods,
            omit.coef        = c("region|cname|year"),
            override.se      = ses,
            override.pvalues = pvl,
            # ci.force         = TRUE,
            custom.header    = list("United States" = 1:2, "China" = 3:4),
            custom.gof.rows  = list("Region FE" = rep("Yes", 4),
                                    "Year FE"   = rep("Yes", 4)),
            custom.note      = paste0("%stars\n",
                                      "Panel corrected standard errors clustered on country and year in parentheses."))
}


m1 <- lm(usa ~ usa_lag + chn + usa_mil + chn_mil + cname + year,
          data  = d)
m2 <- lm(chn ~ chn_lag + usa + usa_mil + chn_mil + cname + year,
          data   = d)
m3 <- lm(usa ~ usa_lag + chn + usa_mil + chn_mil +
           wealth_gdp_ln + pop_ln + dem_inf_death_rate_ln +
           # dis_deaths_cd_percent +
           ally_usa_nonagg + ally_chn_nonagg +
           cname + year,
          data  = d)
m4 <- lm(chn ~ chn_lag + usa + usa_mil + chn_mil +
           wealth_gdp_ln + pop_ln + dem_inf_death_rate_ln +
           # dis_deaths_cd_percent +
           ally_usa_nonagg + ally_chn_nonagg +
           cname + year,
          data   = d)
# screenreg(list(m1,m3,m2,m4),
#           custom.header = list("USA" = 1:2, "CHN" = 3:4),
#           omit.coef = "region|cname|year")
qm(list(m1,m3,m2,m4))

mods <- list(m1,m3,m2,m4)
res <- lapply(mods, function(mod){
  cfs <- coefficients(mod)
  vcv <- vcovPL(mod, cluster = ~cname + year)
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

htmlreg(mods,
         file = "Results/res-20211119.doc",
          omit.coef        = c("region|cname|year"),
          override.se      = ses,
          override.pvalues = pvl,
          # ci.force         = TRUE,
          custom.header    = list("United States" = 1:2, "China" = 3:4),
          custom.gof.rows  = list("Region FE" = rep("Yes", 4),
                                  "Year FE"   = rep("Yes", 4)),
          custom.note      = paste0("%stars\n",
                                    "Panel corrected standard errors clustered on country and year in parentheses."))








# interpret pop coef:
#  (1.20^0.198 – 1) * 100 # for a 20% increase in pop
(1.01^1.77 - 1) * 100

# interpret mil indicator:
# (exp(coef) - 1 ) * 100
(exp(0.2) - 1) * 100   # US Model 2
(exp(-0.24) - 1) * 100 # China Model 4



# https://www.statalist.org/forums/forum/general-stata-discussion/general/1590559-non-negative-continuous-right-skewed-zero-inflated-panel-data-analysis
# Recommend poisson for overdispersed non-negative continuous.
# Could use ZIP to deal with zero-inflation...

m1 <- glm(usa ~ usa_lag + chn + usa_mil + chn_mil + region + year,
          data  = d,
          family = poisson(link = "log"))
m2 <- glm(chn ~ chn_lag + usa + usa_mil + chn_mil + region + year,
          data   = d,
          family = poisson(link = "log"))

screenreg(list(m1,m2), omit.coef = "region|cname|year")


library(pscl)

d <- d %>%
  mutate(across(.cols = c(usa, usa_lag, chn, chn_lag),
                .fns  = ~as.integer(.x)))

m1 <- zeroinfl(chn ~ chn_lag + usa + usa_mil + chn_mil + region + year |
               wealth_gdp_ln + ally_usa_nonagg + ally_chn_nonagg,
              data = d)

m2 <- zeroinfl(usa ~ usa_lag + chn + usa_mil + chn_mil + region + year |
                wealth_gdp_ln + ally_usa_nonagg + ally_chn_nonagg,
              data = d)
screenreg(list(m1,m2))



# or hurdle models...
# https://stats.stackexchange.com/questions/187824/how-to-model-non-negative-zero-inflated-continuous-data
# which may be modeled with a tobit - going to use Rob's lab code (lab 2b)

library(AER)     # Applied econometrics package. Includes tobit.
m1 <- tobit(chn ~ chn_lag + usa + usa_mil + chn_mil +
             wealth_gdp_pc_ln + pop_ln + dem_inf_death_rate_ln ,
             # region + year,
           left = 0,
           right = Inf,
           dist = "gaussian",
           data = d)

m2 <- tobit(usa ~ usa_lag + chn + usa_mil + chn_mil +
             wealth_gdp_pc_ln + pop_ln + dem_inf_death_rate_ln,
             # region + year,
           left = 0,
           right = Inf,
           dist = "gaussian",
           data = d)

screenreg(list(m1,m2))

vcv <- vcovCL(m, cluster = ~cname + year,type = "HC1")
ses <- sqrt(diag(vcv))
screenreg(m, omit.coef = "region|cname|year",
          override.se = ses, ci.force = T)
# You can interpret *these* Tobit regression coefficients similar to OLS
# regression coefficients; just note that the linear effect here refers
# to the "unobserved latent variable" and not the observed outcome.

# HURDLE MODEL:
# Note:
# https://stats.stackexchange.com/questions/320924/is-a-hurdle-model-really-one-model-or-just-two-separate-sequential-models

m0 <- glm(I(chn == 0) ~ -1 + wealth_gdp_ln + ally_usa_nonagg + ally_chn_nonagg +
            dis_deaths_cd_percent + cname + year,
          data = d,
          family = binomial(link = "probit"))
screenreg(m0, omit.coef = "cname|region|year")

m1 <- lm(chn ~ chn_lag + usa + usa_mil + chn_mil +
           wealth_gdp_pc_ln + pop_ln + dem_inf_death_rate_ln +
           region + year,
         subset = chn > 0,
         data = d)
screenreg(m1, omit.coef = "cname|region|year")


m0 <- glm(I(usa == 0) ~ -1 + wealth_gdp_ln + ally_usa_nonagg + ally_chn_nonagg +
            dis_deaths_cd_percent + cname + year,
          data = d,
          family = binomial(link = "probit"))
screenreg(m0, omit.coef = "cname|region|year")

m1 <- lm(usa ~ usa_lag + chn + usa_mil + chn_mil +
           wealth_gdp_pc_ln + pop_ln + dem_inf_death_rate_ln +
           region + year,
         subset = usa > 0,
         data = d)
screenreg(m1, omit.coef = "cname|region|year")



# ----------------------------------- #

d2 <- d %>%
  mutate(across(.cols = c(usa, chn),
                .fns  = ~as.integer(.x>0))) %>%
  mutate(usa_mil = ifelse(usa_mil == "Military",1, 0),
         chn_mil = ifelse(chn_mil == "Military",1, 0))
m1 <- glm(usa ~ -1 + chn + usa_mil + chn_mil +
            wealth_gdp_pc_ln + pop_ln + dem_inf_death_rate_ln +
            cname + year,
          data = d2,
          family = binomial(link = "probit"))
m2 <- glm(chn ~ -1 + usa + usa_mil + chn_mil +
            wealth_gdp_pc_ln + pop_ln + dem_inf_death_rate_ln +
            cname + year,
          data = d2,
          family = binomial(link = "probit"))

screenreg(list(m1,m2), omit.coef = "cname|region|year")

#-----------------------------------------------------------------------------#











































qm(list(m1,m2))






#-----------------------------------------------------------------------------#

m <- mods$CHN

?vcovPL()

cfs <- coefficients(m)
vcv <- vcovPL(m, cluster = ~cname + year)
ses <- vcv %>% diag %>% sqrt
tst <- cfs / ses
n <- nrow(model.matrix(m))
k <- ncol(model.matrix(m))
pvl <- 2*pt(abs(tst), df=n-k,lower.tail= FALSE)
colnames(vcv) <- rownames(vcv) <- names(cfs)

screenreg(m,
          omit.coef        = c("region|cname|year"),
          override.se      = ses,
          override.pvalues = pvl)


#-----------------------------------------------------------------------------#
# TIDY DATA                                                               ----
#-----------------------------------------------------------------------------#

names(d)[str_detect(names(d), "health_")]



# mods <- list(
#   "USA" = lm(usa ~ usa_lag + chn + usa_mil + chn_mil+
#                region + year, data = z),
#   "CHN" = lm(chn ~ chn_lag + usa + usa_mil + chn_mil +
#                region + year, data = z)
# )
#
# screenreg(mods, omit.coef = "region|cname|year")
#
# mods <- list(
#   "USA" = lm(usa ~ usa_lag + chn + usa_mil + chn_mil +
#                wealth_gdppc_ln + pop_ln + dem_inf_death_rate_ln +
#                region + year, data = z),
#   "CHN" = lm(chn ~ chn_lag + usa + usa_mil + chn_mil +
#                wealth_gdppc_ln + pop_ln + dem_inf_death_rate_ln +
#                region + year, data = z)
# )

x <- qm(mods)
x

plt <- z %>%
  left_join(., d[, c("ccode","year","geometry")], by = c("ccode", "year")) %>%
  mutate(x = case_when(
    usa_mil == "Military"    & chn_mil == "Military"    ~ "Both",
    usa_mil == "Military"    & chn_mil == "No military" ~ "USA",
    usa_mil == "No military" & chn_mil == "Military"    ~ "CHN",
    usa_mil == "No military" & chn_mil == "No military" ~ "Neither")) %>%
  select(ccode, year, x, geometry) %>%
  st_set_geometry(., "geometry") %>%
  mutate(year = forcats::fct_drop(year))
table(plt$year, plt$x)

ggplot(data = plt) +
  geom_sf(aes(fill = x)) +
  theme_minimal() +
  facet_wrap(~year)

#-----------------------------------------------------------------------------#

# want exp(b1) dynamic responses to 1-unit shift in other country spending
# @ MIL = 1, MIL = 0
#
# NOTE - OTHER_COUNTRY variable is logged / MILITARY variable is NOT LOGGED!!!



mod <- mods$CHN
vcv <- vcovCL(mod, cluster = ~cname + year, type = "HC1")

# ----------------------------------- #
# Dynamic responses - setup:
# ----------------------------------- #
sim_years <- 10
sim_ci    <- 0.90
# ----------------------------------- #

# ----------------------------------- #
# China responding to US
# ----------------------------------- #
dvl_chn <- "chn_lag"
iv_chn  <- "usa"
vcv_chn <- vcv[c(dvl_chn, iv_chn), c(dvl_chn, iv_chn)]

source("Scripts/Scripts-V2/functions-dynamics.R")

dyn_chn_usa <- dynamics(model     = mod,
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
lrss_chn$lrss %>% round(., 3)

# Temporary shocks eventually fade to 0; permanent shocks eventually accumulate
# to the LRSS response. So how long this eventually?
# Most common: time until 90% has faded/accumulated:
# Calculate the 90% life of effect: i.e., time until 90% of the effect of a
# shock fades away:
lrss_chn$half_life %>% round(., 3)
# ----------------------------------- #



plt_data <- bind_rows("China" = dyn_chn_usa, .id = "model") %>%
  mutate(effect = str_to_sentence(effect))

# NOTE --- LOG-LOG HERE? THESE %

ggplot(data = plt_data, aes(x = time)) +
  geom_ribbon(aes(ymin = lb, ymax = ub, fill = effect), alpha = 0.2) +
  geom_line(aes(y = shock, color = effect), size = 0.2)

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
    legend.text        = element_text(size = 16/.pt),
    axis.text          = element_text(size = 16/.pt),
    axis.title         = element_text(size = 18/.pt),
    # plot.title         = element_text(size = 20/.pt),
    plot.title         = element_blank(),
    # plot.subtitle      = element_text(size = 18/.pt),
    plot.subtitle      = element_blank(),
    plot.caption       = element_text(size = 16/.pt),
    strip.text         = element_text(size = 18/.pt)
  ) +

  labs(y        = "Health aid allocation change (%)",
       # title    = "Great Power: Health aid responsiveness",
       # subtitle = "Ten year predicted response to 1% increase in allocations of the other power",
       caption  = "Based on estimates of full model with controls.\nConfidence intervals computed with parametric simulation.")



















#-----------------------------------------------------------------------------#





























#-----------------------------------------------------------------------------#

vrs <- c("health_chn_dah", "wealth_gdp_ln")

# ----------------------------------- #
# Model Formulas
# ----------------------------------- #
forms <- list(
  "base" = . ~ 1 + health_usa_usaidm_bin + health_chn_aidatm_bins +
    region + year,
  "full" = . ~ 1 + health_usa_usaidm_bin + health_chn_aidatm_bins +
    region + year +
    vdem + I(vdem^2) +
    ally_nonagg +
    # ally_US.nonagg + ally_CN.nonagg +
    # wealth_exports_ln + wealth_imports_ln +
    wealth_gdp_ln + pop_ln +
    dem_life_expec_ln + dem_inf_death_rate_ln +
    dis_daly_ncd_percent + dis_daly_cd_percent
)
# ----------------------------------- #

names(d)[str_detect(names(d), "health_")]

z <- "a ~ b + c + ."
z <- sprintf("health_%s_%s ~ health_%s_%s_lag + health_%s_%s + health_%s_%s_lag",
             "usa")

update(x, z)

#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# ANALYSIS                                                                ----
#-----------------------------------------------------------------------------#
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
  lm(formula = update(f, health_chn_dah_per ~ health_chn_dah_per_lag + health_usa_dah_per + health_usa_dah_per_lag + .),
     data    = d)
  # rlm(formula = update(f, dah_per_chn ~ dah_per_chn_lag + dah_per_usa_lag + .),
  #     data    = d,
  #     psi     = psi.huber,
  #     maxit   = 200)
})

usa <- lapply(forms, function(f){
  lm(formula = update(f, health_usa_dah_per ~ health_usa_dah_per_lag + health_chn_dah_per + health_chn_dah_per_lag + .),
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
  vcv <- vcovPC(mod, cluster = ~ ccode + year)
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
          omit.coef        = c("region|cname|year"),
          # override.se      = ses,
          # override.pvalues = pvl,
          # ci.force         = TRUE,
          custom.header    = list("United States" = 1:2, "China" = 3:4),
          custom.gof.rows  = list("Region FE" = rep("Yes", 4),
                                  "Year FE"   = rep("Yes", 4)),
          custom.note      = paste0("%stars\n",
                                    "Panel corrected standard errors clustered on country and year in parentheses."))


#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# SAVE                                                                    ----
#-----------------------------------------------------------------------------#
#save.image()
#rm(list = ls())
#-----------------------------------------------------------------------------#


#-----------------------------------------------------------------------------#
# ANALYSIS                                                                ----
#-----------------------------------------------------------------------------#
yr <- pnl %>%
  st_drop_geometry %>%
  group_by(year) %>%
  summarize(usa = sum(health_usa_total / 1e6),
            chn = sum(health_chn_total / 1e6)) %>%
  ungroup %>%
  mutate(across(.cols = c(usa, chn),
                .fns  = ~log(.x)))

# ggplot(data = yr, aes(x = year)) +
#   geom_line(aes(y = usa, color = "USA")) +
#   geom_line(aes(y = chn, color = "China"))

cor.test(yr$chn, yr$usa)

t.test(pnl$health_usa_total ~ pnl$health_chn_mil_bin_strict)
t.test(pnl$health_usa_total ~ pnl$health_chn_mil_bin_plausible)
t.test(pnl$health_usa_total ~ pnl$health_chn_mil_bin_medical_team)
t.test(pnl$health_chn_total ~ pnl$health_usa_mil_bin)

m1 <- lm(health_usa_total_per ~ health_chn_total_per + health_chn_mil_bin_plausible +
           year + cname, data = pnl)
m2 <- lm(health_chn_total_per ~ health_usa_total_per + health_usa_mil_bin +
           year + cname, data = pnl)
library(texreg)

screenreg(l = list(m1, m2),
          omit.coef = "cname|year")


m1 <- lm(health_usa_total_per ~ health_usa_total_per_lag +
           health_chn_total_per + health_chn_mil_bin_plausible +
           year + cname, data = pnl)
m2 <- lm(health_chn_total_per ~ health_chn_total_per_lag +
           health_usa_total_per + health_usa_mil_bin +
           year + cname, data = pnl)

screenreg(l = list(m1, m2),
          omit.coef = "cname|year")

#-----------------------------------------------------------------------------#
