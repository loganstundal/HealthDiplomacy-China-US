#-----------------------------------------------------------------------------#
#
# Author:        Logan Stundal
# Date:          September 17, 2021
# Purpose:       Descriptive statistics
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
library(kableExtra)
library(sf)
#---------------------------#

#---------------------------#
# Load data
#---------------------------#
load("Data/HealthDiplomacy.Rdata")
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
# DESCRIPTIVE STATISTICS                                                  ----
#-----------------------------------------------------------------------------#
# ----------------------------------- #
# Summary statistics
# ----------------------------------- #
desc <- d %>%
  st_drop_geometry() %>%
  select(dah_per_usa, dah_per_chn,
         vdem,
         ally_US.nonagg, ally_CN.nonagg,
         wealth_gdp_ln, pop_ln,
         dem_life_expec_ln, dem_inf_death_rate_ln,
         dis_daly_ncd_percent, dis_daly_cd_percent) %>%
  rename(ally_US_nonagg = ally_US.nonagg,
         ally_CN_nonagg = ally_CN.nonagg) %>%
  mutate(across(.cols = everything(),
                .fns  = ~as.numeric(as.character(.x)))) %>%
  summarize(across(.cols = everything(),
                   .fns  = list("Min"  = min,
                                "Mean" = mean,
                                "Max"  = max,
                                "SD"   = sd),
                   .names= "{.col}.{.fn}")) %>%
  pivot_longer(.,
               cols      = everything(),
               names_to  = c("variable", "stat"),
               names_sep = "[.]",
               values_to = "val") %>%
  pivot_wider(.,
              id_cols     = "variable",
              names_from  = "stat",
              values_from = "val") %>%
  as.data.frame()
# ----------------------------------- #


# ----------------------------------- #
# Descriptive statistics table
# ----------------------------------- #
rownames(desc) <- c("US Health Aid",
                    "CN Health Aid",
                    "Democracy",
                    "US Non-Aggression Pact",
                    "CN Non-Aggression Pact",
                    "GDP (log)",
                    "Population (log)",
                    "Life expectancy (log)",
                    "Infant Mortality Rate (log)",
                    "Disease (non-communicable) rate (log)",
                    "Disease (communicable) rate (log)")
desc$variable <- NULL

kbl(x         = desc,
    format    = "pipe",
    digits    = 2,
    row.names = TRUE) %>%
  save_kable(.,
             file = "Results/Tables/descriptive-statistics.txt")
# ----------------------------------- #
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# SAVE                                                                    ----
#-----------------------------------------------------------------------------#
#save.image()
rm(list = ls())
#-----------------------------------------------------------------------------#
