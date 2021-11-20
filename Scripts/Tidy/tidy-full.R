#-----------------------------------------------------------------------------#
#
# Author:        Logan Stundal
# Date:          November 17, 2021
# Purpose:       Tidy full - join all dvs, ivs, and control variables
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

#---------------------------#
# Load required packages
#---------------------------#
library(tidyverse)
library(sf)
library(countrycode)
#---------------------------#

#---------------------------#
# Load data
#---------------------------#
load("Data/tidy-data/tidy-pnl.Rdata")
load("Data/tidy-data/tidy-usa.Rdata")
load("Data/tidy-data/tidy-chn.Rdata")
load("Data/tidy-data/tidy-dah.Rdata")
load("Data/tidy-data/tidy-ctr.Rdata")
#---------------------------#
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# JOIN DATA                                                               ----
#-----------------------------------------------------------------------------#
# ----------------------------------- #
# Year range
# ----------------------------------- #
yr_min <- 2000
yr_max <- 2017
# ----------------------------------- #

# ----------------------------------- #
# Create full panel
# ----------------------------------- #
d <- pnl %>%
  filter(year %in% yr_min:yr_max) %>%
  left_join(., dah, by = c("ccode", "year")) %>%
  left_join(., usa, by = c("ccode", "year")) %>%
  left_join(., chn, by = c("ccode", "year")) %>%
  left_join(., ctr, by = c("ccode", "year")) %>%

  arrange(ccode, year) %>%
  mutate(
    # across(.cols = c(ccode, year),
    #             .fns  = as_factor),
         region = countrycode(ccode, "cown", "region")) %>%
  select(cname:caplat, region, everything()) %>%
  janitor::clean_names()

rm(dah, usa, chn, ctr, pnl, yr_min, yr_max)
# ----------------------------------- #

# d <- d %>%
#   group_by(year) %>%
#   mutate(health_chn_aidat_per = 100 * (health_chn_aidat) / sum(health_chn_aidat),
#          health_chn_aidat_per_lag = 100 * (health_chn_aidat_lag) / sum(health_chn_aidat_lag))

#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# SAVE                                                                    ----
#-----------------------------------------------------------------------------#
save(d, file = "Data/health-diplomacy_20211118.Rdata")
rm(list = ls())
#-----------------------------------------------------------------------------#
