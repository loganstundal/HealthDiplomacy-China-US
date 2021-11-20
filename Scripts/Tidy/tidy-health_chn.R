#-----------------------------------------------------------------------------#
#
# Author:        Logan Stundal
# Date:          November 15, 2021
# Purpose:       Tidy Chinese health aid data into country-year format
#
#
# Copyright (c): Logan Stundal, 2021
# Email:         stund005@umn.edu
#
#-----------------------------------------------------------------------------#
#
# Notes:
#  Chinese Health Aid data source: Aid Data GCDF V2.0
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
library(countrycode)
library(sf)
#---------------------------#

#---------------------------#
# Load data
#---------------------------#
load("Data/tidy-data/tidy-pnl.Rdata")
chn <- read_csv("Data/Health-Aid_China/AidData-gcdf2-tidy_urls-coded.csv")
# imp <- read_csv("Data/Health-Aid_China/AidData-gcdf2-tidy-urls-implement_year.csv")
#---------------------------#
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# SUMMARIZE MILITARY-CODING DATA                                          ----
#-----------------------------------------------------------------------------#
# ----------------------------------- #
# Year range
# ----------------------------------- #
yr_min <- 2000
yr_max <- 2017
# ----------------------------------- #


# ----------------------------------- #
# Merge implementation year back in - because I forgot it...
# ----------------------------------- #
chn <- chn %>%
  rename(year = commitment_year) %>%
  filter(is.na(Drop)) %>%
  mutate(ccode = case_when(
    recipient == "Micronesia" ~ 987,
    recipient == "Serbia"     ~ 345,
    TRUE ~ countrycode(recipient, "country.name", "cown"))) %>%
  select(-Drop) %>%
  drop_na(ccode)
# ----------------------------------- #


# ----------------------------------- #
# Totals - Chinese health aid
# ----------------------------------- #
chn_f <- chn %>%
  group_by(id) %>%
  summarize(ccode  = ccode[1],
            year   = year[1],
            amount = amount_constant_usd2017[1],
            .groups= "keep") %>%
  ungroup %>%
  group_by(ccode, year) %>%
  summarize(health_chn_aidat = sum(amount, na.rm = TRUE),
            .groups= "keep") %>%
  ungroup
# ----------------------------------- #


# ----------------------------------- #
# Totals - Chinese military health aid (Note: Very limited data)
# ----------------------------------- #
chn_m <- chn %>%
  filter(military_coding %in% c("Military", "Plausible military")) %>%
  group_by(id) %>%
  summarize(ccode  = ccode[1],
            year   = year[1],
            amount = amount_constant_usd2017[1],
            .groups= "keep") %>%
  ungroup %>%
  group_by(ccode, year) %>%
  summarize(health_chn_aidatm = sum(amount, na.rm = TRUE),
            .groups= "keep") %>%
  ungroup
# ----------------------------------- #


# ----------------------------------- #
# Cases - Chinese military health aid, binary indicators
# ----------------------------------- #
chn_b <- chn %>%
  group_by(ccode, year) %>%
  summarize(health_chn_aidatm_binp = case_when(any(military_coding == "Military") |
                                               any(military_coding == "Plausible military") ~ "Military",
                                               TRUE ~ "No military"),
            health_chn_aidatm_bins = case_when(any(military_coding == "Military") ~ "Military",
                                               TRUE ~ "No military"),
            health_chn_aidatmt_bin = case_when(any(military_coding == "Medical team") ~ "Medical team",
                                               TRUE ~ "No medical team"),
            .groups = "keep") %>%
  ungroup
# ----------------------------------- #


# ----------------------------------- #
# Full Panel
# ----------------------------------- #
# Drop panel spatial data - this will be recovered when merging all variables
pnl <- pnl %>%
  select(ccode, year) %>%
  filter(year %in% yr_min:yr_max) %>%
  st_drop_geometry

# Merge variables and recode missing values as 0
chn <- pnl %>%
  left_join(., chn_f, by = c("ccode", "year")) %>%
  left_join(., chn_m, by = c("ccode", "year")) %>%
  left_join(., chn_b, by = c("ccode", "year")) %>%
  mutate(health_chn_aidat  = replace_na(health_chn_aidat,  0),
         health_chn_aidatm = replace_na(health_chn_aidatm, 0),
         health_chn_aidatm_binp = replace_na(health_chn_aidatm_binp, "No military"),
         health_chn_aidatm_bins = replace_na(health_chn_aidatm_bins, "No military"),
         health_chn_aidatmt_bin = replace_na(health_chn_aidatmt_bin, "No medical team")) %>%
  mutate(health_chn_aidatm_binp = factor(health_chn_aidatm_binp,
                                         levels = c("Military", "No military")),
         health_chn_aidatm_bins = factor(health_chn_aidatm_bins,
                                         levels = c("Military", "No military")),
         health_chn_aidatmt_bin = factor(health_chn_aidatmt_bin,
                                         levels = c("Medical team", "No medical team")))

# Create time lags
chn <- chn %>%
  group_by(ccode) %>%
  mutate(across(.cols = c(health_chn_aidat, health_chn_aidatm),
                .fns  = ~dplyr::lag(.x, 1),
                .names= "{.col}_lag"))

rm(chn_f, chn_m, chn_b, pnl, yr_min, yr_max)
# ----------------------------------- #
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# SAVE                                                                    ----
#-----------------------------------------------------------------------------#
save(chn, file = "Data/tidy-data/tidy-chn.Rdata")
rm(list = ls())
#-----------------------------------------------------------------------------#
