#-----------------------------------------------------------------------------#
#
# Author:        Logan Stundal
# Date:          November 17, 2021
# Purpose:       Tidy DAH data into country-year format
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
library(countrycode)
library(sf)
#---------------------------#

#---------------------------#
# Load data
#---------------------------#
load("Data/tidy-data/tidy-pnl.Rdata")
dah <- read_csv("Data/Health-Aid_DAH/DAH_1990_2019_DATA.csv")
#---------------------------#
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# TIDY DATA                                                               ----
#-----------------------------------------------------------------------------#
# ----------------------------------- #
# Year range
# ----------------------------------- #
yr_min <- 1999
yr_max <- 2019
# ----------------------------------- #


# ----------------------------------- #
# Tidy DAH data
# ----------------------------------- #
# Summarize operation
dah <- dah %>%
  filter(source  %in% c("China", "United_States"),
         year    %in% yr_min:yr_max,
         # elim_ch: 1= Drop to avoid double counting transfers between channels
         elim_ch != 1) %>%
  mutate(dah_19 = as.numeric(dah_19) * 1e3) %>%
  group_by(source, recipient_country,  year) %>%
  summarize(health_dah = sum(dah_19, na.rm = TRUE),
            .groups = "keep") %>%
  ungroup

# Pivot operation
dah <- dah %>%
  mutate(source = case_when(source == "China" ~ "health_chn_dah",
                            TRUE              ~ "health_usa_dah")) %>%
  pivot_wider(.,
              id_cols     = c(source, recipient_country, year),
              names_from  = source,
              values_from = health_dah) %>%
  mutate(across(starts_with("health_"), .fns = ~replace_na(.x, 0))) %>%
  group_by(year) %>%
  mutate(health_chn_dah_per = 100 * (health_chn_dah / sum(health_chn_dah)),
         health_usa_dah_per = 100 * (health_usa_dah / sum(health_usa_dah))) %>%
  ungroup() %>%
  mutate(ccode = case_when(
    recipient_country == "Serbia" ~ 345,
    TRUE ~ countrycode(recipient_country, "country.name", "cown")
  )) %>%
  drop_na(ccode) %>%
  select(-recipient_country)
# ----------------------------------- #


# ----------------------------------- #
# Merge to panel
# ----------------------------------- #
# Remove spatial data and non-index vars from panel - will restore later
pnl <- pnl %>% st_drop_geometry %>% select(ccode, year)

# Merge dah data to panel, fill out missing values with 0
dah <- left_join(pnl, dah, by = c("ccode", "year")) %>%
  mutate(across(.cols = contains("health_"),
                .fns  = ~replace_na(.x, 0))) %>%
  group_by(ccode) %>%
  mutate(across(.cols = starts_with("health_"),
                .fns  = ~dplyr::lag(.x, 1),
                .names= "{.col}_lag")) %>%
  ungroup
# ----------------------------------- #
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# SAVE                                                                    ----
#-----------------------------------------------------------------------------#
save(dah, file = "Data/tidy-data/tidy-dah.Rdata")
rm(list = ls())
#-----------------------------------------------------------------------------#
