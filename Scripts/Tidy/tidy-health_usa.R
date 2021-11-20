#-----------------------------------------------------------------------------#
#
# Author:        Logan Stundal
# Date:          November 17, 2021
# Purpose:       Tidy Chinese health aid data into country-year format
#
#
# Copyright (c): Logan Stundal, 2021
# Email:         stund005@umn.edu
#
#-----------------------------------------------------------------------------#
#
# Notes:
#  US Health Aid data source: USAID Foreign Aid Complete Dataset (c. 2020)
#
#  Using 2016 constant USD for more comparable figures to the 2017 constant USD
#  in the Chinese AidData set. Cannot locate a deflator value for USD with 2017
#  as the base year.
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
usa <- read_csv("Data/Health-Aid_US/us_foreign_aid_complete.csv")
# def <- read_csv("Data/Controls/wb-gdp-deflators.csv")
#---------------------------#
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# TIDY DATA                                                               ----
#-----------------------------------------------------------------------------#
# ----------------------------------- #
# Year range
# ----------------------------------- #
yr_min <- 2000
yr_max <- 2019
# ----------------------------------- #


# ----------------------------------- #
# Variable subset, filter years, and subset health aid
# ----------------------------------- #
usa <- usa %>%
  mutate(year = suppressWarnings({as.numeric(fiscal_year)})) %>%
  filter(year %in% c(yr_min:yr_max),
         dac_category_name == "Health and Population",
         current_amount >= 0,
         # transaction_type_name == "Disbursements",
         funding_account_name  != "Foreign Military Financing Program") %>%
  select(country_name,
         funding_agency_name, funding_agency_acronym,
         # implementing_agency_name, implementing_agency_acronym, subagency_name,
         dac_category_name, dac_sector_name, dac_purpose_name,
         year,
         transaction_type_name,
         funding_account_name,
         activity_name, activity_description,
         activity_project_number,
         # current_amount,
         constant_amount) %>%
  rename(amount = constant_amount) %>%
  # left_join(.,def[,c("year","US_Defl_2015b")], by = "year") %>%
  # mutate(amount = current_amount / (US_Defl_2015b / 100)) %>%
  # select(-US_Defl_2015b, -current_amount) %>%
  mutate(ccode = case_when(
    country_name == "Korea, Democratic Republic"     ~ 731,
    country_name == "Serbia"                         ~ 345,
    country_name == "Serbia and Montenegro (former)" ~ 341,
    TRUE ~ countrycode(country_name, "country.name", "cown"))) %>%
  drop_na(ccode) %>%
  select(-country_name)

# rm(def, yr_min, yr_max)
# rm(yr_min, yr_max)
# ----------------------------------- #


# ----------------------------------- #
# Total health aid
# ----------------------------------- #
usa_f <- usa %>%
  group_by(ccode, year) %>%
  summarize(health_usa_usaid = sum(amount, na.rm = T),
            .groups = "keep") %>%
  ungroup
# ----------------------------------- #


# ----------------------------------- #
# Military health aid
# ----------------------------------- #
usa_m <- usa %>%
  filter(funding_agency_acronym %in% c("ARMY","DOD","NAVY")) %>%
  group_by(ccode, year) %>%
  summarize(health_usa_usaidm = sum(amount, na.rm = T),
            .groups = "keep") %>%
  ungroup %>%
  mutate(health_usa_usaidm_bin = "Military")
# ----------------------------------- #


# ----------------------------------- #
# Full Panel
# ----------------------------------- #
# Drop panel spatial data - this will be recovered when merging all variables
pnl <- pnl %>%
  select(ccode, year) %>%
  filter(year %in% yr_min:yr_max) %>%
  st_drop_geometry

usa <- pnl %>%
  left_join(., usa_f, by = c("ccode", "year")) %>%
  left_join(., usa_m, by = c("ccode", "year")) %>%
  mutate(health_usa_usaid      = replace_na(health_usa_usaid, 0),
         health_usa_usaidm     = replace_na(health_usa_usaidm, 0),
         health_usa_usaidm_bin = replace_na(health_usa_usaidm_bin,
                                               "No military")) %>%
  mutate(health_usa_usaidm_bin = factor(health_usa_usaidm_bin,
                                        levels = c("Military", "No military")))

# Create percent measures
usa <- usa %>%
  group_by(year) %>%
  mutate(health_usa_usaid_per  = 100 * (health_usa_usaid /  sum(health_usa_usaid)),
         health_usa_usaidm_per = 100 * (health_usa_usaidm / sum(health_usa_usaidm))) %>%
  ungroup %>%
  mutate(across(.cols = contains("_per"),
                .fns  = ~ifelse(is.nan(.x), NA, .x)))

# Create time lags
usa <- usa %>%
  group_by(ccode) %>%
  mutate(across(.cols = starts_with("health_"),
                .fns  = ~dplyr::lag(.x, 1),
                .names= "{.col}_lag"))

rm(usa_f, usa_m, pnl, yr_min, yr_max)
# ----------------------------------- #
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# SAVE                                                                    ----
#-----------------------------------------------------------------------------#
save(usa, file = "Data/tidy-data/tidy-usa.Rdata")
rm(list = ls())
#-----------------------------------------------------------------------------#



