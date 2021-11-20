#-----------------------------------------------------------------------------#
#
# Author:        Logan Stundal
# Date:          November 18, 2021
# Purpose:       Tidy control variables
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
library(readxl)
#---------------------------#

#---------------------------#
# Load data
#---------------------------#
load("Data/tidy-data/tidy-pnl.Rdata")

gdp   <- read_xlsx("Data/Controls/un_stats-gdp.xlsx")
gdppc <- read_xlsx("Data/Controls/un_stats-gdp_pc.xlsx")
pop   <- read_csv("Data/Controls/un_stats-pop_thousands.csv")
dem   <- read_csv("Data/Controls/un_stats-demographics.csv")
dis   <- read_csv("Data/Controls/GHD-Disease/GBD_2019.csv")
vdem  <- vdemdata::vdem
ally  <- read_csv("Data/Controls/ATOP 5_0/atop5_0ddyr.csv")
#---------------------------#

#---------------------------#
# Functions
#---------------------------#
gen_cc <- function(data, cid = "recipient_country", quiet = TRUE){
  if(quiet){
    data <- data %>%
      mutate(ccode = case_when(
        !!sym(cid) == "Serbia" ~ 345,
        TRUE ~ suppressWarnings({countrycode(!!sym(cid), "country.name", "cown")})
      ))
  } else{
    data <- data %>%
      mutate(ccode = case_when(
        !!sym(cid) == "Serbia" ~ 345,
        TRUE ~ countrycode(!!sym(cid), "country.name", "cown")
      ))
  }
  return(data)
}
#---------------------------#
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# CONTROL VARIABLES                                                       ----
#-----------------------------------------------------------------------------#
# ----------------------------------- #
# Years
# ----------------------------------- #
yr_min <- 2000
yr_max <- 2019
# ----------------------------------- #


# ----------------------------------- #
# GDP (Source: UN Stats)
# ----------------------------------- #
# GDP
gdp <- gdp %>%
  select(-CountryID) %>%
  filter(IndicatorName %in% c("Exports of goods and services",
                              "Imports of goods and services",
                              "Gross Domestic Product (GDP)")) %>%
  pivot_longer(.,
               cols      = !c(Country, IndicatorName),
               names_to  = "year",
               values_to = "val") %>%
  drop_na(val) %>%
  filter(year %in% yr_min:yr_max,
         !(year <= 2010 & Country %in% c("Sudan", "South Sudan"))) %>%
  mutate(Country = case_when(Country %in% c("Sudan", "South Sudan") & year == 2011 ~ "Sudan",
                             Country == "Sudan (Former)" ~ "Sudan",
                             TRUE ~ Country),

         IndicatorName = case_when(IndicatorName == "Exports of goods and services" ~ "exports",
                                   IndicatorName == "Imports of goods and services" ~ "imports",
                                   IndicatorName == "Gross Domestic Product (GDP)"  ~ "gdp")) %>%

  # Resolve Sudan duplicates issue
  group_by(Country, year, IndicatorName) %>%
  summarize(val = mean(val), .groups = "keep") %>%
  ungroup %>%

  pivot_wider(.,
              id_cols     = c(Country, year),
              names_from  = IndicatorName,
              values_from = val) %>%

  gen_cc(., "Country") %>%
  select(ccode, everything(), -Country) %>%
  drop_na(ccode) %>%
  mutate(across(.cols  = !c(ccode, year),
                .fns   = ~log(.x),
                .names = "{.col}_ln"),
         year   = as.numeric(year)) %>%
  rename_with(.cols = !c(ccode, year),
              .fn  = ~paste0("wealth_", .x))

# GDP Per capita
gdppc <- gdppc %>%
  select(-CountryID) %>%
  pivot_longer(.,
               cols      = !Country,
               names_to  = "year",
               values_to = "gdp_pc") %>%
  drop_na(gdp_pc) %>%

  filter(year %in% yr_min:yr_max,
         !(year <= 2010 & Country %in% c("Sudan", "South Sudan"))) %>%
  mutate(Country = case_when(Country %in% c("Sudan", "South Sudan") & year == 2011 ~ "Sudan",
                             Country == "Sudan (Former)" ~ "Sudan",
                             TRUE ~ Country)) %>%

  # Resolve Sudan duplicates issue
  group_by(Country, year) %>%
  summarize(gdp_pc = mean(gdp_pc), .groups = "keep") %>%
  ungroup %>%

  gen_cc(., cid = "Country") %>%
  select(ccode, everything(), -Country) %>%
  drop_na(ccode) %>%
  mutate(gdp_pc_ln = log(gdp_pc),
         year      = as.numeric(year)) %>%
  rename_with(.cols = !c(ccode, year),
              .fn  = ~paste0("wealth_", .x))
# ----------------------------------- #


# ----------------------------------- #
# Population (Source: UN Stats)
# ----------------------------------- #
pop <- pop %>%
  gen_cc(., "country") %>%
  drop_na(ccode) %>%
  select(ccode, everything(), -country) %>%
  pivot_longer(.,
               cols      = !ccode,
               names_to  = "year",
               values_to = "pop") %>%
  filter(year %in% yr_min:yr_max) %>%
  mutate(pop    = pop * 1e3,
         year   = as.numeric(year)) %>%
  mutate(pop_ln = log(pop)) %>%
  select(ccode, year, everything())
# ----------------------------------- #


# ----------------------------------- #
# Demographics (Source: UN Stats)
# ----------------------------------- #
dem <- dem %>%
  rename(death_rate      = `Crude death rate (deaths per 1,000 population)`,
         life_expec      = `Life expectancy at birth, both sexes combined (years)`,
         inf_death_rate  = `Infant mortality rate (infant deaths per 1,000 live births)`,
         und5_death_rate = `Under-five mortality (deaths under age 5 per 1,000 live births)`,
         birth_rate      = `Crude birth rate (births per 1,000 population)`,
         fert_rate       = `Total fertility (live births per woman)`,
         pop_gr_rate     = `Population growth rate (percentage)`) %>%
  filter(year %in% yr_min:yr_max) %>%
  gen_cc(., "country") %>%
  drop_na(ccode) %>%
  select(ccode, year, contains("_")) %>%
  mutate(across(.cols = !c(ccode, year, pop_gr_rate),
                .fns  = ~ log(.x),
                .names= "{.col}_ln")) %>%
  select(ccode, year, everything()) %>%
  rename_with(.cols = !c(ccode, year),
              .fn  = ~paste0("dem_", .x))
# ----------------------------------- #


# ----------------------------------- #
# Disease data (Source: Global Health Data Exchange)
# ----------------------------------- #
dis <- dis %>%
  filter(year %in% yr_min:yr_max,
         cause_name %in% c("All causes",
                           "Communicable, maternal, neonatal, and nutritional diseases",
                           "Non-communicable diseases")) %>%
  pivot_wider(.,
              id_cols     = c(location_name, year),
              names_from  = c(measure_name, cause_name, metric_name),
              names_sep   = "_",
              values_from = val) %>%
  gen_cc(., "location_name") %>%
  drop_na(ccode) %>%
  select(ccode, year, everything()) %>%
  rename_with(., function(x){str_replace_all(x, "_Non-communicable diseases_", "_NCD_")}, everything()) %>%
  rename_with(., function(x){str_replace_all(x, "_All causes_",                "_AllCause_")}, everything()) %>%
  rename_with(., function(x){str_replace_all(x, "_Communicable, maternal, neonatal, and nutritional diseases_",
                                             "_CD_")}, everything()) %>%
  rename_with(., function(x){str_replace_all(x, "s [(]Disability-Adjusted Life Years[)]", "")}, everything()) %>%
  mutate(across(.cols = c(DALY_NCD_Rate, DALY_CD_Rate),
                .fns  = ~log(.x),
                .names= "{.col}_ln")) %>%
  select(ccode, year, everything()) %>%
  rename_with(.cols = !c(ccode, year),
              .fn  = ~paste0("dis_", .x))
# ----------------------------------- #


# ----------------------------------- #
# Democracy Indicators (Source: V-Dem)
# ----------------------------------- #
"v2x_polyarchy - Electoral Democracy Index (D)
To what extent is the ideal of electoral democracy in its fullest sense achieved?

In the V-Dem conceptual scheme, electoral democracy is understood as an
essential element of any other conception of representative democracy -
liberal, participatory, deliberative, egalitarian, or some other.

Scale: Interval, from low to high (0-1)"

vdem <- vdem %>%
  select(country_name, year,
         v2x_polyarchy,v2x_libdem,v2x_partipdem,v2x_delibdem,v2x_egaldem) %>%
  filter(year %in% yr_min:yr_max) %>%
  gen_cc(., "country_name") %>%
  drop_na(ccode) %>%
  rowwise() %>%
  mutate(vdem = mean(c_across(v2x_polyarchy:v2x_egaldem))) %>%
  select(ccode, year, vdem)
# ----------------------------------- #


# ----------------------------------- #
# Alliances (Source: ATOP)
# ----------------------------------- #
ally <- ally %>%
  filter(stateA %in% c(2, 710),
         year   %in% yr_min:yr_max) %>%
  rename(ccode = stateB) %>%
  mutate(ally  = case_when(stateA == 2 ~ "ally_usa", TRUE ~ "ally_chn")) %>%
  select(ccode, year, ally, defense, nonagg) %>%
  pivot_wider(.,
              id_cols     = c(ccode, year),
              names_from  = ally,
              names_glue  = "{ally}_{.value}",
              values_from = c(defense, nonagg))
# ----------------------------------- #
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# MERGE DATA / PANEL                                                      ----
#-----------------------------------------------------------------------------#

# ----------------------------------- #
# Tidy panel - remove spatial and non-index vars for now
# ----------------------------------- #
pnl <- pnl %>%
  st_drop_geometry %>%
  select(ccode, year)
# ----------------------------------- #

# ----------------------------------- #
# Merge data
# ----------------------------------- #
ctr <- pnl %>%
  left_join(., gdp,   by = c("ccode", "year")) %>%
  left_join(., gdppc, by = c("ccode", "year")) %>%
  left_join(., pop,   by = c("ccode", "year")) %>%
  left_join(., dem,   by = c("ccode", "year")) %>%
  left_join(., dis,   by = c("ccode", "year")) %>%
  left_join(., vdem,  by = c("ccode", "year")) %>%
  left_join(., ally,  by = c("ccode", "year")) %>%

  # Code NA DAH_ to 0. Intuition: no reported DAH indicates no provided DAH
  mutate(across(.cols = starts_with("dah_"),
                .fns  = ~replace_na(.x, 0))) %>%

  # Tidy alliance data:
  mutate(across(.cols = contains("ally_"),
                .fns  = ~replace_na(.x, 0))) %>%
  mutate(ally_nonagg = case_when(
    (ally_usa_nonagg == 0 & ally_chn_nonagg == 0) ~ "No pacts",
    (ally_usa_nonagg == 1 & ally_chn_nonagg == 0) ~ "US NonAgg",
    (ally_usa_nonagg == 0 & ally_chn_nonagg == 1) ~ "CN NonAgg",
    (ally_usa_nonagg == 1 & ally_chn_nonagg == 1) ~ "Both NonAgg"
  )) %>%
  mutate(ally_nonagg = factor(ally_nonagg,
                              levels = c("No pacts",  "US NonAgg",
                                         "CN NonAgg", "Both NonAgg")),
         across(.cols = c(ally_usa_defense, ally_chn_defense,
                          ally_usa_nonagg,  ally_chn_nonagg),
                .fns  = ~ as.factor(.x))) %>%

  # Disease data "dis" begin in 2000; (filter >= 2000) below for this check.
  # Note - all missing vals occur in Micro-states and Taiwan.
  # Drop these as is typical (and Taiwan bc China...). See code commented out
  # below for validation of which cases are dropped. Note, do not run the
  # drop_na() command for the code below to work.
  drop_na() %>%

  # Arrange variables
  arrange(year, ccode) %>%
  select(ccode, year,
         starts_with("dem_"),
         starts_with("dis_"),
         starts_with("ally_"),
         starts_with("wealth_"),
         everything())

# # Counties with missing values dropped
# miss <- d[rowSums(is.na(d)) > 0,] %>% filter(year %in% as.character(2000:2018)) %>%
#   mutate(cname = countrycode(ccode, "cown", "country.name"))
# table(miss$cname)
# ----------------------------------- #
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# SAVE                                                                    ----
#-----------------------------------------------------------------------------#
save(ctr, file = "Data/tidy-data/tidy-ctr.Rdata")
rm(list = ls())
#-----------------------------------------------------------------------------#



