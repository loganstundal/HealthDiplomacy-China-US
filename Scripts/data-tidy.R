#-----------------------------------------------------------------------------#
#
# Author:        Logan Stundal
# Date:          September 16, 2021
# Purpose:       Note
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
library(cshapes)
library(sf)
library(rmapshaper)
library(readxl)
#---------------------------#

#---------------------------#
# Load data
#---------------------------#
dah <- read_csv("Data/DAH-Database/DAH_1990_2019_DATA.csv")
#---------------------------#

#---------------------------#
# Local functions
#---------------------------#
gen_cc <- function(data, cid = "recipient_country"){
  data <- data %>%
    mutate(ccode = suppressWarnings({
      countrycode(!!sym(cid), "country.name", "cown")
    })) %>%
    mutate(ccode = case_when(!!sym(cid) == "Serbia" ~ 345, TRUE ~ ccode))
  return(data)
}
#---------------------------#
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# TIDY ADMIN                                                              ----
#-----------------------------------------------------------------------------#
# ----------------------------------- #
# Years - based on available China data
# ----------------------------------- #
yr_min <- 1998
yr_max <- 2017
# ----------------------------------- #

# ----------------------------------- #
# Panel
# ----------------------------------- #
# Create panel
pnl <- lapply(seq(yr_min, yr_max), function(yr){
  cshp(date = as.Date(sprintf("%s-06-01",yr)), useGW = FALSE) %>%
    ms_simplify(., keep = 0.05, keep_shapes = TRUE) %>%
    st_transform(., "+proj=robin") %>%
    mutate(year = yr)
}) %>% bind_rows() %>%
  rename(ccode = cowcode) %>%
  select(ccode, year) %>%
  mutate(cid = paste(ccode, year, sep = "_"))

# ID variables
pnl <- pnl %>%
  drop_na(ccode) %>%
  mutate(cname   = countrycode(ccode, "cown", "country.name"),
         cregion = countrycode(ccode, "cown", "region"))
# ----------------------------------- #
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# DAH TIDY                                                                ----
#-----------------------------------------------------------------------------#
# ----------------------------------- #
# Create country codes / regions
# ----------------------------------- #
# This approach is faster.
ccodes <- data.frame(
  "recipient_country" = sort(unique(dah$recipient_country))
  ) %>%
  gen_cc(.) %>%
  drop_na(ccode)
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
  left_join(., ccodes, by = "recipient_country") %>%
  mutate(dah_19     = as.numeric(dah_19) * 1e3) %>%
  group_by(source, ccode,  year) %>%
  summarize(dah_19  = sum(dah_19, na.rm = TRUE),
            .groups = "keep") %>%
  ungroup() %>%
  drop_na(ccode)

# Pivot operation
dah <- dah %>%
  mutate(source = case_when(source == "China" ~ "dah_usd_chn",
                            TRUE              ~ "dah_usd_usa")) %>%
  pivot_wider(.,
              id_cols     = c(source, ccode, year),
              names_from  = source,
              values_from = dah_19) %>%
  mutate(across(starts_with("dah_"), .fns = ~replace_na(.x, 0))) %>%
  group_by(year) %>%
  mutate(dah_per_chn = 100 * (dah_usd_chn / sum(dah_usd_chn, na.rm = T)),
         dah_per_usa = 100 * (dah_usd_usa / sum(dah_usd_usa, na.rm = T))) %>%
  ungroup() %>%
  mutate(cid = paste(ccode, year, sep = "_")) %>%
  select(cid, starts_with("dah_"))
# ----------------------------------- #
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# CONTROL VARIABLES                                                       ----
#-----------------------------------------------------------------------------#
# ----------------------------------- #
# GDP (Source: UN Stats)
# ----------------------------------- #
# GDP
gdp <- read_xlsx("Data/Controls/un_stats-gdp.xlsx") %>%
  filter(IndicatorName %in% c("Exports of goods and services",
                              "Imports of goods and services",
                              "Gross Domestic Product (GDP)")) %>%
  gen_cc(., "Country") %>%
  select(ccode, everything(), -c(Country, CountryID)) %>%
  drop_na(ccode) %>%
  pivot_longer(.,
               cols      = !c(ccode, IndicatorName),
               names_to  = "year",
               values_to = "val") %>%
  filter(year %in% yr_min:yr_max) %>%
  drop_na(val) %>%
  mutate(IndicatorName = case_when(IndicatorName == "Exports of goods and services" ~ "exports",
                                   IndicatorName == "Imports of goods and services" ~ "imports",
                                   IndicatorName == "Gross Domestic Product (GDP)"  ~ "gdp")) %>%
  # Sudan and S. Sudan are distinct pre breakup (begins 2012 per cshps)
  group_by(ccode, year, IndicatorName) %>%
  summarize(val = mean(val), .groups = "keep") %>%
  ungroup %>%
  pivot_wider(.,
              id_cols     = c(ccode, year),
              names_from  = IndicatorName,
              values_from = val) %>%
  mutate(across(.cols  = !c(ccode, year),
                .fns   = ~log(.x),
                .names = "{.col}_ln")) %>%
  mutate(cid         = paste(ccode, year, sep = "_"),
         net_exports = exports - imports) %>%
  select(cid, everything(), -ccode, -year)

# GDP Per capita
gdppc <- read_xlsx("Data/Controls/un_stats-gdp_pc.xlsx") %>%
  gen_cc(., cid = "Country") %>%
  select(ccode, everything(), -c(CountryID, Country)) %>%
  drop_na(ccode) %>%
  pivot_longer(.,
               cols      = !ccode,
               names_to  = "year",
               values_to = "gdp_pc") %>%
  filter(year %in% yr_min:yr_max) %>%
  mutate(cid = paste(ccode, year, sep = "_")) %>%
  drop_na(gdp_pc) %>%
  # Sudan and S. Sudan are distinct pre breakup (begins 2012 per cshps)
  group_by(cid) %>%
  summarize(gdp_pc = mean(gdp_pc)) %>%
  ungroup %>%
  mutate(gdp_pc_ln = log(gdp_pc))
# ----------------------------------- #


# ----------------------------------- #
# Population (Source: UN Stats)
# ----------------------------------- #
pop <- read_csv("Data/Controls/un_stats-pop_thousands.csv") %>%
  gen_cc(., "country") %>%
  drop_na(ccode) %>%
  select(ccode, everything(), -country) %>%
  pivot_longer(.,
               cols      = !ccode,
               names_to  = "year",
               values_to = "pop") %>%
  filter(year %in% yr_min:yr_max) %>%
  mutate(cid = paste(ccode, year, sep = "_"),
         pop = pop * 1e3) %>%
  mutate(pop_ln = log(pop)) %>%
  select(cid, everything(), -ccode, -year)
# ----------------------------------- #


# ----------------------------------- #
# Demographics (Source: UN Stats)
# ----------------------------------- #
dem <- read_csv("Data/Controls/un_stats-demographics.csv") %>%
  rename(death_rate = `Crude death rate (deaths per 1,000 population)`,
         life_expec = `Life expectancy at birth, both sexes combined (years)`,
         inf_death  = `Infant mortality rate (infant deaths per 1,000 live births)`,
         und5_death = `Under-five mortality (deaths under age 5 per 1,000 live births)`,
         birth_rate = `Crude birth rate (births per 1,000 population)`,
         fert_rate  = `Total fertility (live births per woman)`,
         pop_gr_rate= `Population growth rate (percentage)`) %>%
  filter(year %in% yr_min:yr_max) %>%
  gen_cc(., "country") %>%
  drop_na(ccode) %>%
  select(ccode, year, contains("_")) %>%
  mutate(cid = paste(ccode, year, sep = "_"),
         across(.cols = !c(cid, ccode, year),
                .fns  = ~ log(.x),
                .names= "{.col}_ln")) %>%
  # Note: warning of NaNs due to pop_gr_rate_ln which is dropped
  select(cid, everything(), -ccode, -year, -pop_gr_rate_ln)
# ----------------------------------- #


# ----------------------------------- #
# Disease data (Source: Global Health Data Exchange)
# ----------------------------------- #
dis <- read_csv("Data/Controls/GHD-Disease/GBD_2019.csv") %>%
  filter(year %in% yr_min:yr_max) %>%
  filter(cause_name %in% c("All causes",
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
  mutate(cid = paste(ccode, year, sep = "_")) %>%
  select(cid, everything(), -ccode, -year)
# ----------------------------------- #


# ----------------------------------- #
# Democracy Indicators (Source: V-Dem)
# ----------------------------------- #
"
v2x_polyarchy - Electoral Democracy Index (D)
To what extent is the ideal of electoral democracy in its fullest sense achieved?

In the V-Dem conceptual scheme, electoral democracy is understood as an
essential element of any other conception of representative democracy -
liberal, participatory, deliberative, egalitarian, or some other.

Scale: Interval, from low to high (0-1)
"

vdem <- vdemdata::vdem %>%
  select(country_name, year,
         v2x_polyarchy,v2x_libdem,v2x_partipdem,v2x_delibdem,v2x_egaldem) %>%
  filter(year %in% yr_min:yr_max) %>%
  gen_cc(., "country_name") %>%
  rowwise() %>%
  mutate(vdem = mean(c_across(v2x_polyarchy:v2x_egaldem)),
         cid  = paste(ccode, year, sep = "_")) %>%
  select(cid, vdem)
# ----------------------------------- #
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# MERGE DATA / PANEL                                                      ----
#-----------------------------------------------------------------------------#
d <- pnl %>%
  left_join(., dah,   by = "cid") %>%
  left_join(., gdp,   by = "cid") %>%
  left_join(., gdppc, by = "cid") %>%
  left_join(., pop,   by = "cid") %>%
  left_join(., dem,   by = "cid") %>%
  left_join(., dis,   by = "cid") %>%
  left_join(., vdem,  by = "cid") %>%
  janitor::clean_names() %>%

  # Code NA DAH_ to 0. Intuition: no reported DAH indicates no provided DAH
  mutate(across(.cols = starts_with("dah_"),
                .fns  = ~replace_na(.x, 0))) %>%

  # Create temporal variables:
  group_by(ccode) %>%
  mutate(across(.cols  = c(dah_usd_chn, dah_usd_usa, dah_per_chn, dah_per_usa),
                .fns   = ~ dplyr::lag(.x, 1),
                .names = "{.col}_lag")) %>%
  mutate(across(.cols  = c(dah_usd_chn, dah_usd_usa, dah_per_chn, dah_per_usa),
                .fns   = ~ .x - dplyr::lag(.x, 1),
                .names = "{.col}_diff")) %>%
  mutate(across(.cols  = contains("_diff"),
                .fns   = ~ dplyr::lag(.x, 1),
                .names = "{.col}_lag")) %>%
  ungroup %>%

  # Note - all missing vals occur in Micro-states and Taiwan.
  # Drop these as is typical (and Taiwan bc China...). See code commented out
  # below for validation of which cases are dropped. Note, do not run the
  # drop_na() command for the code below to work.
  #
  # Disease data "dis" begin in 2000, thus (filter >= 2000) below for this
  # check.
  drop_na() %>%

  # Arrange variables
  select(cid, ccode, year, cname, cregion,
         starts_with("dah_"),
         everything(),
         geometry) %>%
  mutate(ccode = as.factor(ccode),
         year  = as.factor(year)) %>%
  st_set_geometry(.,"geometry")

# Counties with missing values dropped
# miss <- d[rowSums(is.na(d)) > 0,] %>% filter(year >= 2000)
# table(miss$cname)
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# SAVE                                                                    ----
#-----------------------------------------------------------------------------#
save(d, file = "Data/HealthDiplomacy.Rdata")
rm(list = ls())
#-----------------------------------------------------------------------------#
