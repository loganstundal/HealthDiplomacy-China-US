#-----------------------------------------------------------------------------#
#
# Author:        Logan Stundal
# Date:          February 10, 2021
# Purpose:       Tidy and combine US and Chinese aid / health aid data
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
library(readxl)
library(sf)

#---------------------------#
# Set working directory
#---------------------------#
setwd("C:/Users/logan/GoogleDrive/UMN/CLASSES/2021 SPRING/RA - Nisha/Health-Diplomacy/")

#---------------------------#
# Load data
#---------------------------#
defl       <- read_csv("data/data-source/WB-GDP-Deflators.csv")
china_full <- read_xlsx(paste0("data/data-source/china/GlobalChineseOfficialFinanceDataset_v1.0/",
                               "GlobalChineseOfficialFinanceDataset_v1.0.xlsx"))
us_full    <- read_csv("data/data-source/us/us_foreign_aid_complete.csv")
ghdx       <- read_csv("Data/GHDx_DAH_DATABASE_1990_2019_CSV/IHME_DAH_DATABASE_1990_2019_Y2020M04D23.CSV")

#---------------------------#
# Local Functions
#---------------------------#
gen_cc <- function(d, cname){
  d <- d %>%
    mutate(ccode = suppressWarnings({countrycode::countrycode(!!sym(cname), "country.name","cown")})) %>%
    mutate(ccode = case_when(str_detect(!!sym(cname),  "Serbia") ~ 345,
                             str_detect(!!sym(cname),  "Serbia and Montenegro (former)") ~ 345,
                             str_detect(!!sym(cname),  "Korea, Democratic Republic") ~ 731,
                             (str_detect(!!sym(cname), "South Sudan") & year < 2011) ~ 625,
                             TRUE ~ ccode))
  return(d)
}
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# FILTERING VARIABLES                                                     ----
#-----------------------------------------------------------------------------#
yr_min = 2001
yr_max = 2014
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# TIDY FRAME                                                              ----
#-----------------------------------------------------------------------------#
# ----------------------------------- #
# Extract countries based on Weidmann's Cshapes
# ----------------------------------- #
d <- data.frame()

for(y in seq(yr_min, yr_max, 1)){
  cat(sprintf("Working on year: %s\n", y))

  tmp <- cshapes::cshp(date = as.Date(sprintf("%s-12-31", y)),
                       useGW = FALSE)@data %>%
    rename(ccode = COWCODE) %>%
    mutate(year  = y) %>%
    select(ccode, year)

  d <- bind_rows(d, tmp)
};rm(y, tmp)
# ----------------------------------- #


# ----------------------------------- #
# Tidy data frame
# ----------------------------------- #
d <- d %>%
  filter(!ccode %in% c(2, 710)) %>%
  arrange(ccode, year) %>%
  mutate(cname  = countrycode::countrycode(ccode, "cown", "country.name"),
         region = countrycode::countrycode(ccode, "cown", "region"))

rownames(d) <- 1:nrow(d)
# ----------------------------------- #
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# GHDx - Bilateral Health Aid Data                                        ----
#-----------------------------------------------------------------------------#
ghdx <- ghdx %>%
  filter(source %in% c("China", "United_States"),
         year   %in% c(yr_min:yr_max),
         !channel %in% c("INTLNGO","NGO"),
         elim_ch != 1,
         dah_19  != "-") %>%
  mutate(dah_19 = as.numeric(dah_19) * 1e3) %>%
  gen_cc(., "recipient_country") %>%
  group_by(source, ccode, year) %>%
  summarize(cname  = recipient_country[1],
            dah_19 = sum(dah_19, na.rm = TRUE),
            .groups = "keep") %>%
  ungroup() %>%
  mutate(source = case_when(source == "China" ~ "health.ghdx_China",
                            TRUE ~ "health.ghdx_US")) %>%
  pivot_wider(.,
              id_cols     = c(source,ccode, year),
              names_from  = source,
              values_from = dah_19) %>%
  drop_na(ccode)%>%
  group_by(year) %>%
  mutate(health_pht.ghdx_China = 100 * (health.ghdx_China / sum(health.ghdx_China, na.rm = T)),
         health_pht.ghdx_US    = 100 * (health.ghdx_US / sum(health.ghdx_US, na.rm = T))) %>%
  ungroup()
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# TIDY - CHINA AIDDATA                                                    ----
#-----------------------------------------------------------------------------#
# ----------------------------------- #
# Variable subset & filter research cases and projects where funds were transferred
# ----------------------------------- #
china_full <- china_full %>%
  filter(year    %in% c(yr_min:yr_max),
         !status %in% c("Cancelled", "Suspended"),
         recommended_for_research == TRUE) %>%
  select(project_id, year, funding_agency,
         recipient_condensed,
         title,
         description,
         # status, flow, flow_class,
         # amount,
         usd_current,
         # usd_defl_2014,
         crs_sector_name,
         flow,
         # line_of_credit,
         # year_uncertain,
         # loan_type, interest_rate, maturity,
         # location_details,
         sources, sources_count)
# ----------------------------------- #


# ----------------------------------- #
# Calculate constant currency, drop missing
# ----------------------------------- #
china_full <- china_full %>%
  gen_cc(., "recipient_condensed") %>%
  tidyr::drop_na(ccode) %>%
  left_join(.,defl[,c("year","China_Defl_2015b")], by = "year") %>%
  mutate(aid.total = usd_current / (China_Defl_2015b / 100)) %>%
  select(-China_Defl_2015b, -usd_current) %>%
  filter(crs_sector_name %in% c("Health",
                                "Population Policies / Programmes and Reproductive Health",
                                "Water Supply and Sanitation",
                                "Women in Development"))

# Aid by flow (totals and percentages)
china_full %>%
  group_by(flow) %>%
  summarize(total_aid = sum(aid.total, na.rm = T) / 1e6,
            proj      = n()) %>%
  ungroup() %>%
  mutate(prop_aid = round(100 * (total_aid / sum(total_aid)), 2),
         prop_projects = round(100 * (proj / sum(proj)), 2))

# round(prop.table(table(china_full$flow)) * 100, 2)
# ----------------------------------- #


# ----------------------------------- #
# Regional Aid totals - will merge onto
# ----------------------------------- #
      # aid_totals <- expand.grid(year   = yr_min:yr_max,
      #                           region = unique(china_full$region))
      #
      # aid_totals <- china_full %>%
      #   group_by(year, region) %>%
      #   summarize(aid.total_China = sum(aid.total),
      #             .groups         = "keep") %>%
      #   ungroup() %>%
      #   left_join(aid_totals, ., by = c("year","region")) %>%
      #   as_tibble()
# ----------------------------------- #


# ----------------------------------- #
# Subset for health, aggregate, and Reorganize
# ----------------------------------- #

# prop.table(table(is.na(tst$usd_current))) * 100
# About 57% of the health coded cases are missing
# currency values in AidData. These largely correspond
# to the deployment of Medical Teams, but also cases
# of provided medical training, donation of supplies,
# and in some cases construction of medical infrastructure.

china <- china_full %>%
  group_by(year, ccode) %>%
  mutate(Missing_health = sum(is.na(aid.total))) %>%
  drop_na(aid.total) %>%
  summarize(health.aiddata_China         = sum(aid.total),
            health_missing.aiddata_China = Missing_health[1],
            .groups = "keep") %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(health_pht.aiddata_China = 100 * (health.aiddata_China / sum(health.aiddata_China, na.rm = T))) %>%
  ungroup()


        # aid_totals <- china %>%
        #   group_by(year, region) %>%
        #   summarize(aid.health_China = sum(aid.health_China),
        #             .groups = "keep") %>%
        #   ungroup() %>%
        #   left_join(aid_totals, ., by = c("year","region")) %>%
        #   mutate(aid.health.pT_China = 100 * (aid.health_China / aid.total_China))
# ----------------------------------- #


# ----------------------------------- #
# Factiva Sources - Re: Nisha email w/ Mike Tierney and Brooke Russell on 20210211
# ----------------------------------- #
      # sources <- china_full %>%
      #   filter(crs_sector_name %in% c("Health",
      #                                 "Population Policies / Programmes and Reproductive Health",
      #                                 "Water Supply and Sanitation",
      #                                 "Women in Development")) %>%
      #   select(project_id, year, title, sources, sources_count) %>%
      #   separate(sources,
      #            sep  = ";",
      #            into = sprintf("Source_%s",1:max(.$sources_count)),
      #            fill = "right") %>%
      #   select(-sources_count) %>%
      #   pivot_longer(cols      = starts_with("Source_"),
      #                names_to  = "Source_Number",
      #                values_to = "Link") %>%
      #   mutate(URL = str_split(Link, pattern = ", ", n = 2)) %>%
      #   unnest_wider(URL) %>%
      #   rename(URL      = "...1",
      #          URL_type = "...2") %>%
      #   tidyr::drop_na(URL)
      #
      # sources <- sources %>% select(-Link)
      #
      # sources_factiva <- sources %>%
      #   filter(str_detect(URL, "factiva"))
      #
      # write_csv(sources_factiva, path = "Data/Sources-China-Factiva.csv")
      # write_csv(sources,         path = "Data/Sources-China-All.csv")
      # rm(sources, sources_factiva)
# ----------------------------------- #
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
 # TIDY - US AIDDATA                                                      ----
#-----------------------------------------------------------------------------#
# ----------------------------------- #
# Variable subset, filter years to match china data 2000-2014, & filter only "Disbursements"
# ----------------------------------- #
us_full <- us_full %>%
  mutate(year = suppressWarnings({as.numeric(fiscal_year)})) %>%
  filter(year %in% c(yr_min:yr_max),
         # transaction_type_name == "Disbursements",
         funding_account_name  != "Foreign Military Financing Program") %>%
  select(country_name,
         funding_agency_name, funding_agency_acronym,
         # implementing_agency_name, implementing_agency_acronym, subagency_name,
         dac_category_name, dac_sector_name, dac_purpose_name,
         year,
         funding_account_name,
         activity_name, activity_description,
         activity_project_number,
         current_amount)
# ----------------------------------- #


# ----------------------------------- #
# Calculate constant currency, drop missing
# ----------------------------------- #
# US has so many obs. this will be faster to gen. ccode.

tmp = data.frame("country_name" = unique(us_full$country_name)) %>%
  filter(!str_detect(country_name, pattern = "Region|Territory")) %>%
  mutate(ccode = suppressWarnings({countrycode::countrycode(country_name, "country.name", "cown")})) %>%
  mutate(ccode = case_when(country_name == "Korea, Democratic Republic" ~ 731,
                           country_name == "Serbia and Montenegro (former)" ~ 345,
                           country_name == "Serbia" ~ 345,
                           TRUE ~ ccode)) %>%
  tidyr::drop_na(ccode)

us_full <- us_full %>%
  left_join(., tmp, by = "country_name") %>%
  tidyr::drop_na(ccode) %>%
  left_join(.,defl[,c("year","US_Defl_2015b")], by = "year") %>%
  mutate(aid.total = current_amount / (US_Defl_2015b / 100)) %>%
  select(-US_Defl_2015b, -current_amount)

rm(tmp)
# ----------------------------------- #


# ----------------------------------- #
# Aid totals - merge onto
# ----------------------------------- #
      # aid_totals <- us_full %>%
      #   group_by(year,region) %>%
      #   summarize(aid.total_US = sum(aid.total),
      #             .groups  = "keep") %>%
      #   left_join(aid_totals, ., by = c("year", "region"))
# ----------------------------------- #

# ----------------------------------- #
# Subset for health, aggregate, and Reorganize
# ----------------------------------- #
us <- us_full %>%
  filter(dac_category_name == "Health and Population") %>%
  filter(aid.total > 0) %>%                                # Filtering accounting correction from past years
  group_by(year, ccode) %>%
  summarize(health.usaid_US = sum(aid.total),
            .groups         = "keep") %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(health_pht.usaid_US = 100 * (health.usaid_US / sum(health.usaid_US))) %>%
  ungroup()

      # aid_totals <- us %>%
      #   group_by(year, region) %>%
      #   summarize(aid.health_US = sum(aid.health_US),
      #             .groups       = "keep") %>%
      #   ungroup() %>%
      #   left_join(aid_totals, ., by = c("year","region")) %>%
      #   mutate(aid.health.pT_US = 100 * (aid.health_US / aid.total_US))
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# Examining US military health aid projects for draft 2021-03-29
#-----------------------------------------------------------------------------#
mil <- us_full %>%
  filter(dac_category_name == "Health and Population",
         funding_agency_acronym %in% c("ARMY","DOD","NAVY"))

mil2 <- mil %>%
  select(activity_name, aid.total) %>%
  mutate(g = case_when(str_detect(activity_name, "Global Emerging Infections") ~ "Disease",
                                    str_detect(activity_name, "HIV") ~ "HIV",
                                    str_detect(activity_name, "Water|water") ~ "Water",
                                    TRUE ~ "Other"))
mil3 <- mil2 %>% group_by(g) %>%
  summarize(aid = sum(aid.total) / 1e6) %>%
  ungroup() %>%
  mutate(prop.aid = round(100 * (aid / sum(aid)),2))

round(prop.table(table(mil2$g)) * 100, 2)

#-----------------------------------------------------------------------------#

#-----------------------------------------------------------------------------#
# TIDY - AID TOTALS DATA                                                  ----
#-----------------------------------------------------------------------------#
        # aid_totals <- aid_totals %>%
        #   pivot_longer(cols     = starts_with("aid."),
        #                names_to = c("Var","Donor"),
        #                names_sep = "_") %>%
        #   pivot_wider(.,
        #               names_from  = Var,
        #               values_from = value)
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# COVARIATES                                                              ----
#-----------------------------------------------------------------------------#
# ----------------------------------- #
# GDP (Source: UN Statistics Division)
# ----------------------------------- #
gdp <- read_csv("c:/users/logan/googledrive/umn/research/data/un data/un gdp/GDP-2015Constant.csv") %>%
  filter(Item == "Gross Domestic Product (GDP)") %>%
  rename(country = `Country or Area`,
         year    = `Year`,
         gdp     = Value) %>%
  gen_cc(., "country") %>%
  drop_na(ccode) %>%
  mutate(gdp_log = log(gdp)) %>%
  select(ccode, year, gdp, gdp_log)
# ----------------------------------- #


# ----------------------------------- #
# Population Data (Source: UN Statistics Division)
# ----------------------------------- #
pop <- read_csv("c:/users/logan/googledrive/umn/research/data/un data/un pop/UN Population.csv") %>%
  filter(Variant == "Medium") %>%
  rename(country = `Country or Area`,
         year    = `Year(s)`,
         pop     = Value) %>%
  gen_cc(., "country") %>%
  drop_na(ccode) %>%
  mutate(pop_log = log(pop)) %>%
  select(ccode, year, pop, pop_log)
# ----------------------------------- #


# ----------------------------------- #
# Disease data (Source: Global Health Data Exchange)
# ----------------------------------- #
dis <- read_csv("Data/Covariates/GHDx-Disease/IHME-GBD_2019_DATA-0c238851-1.csv") %>%
  filter(cause_name %in% c("All causes",
                           "Communicable, maternal, neonatal, and nutritional diseases",
                           "Non-communicable diseases")) %>%
  pivot_wider(.,
              id_cols     = c(location_name, year),
              names_from  = c(measure_name, cause_name, metric_name),
              names_sep   = "_",
              values_from = val) %>%
  select(-c(`Deaths_All causes_Percent`, `Incidence_All causes_Percent`,
            `DALYs (Disability-Adjusted Life Years)_All causes_Percent`,
            `Prevalence_All causes_Percent`)) %>%
  mutate(ccode = suppressWarnings({countrycode::countrycode(location_name, "country.name", "cown")})) %>%
  drop_na(ccode) %>%
  gen_cc(., "location_name") %>%
  select(ccode, year, everything(), -location_name) %>%
  group_by(ccode, year) %>%
  arrange(ccode, year) %>%
  summarize(across(everything(), mean),
            .groups = "keep") %>%
  ungroup() %>%
  rename_with(., function(x){str_replace_all(x, "_Non-communicable diseases_", "_NCD_")}, everything()) %>%
  rename_with(., function(x){str_replace_all(x, "_All causes_",                "_AllCause_")}, everything()) %>%
  rename_with(., function(x){str_replace_all(x, "_Communicable, maternal, neonatal, and nutritional diseases_",
                                             "_CD_")}, everything()) %>%
  rename_with(., function(x){str_replace_all(x, "s [(]Disability-Adjusted Life Years[)]", "")}, everything())
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
  mutate(vdem_index = mean(c_across(v2x_polyarchy:v2x_egaldem))) %>%
  select(ccode, year, starts_with("v2x"), vdem_index) %>%
  rename_with(.data = ., .fn = function(x){str_replace(x,"v2x","vdem")}, .cols = starts_with("v2x"))
# ----------------------------------- #
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# FINAL MERGE & CLEAN-UP                                                  ----
#-----------------------------------------------------------------------------#
# ----------------------------------- #
# Merge
# ----------------------------------- #
hd <- d %>%
  left_join(., us,    by = c("ccode","year")) %>%
  left_join(., china, by = c("ccode","year")) %>%
  left_join(., ghdx,  by = c("ccode", "year")) %>%
  left_join(., dis,   by = c("ccode", "year")) %>%
  left_join(., gdp,   by = c("ccode", "year")) %>%
  left_join(., pop,   by = c("ccode", "year")) %>%
  left_join(., vdem,  by = c("ccode", "year")) %>%
  mutate(ccode       = as_factor(ccode),
         year        = as_factor(year),
         gdp_pc      = gdp / (pop * 1e3),
         gdp_pc_log  = log(gdp / (pop * 1e3)),
         vdem_index2 = vdem_index^2,
         ccode       = as_factor(ccode),
         year        = as_factor(year)) %>%
  mutate(gdp_pc2     = gdp_pc^2,
         gdp_pc2_log = log(gdp_pc^2)) %>%
  select(cname, ccode, year, region,
         starts_with(c("health","pop","gdp","vdem","DALY","Incidence","Prevalence","Death"))) %>%
  filter(year %in% yr_min:yr_max)
# ----------------------------------- #


# ----------------------------------- #
# Clean-up
# ----------------------------------- #
rm(d ,defl, yr_min, yr_max, ghdx, gen_cc,
   us, china, gdp, pop, dis, vdem)
# ----------------------------------- #
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# SAVE                                                                    ----
#-----------------------------------------------------------------------------#
# save(us_full, china_full, file = "data/us-china_new.rdata")
# rm(us_full, china_full,)
#
# save.image(file = "data/data-hd-tidy-new.rdata")
# rm(list = ls())
#-----------------------------------------------------------------------------#


