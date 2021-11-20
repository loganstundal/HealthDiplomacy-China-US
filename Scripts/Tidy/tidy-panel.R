#-----------------------------------------------------------------------------#
#
# Author:        Logan Stundal
# Date:          November 15, 2021
# Purpose:       Create panel for Military health diplomacy paper
#
#
# Copyright (c): Logan Stundal, 2021
# Email:         stund005@umn.edu
#
#-----------------------------------------------------------------------------#
#
# Notes:
#      Military AidData ranges between the following years:
#        - project commitment: 2000-2017
#        - project implement : 2000-2019
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
library(cshapes)
#---------------------------#
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# CREATE PANEL                                                            ----
#-----------------------------------------------------------------------------#
min_yr <- 1999
max_yr <- 2019

tidy_map <- function(yr){
  x <- cshp(date  = as.Date(sprintf("%s-06-01", yr)),
            useGW = FALSE) %>%
    rename(cname = country_name,
           ccode = cowcode) %>%
    mutate(year  = yr) %>%
    select(cname, ccode, year, capname, caplong, caplat)
  x <- rmapshaper::ms_simplify(input = x, keep = 0.05, keep_shapes = TRUE)
  x <- st_transform(x, crs = st_crs("+proj=robin"))
  return(x)
}

pnl <- lapply(min_yr:max_yr, tidy_map) %>% bind_rows
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# SAVE                                                                    ----
#-----------------------------------------------------------------------------#
save(pnl, file = "Data/tidy-data/tidy-pnl.Rdata")
rm(list = ls())
#-----------------------------------------------------------------------------#
