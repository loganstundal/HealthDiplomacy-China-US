#-----------------------------------------------------------------------------#
#
# Author:        Logan Stundal
# Date:          September 17, 2021
# Purpose:       Figures and other plots
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
library()
#---------------------------#

#---------------------------#
# Set working directory
#---------------------------#
setwd()
#---------------------------#

#---------------------------#
# Load data
#---------------------------#

#---------------------------#
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# TIDY DATA                                                               ----
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# ANALYSIS                                                                ----
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# SAVE                                                                    ----
#-----------------------------------------------------------------------------#
#save.image()
#rm(list = ls())
#-----------------------------------------------------------------------------#











#-----------------------------------------------------------------------------#
# QUICK MAP
#-----------------------------------------------------------------------------#

# library(biscale)

mpdat <- d %>%
  as.data.frame %>%
  mutate(yr_grp = case_when(year %in% as.character(2003:2007) ~ "2003-2007",
                            year %in% as.character(2008:2012) ~ "2008-2012",
                            year %in% as.character(2013:2017) ~ "2013-2017")) %>%
  dplyr::select(cname, yr_grp, dah_per_usa, dah_per_chn, geometry) %>%
  group_by(cname, yr_grp) %>%
  summarize(across(.cols = !geometry,
                   .fns  = mean),
            geometry = geometry[3],
            .groups = "keep") %>%
  ungroup

iqr <- mpdat %>% dplyr::select(starts_with("dah"))
iqr <- lapply(iqr, mean)

mpdat <- mpdat %>%
  mutate(dah_chn_grp = case_when(dah_per_chn == 0 ~ "1",
                                 dah_per_chn > 0 & dah_per_chn <= iqr$dah_per_chn ~ "2",
                                 dah_per_chn > iqr$dah_per_chn ~ "3"),
         dah_usa_grp = case_when(dah_per_usa == 0 ~ "1",
                                 dah_per_usa > 0 & dah_per_usa <= iqr$dah_per_usa ~ "2",
                                 dah_per_usa > iqr$dah_per_usa ~ "3")) %>%
  mutate(bi_class = paste(dah_usa_grp, dah_chn_grp, sep = "-")) %>%
  st_set_geometry(., "geometry")

ggplot(data = mpdat) +
  geom_sf(aes(fill = dah_per_chn)) +
  facet_wrap(~yr_grp) +
  scale_fill_gradient2(low  = "red",
                       high = "blue",
                       mid  = "white",
                       midpoint = mean(mpdat$dah_per_chn))

ggplot(data = mpdat) +
  geom_sf(aes(fill = dah_per_usa)) +
  facet_wrap(~yr_grp) +
  scale_fill_gradient2(low  = "red",
                       high = "blue",
                       mid  = "white",
                       midpoint = mean(mpdat$dah_per_usa))

ggplot(data = mpdat) +
  geom_sf(aes(fill = bi_class)) +
  biscale::bi_scale_fill(pal = "GrPink") +
  facet_wrap(~yr_grp, nrow = 3)

#-----------------------------------------------------------------------------#

