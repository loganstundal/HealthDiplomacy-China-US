#-----------------------------------------------------------------------------#
#
# Author:        Logan Stundal
# Date:          November 19, 2021
# Purpose:       Visualization - military health diplomacy
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
#---------------------------#

#---------------------------#
# Load data
#---------------------------#
load("Data/health-diplomacy_20211118.Rdata")
#---------------------------#
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# TIDY DATA                                                               ----
#-----------------------------------------------------------------------------#
d <- d %>%
  mutate(mil = case_when(
    health_usa_usaidm_bin == "Military" & health_chn_aidatm_binp == "Military" ~ "Both",
    health_usa_usaidm_bin == "Military" & health_chn_aidatm_binp == "No military" ~ "USA",
    health_usa_usaidm_bin == "No military" & health_chn_aidatm_binp == "Military" ~ "CHN",
    health_usa_usaidm_bin == "No military" & health_chn_aidatm_binp == "No military" ~ "Neither")) %>%
  select(ccode, cname, year, mil) %>%
  mutate(mil = factor(mil,
                      levels = c("Neither","USA", "CHN", "Both"))) %>%
  filter(year %in% 2004:2015)
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# PLOT                                                                    ----
#-----------------------------------------------------------------------------#
uscn <- d %>% filter(ccode %in% c(2, 710))
d    <- d %>% filter(!ccode %in% c(2, 710))

cols <- c("Neither"    = "#E8E8E8",
          "Both"     = "#2A5A5B",
          "USA"     = "#6C83B5",
          "CHN" = "#73AE80")

# biscale::bi_pal(pal = "DkCyan", dim = 2, preview = F)
# biscale::bi_pal(pal = "DkCyan", dim = 2)

plt <- ggplot() +
  geom_sf(data = d,    aes(fill = mil), size = 0.1) +
  geom_sf(data = uscn, fill = "gray40", size = 0.1) +

  theme(
    panel.grid       = element_blank(),
    panel.background = element_rect(fill = NA, color = "black", size = 0.1),
    axis.text        = element_blank(),
    axis.ticks       = element_blank(),
    legend.position  = "bottom",
    legend.direction = "horizontal",
    legend.title     = element_blank(),
    strip.background = element_rect(fill = "gray90", color = "black", size = 0.1),
    # strip.text       = element_text(size = (12/.pt)),
    strip.text.x     = element_text(margin = margin(0.1,0,0.1,0, "cm"))
  ) +
  guides(fill = guide_legend(nrow = 2)) +
  scale_fill_manual(values = cols) +
  facet_wrap(~year, nrow = 4, ncol = 3) +
  labs(title = "Military Health Diplomacy: 2004 - 2015")

# plt

ggsave(plot   = plt,
       file   = "Results/Figures/map-military_hd.png",
       width  = 9,
       height = 6.5,
       units  = "in",
       dpi    = 350)
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# SAVE                                                                    ----
#-----------------------------------------------------------------------------#
#save.image()
#rm(list = ls())
#-----------------------------------------------------------------------------#
