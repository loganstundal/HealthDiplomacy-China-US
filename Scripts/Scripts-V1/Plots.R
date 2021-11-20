#-----------------------------------------------------------------------------#
#
# Author:        Logan Stundal
# Date:          March 20, 2021
# Purpose:       Visualization
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
library(sf)
library(biscale)
library(cowplot)

#---------------------------#
# Set working directory
#---------------------------#
setwd("C:/Users/logan/GoogleDrive/UMN/CLASSES/2021 SPRING/RA - Nisha/Health-Diplomacy/")

#---------------------------#
# Load data
#---------------------------#
load("data/data-hd-tidy-new.rdata")

#---------------------------#
# Local functions
#---------------------------#
# Simple plot save function for consistent sizing
plt_save <- function(plot, filename, dim_h = 6.5, dim_w = 6.5){
  ggsave(plot     = plot,
         filename = filename,
         dpi      = 400,
         width    = dim_w,
         height   = dim_h)
}
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# TIDY                                                                    ----
#-----------------------------------------------------------------------------#
hd <- hd %>%
  mutate(ccode = as.numeric(as.character(ccode)),
         year  = as.numeric(as.character(year)))
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# ANALYSIS                                                                ----
#-----------------------------------------------------------------------------#
yr <- hd %>%
  group_by(year) %>%
  summarize(US    = sum(health.usaid_US,      na.rm = TRUE),
            China = sum(health.aiddata_China, na.rm = TRUE))
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# MAPS                                                                    ----
#-----------------------------------------------------------------------------#

# ----------------------------------- #
# SPATIAL DATA
# ----------------------------------- #
mp0107 <- st_as_sf(
  suppressWarnings({cshapes::cshp(date = as.Date("2004-06-01"), useGW = F)})
) %>%
  st_crop(xmin = -180, xmax = 180, ymin = -90, ymax = 90) %>%
  st_transform(., crs = st_crs(sp::CRS("+proj=robin"))) %>%
  rename(ccode = COWCODE) %>%
  select(ccode)

mp0814 <- st_as_sf(
  suppressWarnings({cshapes::cshp(date = as.Date("2013-06-01"), useGW = F)})
) %>%
  st_crop(xmin = -180, xmax = 180, ymin = -90, ymax = 90) %>%
  st_transform(., crs = st_crs(sp::CRS("+proj=robin"))) %>%
  rename(ccode = COWCODE) %>%
  select(ccode)
# ----------------------------------- #


# ----------------------------------- #
# Create temporary data
# ----------------------------------- #
tmp <- hd %>%
  mutate(group = case_when(year %in% c(2001:2007) ~ "2001-2007",
                           TRUE ~ "2008-2014")) %>%
  group_by(cname, group) %>%
  summarize(ccode            = ccode[1],
            aid.health_US    = sum(health.ghdx_US,      na.rm = TRUE),
            aid.health_China = sum(health.ghdx_China, na.rm = TRUE),
            .groups = "keep") %>%
  ungroup() %>%
  group_by(group) %>%
  mutate(aid.health.perHT_China = 100 * (aid.health_China / sum(aid.health_China, na.rm = TRUE)),
         aid.health.perHT_US    = 100 * (aid.health_US    / sum(aid.health_US,    na.rm = TRUE))) %>%
  ungroup() %>%
  pivot_longer(.,
               cols = starts_with("aid"),
               names_sep = "_",
               names_to  = c("Variable", "Donor"),
               values_to = c("Value")) %>%
  pivot_wider(.,
              id_cols     = c(ccode, group, Variable),
              values_from = Value,
              names_from  = Donor)
# ----------------------------------- #


# ----------------------------------- #
# Create map data
# ----------------------------------- #
mp0107 <- mp0107 %>%
  left_join(., tmp %>%
              filter(group    == "2001-2007",
                     Variable == "aid.health.perHT"), by = "ccode") %>%
  st_set_geometry(., "geometry") %>%
  mutate(flag_us = case_when((US >  0 & US < 1) ~ 1,
                             (US >= 1 & US < 5) ~ 2,
                             (US >= 5)          ~ 3),
         flag_china = case_when((China >  0 & China < 1) ~ 1,
                                (China >= 1 & China < 5) ~ 2,
                                (China >= 5)             ~ 3)) %>%
  mutate(bi_class = sprintf("%s-%s",flag_us, flag_china)) %>%
  mutate(US_no_China = as_factor(case_when(bi_class == "1-NA" ~ 1)),
         China_no_US = as_factor(case_when(bi_class == "NA-1" ~ 1)),
         bi_class    = factor(bi_class, levels = expand.grid(1:3,1:3) %>%
                                mutate(z = paste(Var1, Var2, sep = "-")) %>% pull(z))) %>%
  mutate(group = case_when(is.na(group) ~ "2001-2007", TRUE ~ group))

mp0814 <- mp0814 %>%
  left_join(., tmp %>%
              filter(group    == "2008-2014",
                     Variable == "aid.health.perHT"), by = "ccode") %>%
  st_set_geometry(., "geometry") %>%
  mutate(flag_us = case_when((US >  0 & US < 1) ~ 1,
                             (US >= 1 & US < 5) ~ 2,
                             (US >= 5)          ~ 3),
         flag_china = case_when((China >  0 & China < 1) ~ 1,
                                (China >= 1 & China < 5) ~ 2,
                                (China >= 5)             ~ 3)) %>%
  mutate(bi_class = sprintf("%s-%s",flag_us, flag_china)) %>%
  mutate(US_no_China = as_factor(case_when(bi_class == "1-NA" ~ 1)),
         China_no_US = as_factor(case_when(bi_class == "NA-1" ~ 1)),
         bi_class    = factor(bi_class, levels = expand.grid(1:3,1:3) %>%
                                mutate(z = paste(Var1, Var2, sep = "-")) %>% pull(z))) %>%
  mutate(group = case_when(is.na(group) ~ "2008-2014", TRUE ~ group))

# Merged plot data
pltdat <- bind_rows(mp0107, mp0814)
# ----------------------------------- #


# ----------------------------------- #
# Create maps
# ----------------------------------- #
caption_text <- sprintf("Figure based on data from:\n%s",
                        "IHME, Development Assistance for Health Dataset")

border_color  <- "gray60"
# us_only_color <- viridis::plasma(8)[7]
us_only_color <- "cadetblue4"


bi_map <- ggplot() +
  geom_sf(data = pltdat, aes(fill = bi_class), color = border_color, size = 0.1, show.legend = FALSE) +
  geom_sf(data = pltdat %>% filter(US_no_China == 1), fill = us_only_color, color = border_color, size = 0.1) +
  geom_sf(data = pltdat %>% filter(ccode %in% c(2, 710)), fill = "gray85", color = border_color, size = 0.1) +
  facet_wrap(~ group) +
  bi_scale_fill(pal = "DkViolet", dim = 3) +
  labs(title    = "US, Chinese Health Aid",
       caption  = caption_text,
       subtitle = "Percent (%) annual health aid budget allocation") +
  facet_wrap(~ group, nrow = 2) +
  theme_void() +
  theme(strip.text.x = element_text(margin = margin(2,0,2,0, "mm")),
        plot.margin  = margin(1,1,4,1, "mm"),
        strip.background = element_blank(),
        strip.text = element_text(face = "plain", size = 10))

# legend <- bi_legend(pal = "DkViolet",
#                     dim = 3,
#                     xlab = "US aid",
#                     ylab = "China aid",
#                     size = 10)

# Custom legend for extra "US only aid" case.
x <- c("3-3" = "#3F2949",
       "2-3" = "#435786",
       "1-3" = "#4885C1",
       "3-2" = "#77324C",
       "2-2" = "#806A8A",
       "1-2" = "#89A1C8",
       "3-1" = "#AE3A4E",
       "2-1" = "#BC7C8F",
       "1-1" = "#CABED0",
       "3-0" = "white",
       "2-0" = "white",
       "1-0" = us_only_color,
       "0-3" = "white",
       "0-2" = "white",
       "0-1" = "white",
       "0-0" = "white")
x <- dplyr::tibble(bi_class = names(x), bi_fill = x)
x <- tidyr::separate(x, bi_class, into = c("x", "y"),
                sep = "-")

my_legend <- ggplot(data = x) +
  geom_tile(aes(x=as.integer(x), y=as.integer(y), fill = bi_fill)) +
  scale_fill_identity() +
  labs(x = substitute(paste("US","" %->% "")), y = substitute(paste("China", "" %->%""))) +
  geom_segment(aes(x = -0.10, xend = 3.80, y =  0.50, yend = 0.50), size = 0.5) +  # x
  geom_segment(aes(x =  0.50, xend = 0.50, y = -0.70, yend = 3.60), size = 0.5) +  # y
  scale_x_continuous(limits = c(-0.1,4.80), expand = c(0,0)) +
  scale_y_continuous(limits = c(-0.7,4.80), expand = c(0,0)) +
  bi_theme() +
  theme(text = element_text(size = 10, face = "plain")) +
  coord_fixed()

map_bivariate_per_health <- ggdraw() +
  draw_plot(bi_map, 0, 0, 1, 1) +
  draw_plot(my_legend, 0.05, 0.05, 0.22, 0.22)
# ----------------------------------- #


# ----------------------------------- #
# Save bivariate map
# ----------------------------------- #
plt_save(map_bivariate_per_health, sprintf("plots/map_bivariate_per_health_%s.png",
                                           format(Sys.Date(), "%Y%m%d")))
# ----------------------------------- #
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# REGIONAL PLOTS
#-----------------------------------------------------------------------------#

my_cols <- c("#AE3A4E","#4885C1","#3F2949")
# ----------------------------------- #
# Data prep
# ----------------------------------- #
tmp <- hd %>%
  mutate(region = case_when(!region %in% c("Sub-Saharan Africa", "South Asia") ~ "Other",
                            TRUE ~ region)) %>%
  mutate(region = factor(region, levels = c("Sub-Saharan Africa","South Asia","Other"))) %>%
  group_by(region, year) %>%
  summarize(aid.health_US    = sum(health.ghdx_US,    na.rm = TRUE),
          aid.health_China = sum(health.ghdx_China, na.rm = TRUE),
          .groups = "keep") %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(aid.health.perHT_China = 100 * (aid.health_China / sum(aid.health_China, na.rm = TRUE)),
         aid.health.perHT_US    = 100 * (aid.health_US    / sum(aid.health_US,    na.rm = TRUE))) %>%
  ungroup() %>%
  pivot_longer(.,
               cols = starts_with("aid.health"),
               names_to = "Variable",
               values_to = "Value") %>%
  mutate(Var    = str_split(Variable, "_") %>% map(., 1) %>% unlist,
         Source = str_split(Variable, "_") %>% map(., 2) %>% unlist)

# ----------------------------------- #
# Plots
# ----------------------------------- #
regplt <- ggplot(data = tmp %>% filter(Var == "aid.health.perHT"), aes(x = year, y = Value)) +
  geom_line(aes(color = region), size = 0.7) +
  facet_wrap(~Source) +
  scale_color_manual(name   = "",
                     values = my_cols) +
  scale_x_continuous(breaks = c(2001,2005,2010,2014)) +
  theme_minimal() +
  theme(plot.margin  = margin(1,1,4,1, "mm"),
        legend.position = "bottom",
        legend.text = element_text(size = 10),
        axis.title = element_blank(),
        axis.text.x = element_text(angle = 45),
        strip.text.x = element_text(margin = margin(2,0,2,0, "mm")),
        strip.background = element_blank(),
        strip.text = element_text(face = "plain", size = 10),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "gray80", linetype = "dotted", size = 0.2),
        panel.grid.minor.y = element_line(color = "gray80", linetype = "dotted", size = 0.2)) +
  labs(title    = "Health Diplomacy - Regional Contributions, 2001-2014",
       subtitle = "Percent (%) annual health aid budget allocation",
       caption  = caption_text)

# ----------------------------------- #
# Save regional plot
# ----------------------------------- #
plt_save(regplt, sprintf("plots/ts_regional_per_health_%s.png",
                         format(Sys.Date(), "%Y%m%d")),
         dim_h = 5.0)
# ----------------------------------- #


#-----------------------------------------------------------------------------#

#-----------------------------------------------------------------------------#
# SAVE                                                                    ----
#-----------------------------------------------------------------------------#
#save.image()
#rm(list = ls())
