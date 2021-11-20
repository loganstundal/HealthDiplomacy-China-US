#-----------------------------------------------------------------------------#
#
# Author:        Logan Stundal
# Date:          February 11, 2021
# Purpose:       Initial exploratory plotting for HD data.
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
# ADMINISTRATIVE --------------------------------------------------------------

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
setwd("C:/Users/logan/GoogleDrive/UMN/CLASSES/2021 SPRING/RA - Nisha/Health-Diplomacy")

#---------------------------#
# Load data
#---------------------------#
load("data/data-hd-tidy.rdata")

# Custom ggplot save function with univeral options
plt_save <- function(plot, filename){
  ggsave(plot     = plot,
         filename = filename,
         dpi      = 400,
         width    = 6.5,
         height   = 6)
}

#-----------------------------------------------------------------------------#
# ADMINISTRATIVE                                                          ----
#-----------------------------------------------------------------------------#
ts_theme <-
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(linetype = "dashed", color = "gray70"),
        panel.grid.minor.y = element_line(linetype = "dotted", color = "gray80"))

caption_text <- sprintf("Sources:\nChina - %s\nUnited States - %s",
                        "AidData Global Chinese Official Finance Dataset, Version 1.0",
                        "Foreign Aid Explorer: Complete U.S. foreign aid dataset, 10/29/2020")
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# Descriptive Statistics
#-----------------------------------------------------------------------------#
hd <- hd %>%
  mutate(year = as.numeric(as.character(year)),
         ccode = as.numeric(as.character(ccode)))

range(hd$year)

aid_totals %>%
  group_by(Donor,region) %>%
  summarize(Aid_Total  = sum(aid.total, na.rm = T)  / 1e6,
            Aid_Health = sum(aid.health, na.rm = T) / 1e6,
            Aid_Health_per = mean(aid.health.pT, na.rm = T)) %>%
  ungroup()



# NEED TO FIX THIS LATER
d <- hd %>%
  mutate(aid.health_US = case_when(aid.health_US < 0 ~ 0, TRUE ~ aid.health_US),
         aid.health.perHT_US = case_when(aid.health.perHT_US < 0 ~ 0, TRUE ~ aid.health.perHT_US)) %>%
  mutate(across(everything(), ~replace_na(.x, 0)))

m1 <- lm(log(aid.health_US + 1) ~ log(aid.health_China + 1), data = d)
summary(m1)

m2 <- lm(log(aid.health_China + 1) ~ log(aid.health_US + 1), data = d)
summary(m2)

#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# TIME_SERIES                                                             ----
#-----------------------------------------------------------------------------#
tmp <- aid_totals %>%
  group_by(year, Donor) %>%
  summarize(aid.total  = sum(aid.total,  na.rm = T),
            aid.health = sum(aid.health, na.rm = T)) %>%
  ungroup() %>%
  mutate(aid.health.pT = 100 * (aid.health / aid.total))


# ----------------------------------- #
# GLOBAL - Absolute dollars
# ----------------------------------- #
ts_global_abs <- ggplot(tmp,
       aes(x = year)) +
  geom_line(aes(y = aid.health, color = Donor)) +
  geom_hline(aes(yintercept = 0), color = "black", linetype = "solid") +
  scale_y_continuous(labels = function(x){x/1e9},
                     breaks = seq(0, 12, 2)*1e9,
                     limits = c(0,12)*1e9) +
  scale_x_continuous(breaks = c(2001, 2005, 2010, 2014)) +
  labs(title    = "Health Aid - Global",
       subtitle = "US Dollars, 2015 constant",
       y        = "USD Billions",
       caption  = caption_text) +
  guides(color = guide_legend(nrow = 1)) +
  ts_theme

plt_save(ts_global_abs, "plots/ts_global_abs.png")

# ----------------------------------- #
# GLOBAL - Percent annual aid budget
# ----------------------------------- #
ts_global_rel <- ggplot(tmp,
       aes(x = year)) +
  geom_line(aes(y = aid.health.pT, color = Donor)) +
  geom_hline(aes(yintercept = 0), color = "black", linetype = "solid") +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1),
                     limits = c(0, 25)) +
  scale_x_continuous(breaks = c(2001, 2005, 2010, 2014)) +
  labs(title    = "Health Aid - Global",
       subtitle = "Percentage of total annual aid budget",
       caption  = caption_text) +
  guides(color = guide_legend(nrow = 1)) +
  ts_theme + theme(axis.title = element_blank())

plt_save(ts_global_rel, "plots/ts_global_rel.png")

# ----------------------------------- #
# REGIONAL - Absolute dollars
# ----------------------------------- #
ts_regional_abs <- ggplot(aid_totals %>%
         filter(region %in% c("Latin America & Caribbean", "Sub-Saharan Africa")) %>%
         mutate(aid.health = replace_na(aid.health, 0)),
       aes(x = year, color = region)) +
  geom_line(aes(y = aid.health)) +
  scale_y_continuous(labels = function(x){x/1e9},
                     limits = c(0,10)*1e9,
                     breaks = seq(0,10,2)*1e9) +
  scale_x_continuous(breaks = c(2001, 2005, 2010, 2014)) +
  facet_wrap(~ Donor) +
  labs(title    = "Health Aid - Regional",
       subtitle = "US Dollars, 2015 constant",
       y        = "USD Billions",
       caption  = caption_text) +
  guides(color = guide_legend(nrow = 1)) +
  ts_theme +
  theme(axis.title.x     = element_blank(),
        strip.background = element_rect(fill = "gray80"))

plt_save(ts_regional_abs, "plots/ts_regional_abs.png")

# ----------------------------------- #
# REGIONAL - Percent annual aid budget
# ----------------------------------- #
ts_regional_rel <- ggplot(aid_totals %>%
         filter(region %in% c("Latin America & Caribbean", "Sub-Saharan Africa")) %>%
         mutate(aid.health.pT = replace_na(aid.health.pT, 0)),
       aes(x = year, color = region)) +
  geom_line(aes(y = aid.health.pT)) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1)) +
  scale_x_continuous(breaks = c(2001, 2005, 2010, 2014)) +
  facet_wrap(~ Donor) +
  labs(title    = "Health Aid - Regional",
       subtitle = "Percentage of total annual aid budget",
       caption  = caption_text) +
  guides(color = guide_legend(nrow = 1)) +
  ts_theme +
  theme(axis.title = element_blank(),
        strip.background = element_rect(fill = "gray80"))

plt_save(ts_regional_rel, "plots/ts_regional_rel.png")

# ----------------------------------- #
# Relative regional health aid -
# given a pre-selected amount of health aid, where does each state decide to deploy its resources?
# ----------------------------------- #
tmp <- aid_totals %>%
  group_by(year, Donor) %>%
  mutate(aid.health.regional.pHT = 100 * (aid.health / sum(aid.health, na.rm = TRUE)))

ts_regional_rel_health <- ggplot(tmp %>%
         filter(region %in% c("Latin America & Caribbean", "Sub-Saharan Africa")) %>%
         mutate(aid.health.regional.pHT = replace_na(aid.health.regional.pHT, 0)),
       aes(x = year, color = region)) +
  geom_line(aes(y = aid.health.regional.pHT)) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1)) +
  scale_x_continuous(breaks = c(2001, 2005, 2010, 2014)) +
  facet_wrap(~ Donor) +
  labs(title    = "Health Aid - Regional",
       subtitle = "Relative health aid contribution",
       caption  = caption_text) +
  guides(color = guide_legend(nrow = 1)) +
  ts_theme +
  theme(axis.title = element_blank(),
        strip.background = element_rect(fill = "gray80"))

plt_save(ts_regional_rel_health, "plots/ts_regional_rel_health.png")


# ----------------------------------- #
# Clean-up
# ----------------------------------- #
rm(list = setdiff(ls(),c("aid_totals","hd","m","caption_text","plt_save")))
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# MAPS                                                                    ----
#-----------------------------------------------------------------------------#
# ----------------------------------- #
# Map - setup
# ----------------------------------- #
# Create centroids object for mapping
cents <- st_centroid(m)


# create group averages for two time blocs: 2001-2007, 2008-2014
yr_grp_tots <- aid_totals %>%
  mutate(group = case_when(year %in% c(2001:2007) ~ "2001-2007",
                           TRUE ~ "2008-2014")) %>%
  group_by(group, Donor) %>%
  summarize(aid.total = sum(aid.total, na.rm = TRUE))

tmp <- hd %>%
  mutate(group = case_when(year %in% c(2001:2007) ~ "2001-2007",
                           TRUE ~ "2008-2014")) %>%
  group_by(country, group) %>%
  summarize(ccode            = ccode[1],
            aid.health_US    = sum(aid.health_US,    na.rm = TRUE),
            aid.health_China = sum(aid.health_China, na.rm = TRUE),
            .groups = "keep") %>%
  ungroup() %>%
  left_join(., yr_grp_tots, by = "group") %>%
  pivot_wider(.,
              values_from = aid.total,
              names_from  = Donor) %>%
  group_by(group) %>%
  mutate(aid.health.perHT_China = 100 * (aid.health_China / sum(aid.health_China, na.rm = TRUE)),
         aid.health.perHT_US    = 100 * (aid.health_US    / sum(aid.health_US,    na.rm = TRUE)),

         aid.health.perT_China  = 100 * (aid.health_China / sum(China, na.rm = TRUE)),
         aid.health.perT_US     = 100 * (aid.health_US    / sum(US,    na.rm = TRUE))) %>%
  ungroup() %>%
  select(-China, -US) %>%
  left_join(., cents, by = "ccode") %>%
  st_set_geometry(., "geometry")

# lapply(names(tmp)[str_detect(names(tmp), "aid")], function(x){summary(tmp[x])})

tmp <- tmp %>%
  pivot_longer(cols = starts_with("aid"),
               names_to = c("Var","Donor"),
               names_sep = "_") %>%
  pivot_wider(.,
              names_from  = Var,
              values_from = value) %>%
  st_set_geometry(., "geometry")

# sapply(c("US","China"),
#        {function(x) mapply(function(x,y){tmp %>% filter(Donor == x) %>% pull(y) %>% summary},x,
#                           c("aid.health","aid.health.perHT","aid.health.perT"))},
#        simplify = FALSE, USE.NAMES = TRUE)


# ----------------------------------- #
"
What do I want to map here?

Three variables:

1. Absolute health aid dollars
2. Health aid dollars relative to total aid in period
3. Health aid dollars relative to total health aid in period
"
# ----------------------------------- #


# ----------------------------------- #
# Map - % Health aid
# ----------------------------------- #
map_per_health <- ggplot() +
  geom_sf(data = m, fill = "gray99", size = 0.2) +
  geom_sf(data = tmp,
          aes(color = Donor, size = aid.health.perHT), alpha = 0.5) +
  scale_size_continuous(name   = "% health allocation\n",
                        breaks = c(0, 5, 20, 40, 100),
                        range  = c(0, 7)) +
  facet_wrap(~ group, nrow = 2) +
  labs(title = "Health Aid",
       subtitle = "Share of annual health aid budget allocation",
       caption  = caption_text) +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "gray80")) +
  guides(color = guide_legend(override.aes = list(size = 7),
                              order        = 1))

plt_save(map_per_health, "plots/map_per_health.png")

# ----------------------------------- #
# Map - $ Health aid
# ----------------------------------- #
map_dollars_health <- ggplot() +
  geom_sf(data = m, fill = "gray99", size = 0.2) +
  geom_sf(data = tmp,
          aes(color = Donor, size = aid.health), alpha = 0.5) +
  scale_size_continuous(name   = "$ health allocation\n(millions)",
                        labels = function(x){x/1e6},
                        range  = c(0,7)) +
  facet_wrap(~ group, nrow = 2) +
  labs(title = "Health Aid",
       subtitle = "Amount of annual health aid budget allocation",
       caption  = caption_text) +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "gray80")) +
  guides(color = guide_legend(override.aes = list(size = 7),
                              order        = 1))

plt_save(map_dollars_health, "plots/map_dollars_health.png")
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# BIVARIATE CHOROPLETH MAPS                                               ----
#-----------------------------------------------------------------------------#
# Since I always forget:
# https://cran.r-project.org/web/packages/biscale/vignettes/biscale.html

# Create bi-class
tmp <- tmp %>%
  st_drop_geometry() %>%
  select(country, group, ccode, Donor, aid.health.perHT) %>%
  # # Logging to reduce extreme outliers
  # mutate(aid.health = log(aid.health + 1)) %>%
  pivot_wider(.,
              names_from  = Donor,
              values_from = aid.health.perHT) %>%
  left_join(., m, by = "ccode") %>%
  st_set_geometry(., "geometry")

tmp <- tmp %>%
  mutate(flag_us = case_when(US < 1 ~ 1,
                             US >= 1 & US < 5 ~ 2,
                             US >= 5 ~ 3),
         flag_china = case_when(China < 1 ~ 1,
                             China >= 1 & China < 5 ~ 2,
                             China >= 5 ~ 3)) %>%
  mutate(bi_class = sprintf("%s-%s",flag_us, flag_china))

# table(tmp$bi_class)

mp <- ggplot() +
  geom_sf(data = m, fill = "gray90",color = "gray85", size = 0.1) +
  geom_sf(data = tmp, aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "DkViolet", dim = 3) +
  labs(title    = "Health Aid",
       subtitle = "US-China, relative health aid allocation decisions",
       caption  = caption_text) +
  # bi_theme() +
  facet_wrap(~ group, nrow = 2) +
  theme_void() +
  theme(strip.background = element_rect(fill = "gray80"),
        strip.text.x = element_text(margin = margin(2,0,2,0, "mm")),
        plot.margin  = margin(1,1,4,1, "mm"))

legend <- bi_legend(pal = "DkViolet",
                    dim = 3,
                    xlab = "% US aid",
                    ylab = "% China aid",
                    size = 8)
# bi_pal("DkBlue", preview = FALSE)

map_bivariate_per_health <- ggdraw() +
  draw_plot(mp, 0, 0, 1, 1) +
  draw_plot(legend, 0.15, 0.12, 0.18, 0.18)


# ----------------------------------- #
# Save bivariate map
# ----------------------------------- #
plt_save(map_bivariate_per_health, "plots/map_bivariate_per_health.png")
# ----------------------------------- #
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# SAVE                                                                    ----
#-----------------------------------------------------------------------------#
rm(list=ls())

# save.image()
# rm(list = ls())
#-----------------------------------------------------------------------------#




