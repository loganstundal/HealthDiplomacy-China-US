#-----------------------------------------------------------------------------#
#
# Author:        Logan Stundal
# Date:          September 07, 2021
# Purpose:       APSA - 2021; GIF for iPoster
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
library(cshapes)
library(biscale)
library(gganimate)
library(cowplot)
library(rmapshaper)
#---------------------------#

#---------------------------#
# Set working directory
#---------------------------#
# setwd("C:/Users/logan/GoogleDrive/UMN/CLASSES/2021 SPRING/RA - Nisha/Health-Diplomacy/")
setwd("C:/Users/logan/GoogleDrive/UMN/RESEARCH/PAPERS/HealthDiplomacy-China-US/")
#---------------------------#

#---------------------------#
# Load data
#---------------------------#
load("data/data-hd-tidy-new.rdata")
#---------------------------#

#---------------------------#
# Locals
#---------------------------#
map_theme <- theme(panel.background = element_rect(fill = "transparent", color = "black",
                                                   size = 0.2),
                   panel.grid       = element_blank(),
                   axis.text        = element_blank(),
                   axis.ticks       = element_blank(),
                   legend.title     = element_blank(),
                   legend.position  = "bottom",
                   legend.direction = "horizontal",
                   strip.background = element_rect(fill = "gray90", color = "black",
                                                   size = 0.2))
#---------------------------#
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# TIDY DATA                                                               ----
#-----------------------------------------------------------------------------#
# ----------------------------------- #
# Health data
# ----------------------------------- #
hd2 <- hd %>%
  mutate(ccode = as.numeric(as.character(ccode)),
         year  = as.numeric(as.character(year))) %>%
  mutate(group = case_when(year %in% c(2001:2007) ~ "2001-2007",
                           TRUE                   ~ "2008-2014")) %>%
  group_by(cname, year) %>%
  summarize(ccode            = ccode[1],
            group            = group[1],
            aid.health_US    = sum(health.ghdx_US,      na.rm = TRUE),
            aid.health_China = sum(health.ghdx_China, na.rm = TRUE),
            .groups          = "keep") %>%
  ungroup() %>%
  group_by(group) %>%
  mutate(aid.health.perHT_China = 100 * (aid.health_China / sum(aid.health_China, na.rm = TRUE)),
         aid.health.perHT_US    = 100 * (aid.health_US    / sum(aid.health_US,    na.rm = TRUE))) %>%
  ungroup() %>%
  pivot_longer(.,
               cols       = starts_with("aid"),
               names_sep  = "_",
               names_to   = c("Variable", "Donor"),
               values_to  = c("Value")) %>%
  pivot_wider(.,
              id_cols     = c(ccode, year, Variable),
              values_from = Value,
              names_from  = Donor) %>%
  filter(Variable == "aid.health.perHT") %>%
  select(ccode, year, US, China)


yrs <- sort(unique(hd2$year))
res <- lapply(yrs, function(x){
  tmp <- hd2 %>% filter(year == x) %>%
    rename(CH = China)

  med_us <- median(tmp$US, na.rm = TRUE)
  med_ch <- median(tmp$CH, na.rm = TRUE)

  tmp <- tmp %>%
    mutate(US2 = case_when(US == 0               ~ "1",
                           US > 0 & US <= med_us ~ "2",
                           US > med_us           ~ "3"),
           CH2 = case_when(CH == 0               ~ "1",
                           CH > 0 & CH <= med_ch ~ "2",
                           CH > med_ch           ~ "3")) %>%
    mutate(across(.cols = c(US2, CH2), .fns = ~factor(.x, levels = c("1", "2", "3")))) %>%
    mutate(bi_class     = paste(US2, CH2, sep = "-"))

  mp  <- cshp(date = as.Date(sprintf("%s-06-01", x)), useGW = FALSE) %>%
    rename(ccode = cowcode) %>% select(ccode) %>%
    st_transform(., "+proj=robin") %>%
    ms_simplify(., keep = 0.05, keep_shapes = TRUE)
  tmp <- left_join(mp, tmp, by = "ccode") %>%
    st_set_geometry(., "geometry") %>%
    mutate(year = as.character(x))
})

res <- bind_rows(res)

rm(yrs)

# ggplot(data = tmp) + geom_sf(aes(fill = US2)) +
#   scale_fill_viridis_d(option = "magma")
#
# ggplot(data = tmp) + geom_sf(aes(fill = CH2)) +
#   scale_fill_viridis_d(option = "magma")

pal <- bi_pal_manual(val_1_3 = "#cc0024", val_2_3 = "#8a274a", val_3_3 = "#4b264d",
                     val_1_2 = "#dd7c8a", val_2_2 = "#8d6c8f", val_3_2 = "#4a4779",
                     val_1_1 = "#dddddd", val_2_1 = "#7bb3d1", val_3_1 = "#016eae")

leg <- bi_legend(pal  = pal,
                 dim  = 3,
                 xlab = "US Aid",
                 ylab = "China Aid",
                 size = 16)
rm(pal)

usch <- cshp(date = as.Date("2010-06-01"), useGW = FALSE) %>%
  rename(ccode = cowcode) %>% select(ccode) %>%
  st_transform(., "+proj=robin") %>%
  filter(ccode %in% c(2, 710)) %>%
  ms_simplify(., keep = 0.05, keep_shapes = TRUE)

mp <- ggplot(data = res) +
  geom_sf(aes(fill  = bi_class, group = interaction(bi_class, year)),
          color = "gray70", size = 0.1, show.legend = FALSE) +
  geom_sf(data = usch, fill = "gray20", color = "gray70", size = 0.1) +
  bi_scale_fill(pal = pal, dim = 3) +
  # facet_wrap(~time_id) +
  map_theme +
  # theme(plot.title = element_text(size = (16/.pt))) +
  # theme(plot.margin=unit(c(0, 0.1, 0, 0),"in"))
  labs(title = "Year: {frame_time}",
       caption = "Data source: IHME - Development Assistance for Health Database")

anim <- mp + transition_time(as.integer(year))
anim <- animate(anim, duration = 30, end_pause = 5,
                width  = 9.0,
                height = 4.5,
                units  = "in",
                res    = 250)

mp_g <- image_read()

anim_save("dah-map.gif", animation = anim)

ggsave(plot     = leg,
       filename = "dah-legend.png",
       width    = 3.0,
       height   = 3.0,
       units    = "in",
       dpi      = 350)


# ----------------------------------- #

mp07 <- cshp(date = as.Date("2006-06-01"), useGW = FALSE) %>%
  rename(ccode = cowcode) %>%
  select(ccode) %>%
  st_transform(., "+proj=robin")

mp08 <- cshp(date = as.Date("2012-06-01"), useGW = FALSE) %>%
  rename(ccode = cowcode) %>%
  select(ccode) %>%
  st_transform(., "+proj=robin")

par(mfrow = c(1,2))
plot(mp07$geometry, main = "pre08");plot(mp08$geometry, main = "post08")

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
