#-----------------------------------------------------------------------------#
#
# Author:        Logan Stundal
# Date:          September 18, 2021
# Purpose:       Qualitative Data
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
library(tidyverse)
library(sf)
library(kableExtra)
#---------------------------#

#---------------------------#
# Load data
#---------------------------#
load("Data/HealthDiplomacy.Rdata")
#---------------------------#
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# TIDY DATA                                                               ----
#-----------------------------------------------------------------------------#
d   <- d %>%
  filter(!cname %in% c("United States", "China"),
         !year  %in% c(as.character(2000:2002))) %>%
  arrange(year, ccode)
#-----------------------------------------------------------------------------#


#-----------------------------------------------------------------------------#
# MILITARY ACTIVITY                                                       ----
#-----------------------------------------------------------------------------#
# sort(unique(d$cname)) %>% noquote

us_mil <- c("Azerbaijan", "Bangladesh", "Botswana", "Equatorial Guinea",
            "Ethiopia", "Malaysia", "Mozambique", "Rwanda", "United Arab Emirates",
            "Zambia")
cn_mil <- c("Belarus", "Bolivia", "Botswana", "Congo - Brazzaville",
            "Congo - Kinshasa", "Ghana", "Guinea", "Liberia", "Mali",
            "Mongolia", "Nepal", "Nigeria", "Papua New Guinea", "Senegal",
            "Togo", "Zambia")

# all(us_mil %in% d$cname)
# all(cn_mil %in% d$cname)

# China top 10 based on reported public aid - to make more comparable with us:
# d %>%
#   st_drop_geometry() %>%
#   select(cname, dah_usd_chn) %>%
#   filter(cname %in% cn_mil) %>%
#   group_by(cname) %>%
#   summarize(res = sum(dah_usd_chn)) %>%
#   slice(1:10) %>%
#   arrange(cname) %>%
#   pull(cname) %>%
#   paste(., collapse = '", "') %>%
#   noquote

cn_mil <- c("Belarus", "Bolivia", "Botswana", "Congo - Brazzaville",
            "Congo - Kinshasa", "Ghana", "Guinea", "Liberia", "Mali", "Mongolia")

# Overlapping case: Botswana
# us_mil[us_mil %in% cn_mil]

# ----------------------------------- #
# Table of cases
# ----------------------------------- #
kbl(x      = data.frame("United States" = us_mil, "China" = cn_mil),
    format = "pipe") %>%
  save_kable(.,
             file = "Results/Tables/military-cases.txt")
# ----------------------------------- #
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# MILITARY SF TIDY                                                        ----
#-----------------------------------------------------------------------------#
# ----------------------------------- #
# SF data
# ----------------------------------- #
mil <- cshapes::cshp(date = as.Date("2010-06-01"), useGW = FALSE) %>%
  mutate(cname = countrycode::countrycode(cowcode, "cown", "country.name")) %>%
  select(cname) %>%
  mutate(val = case_when(cname %in% us_mil & cname %in% cn_mil ~ "Both",
                         cname %in% us_mil ~ "US military",
                         cname %in% cn_mil ~ "CN military",
                         TRUE ~ NA_character_)) %>%
  mutate(val = factor(val, levels = c("US military", "CN military", "Both"))) %>%
  rmapshaper::ms_simplify(., keep = 0.05, keep_shapes = TRUE) %>%
  st_transform(., crs = "+proj=robin")
# ----------------------------------- #


# ----------------------------------- #
# Factors to explore military health presence
# ----------------------------------- #
dvs <- c("life_expec", "inf_death", "daly_ncd_rate", "daly_cd_rate",
         "imports_ln", "exports_ln", "gdp_ln", "vdem")
health <- d %>%
  st_drop_geometry() %>%
  select(cname, !!dvs) %>%
  group_by(cname) %>%
  summarize(across(.fns  = mean)) %>%
  ungroup

mil <- left_join(mil, health, by = "cname") %>%
  mutate(us_mil = case_when(cname %in% us_mil ~ 1, TRUE ~ 0),
         cn_mil = case_when(cname %in% cn_mil ~ 1, TRUE ~ 0))

rm(health)
# ----------------------------------- #
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# MILITARY PATTERNS                                                       ----
#-----------------------------------------------------------------------------#
# ----------------------------------- #
# T-Tests
# ----------------------------------- #
health_usmil <- lapply(dvs, function(dv){t.test(mil[[dv]] ~ mil$us_mil)})
names(health_usmil) <- dvs

health_cnmil <- lapply(dvs, function(dv){t.test(mil[[dv]] ~ mil$cn_mil)})
names(health_cnmil) <- dvs

res <- list("US" = health_usmil, "CN" = health_cnmil)
rm(health_usmil, health_cnmil)
# ----------------------------------- #


# ----------------------------------- #
# Print and Review
# ----------------------------------- #
tidy_t <- function(x){
  cat(sprintf("t = %s; p-value = %s\n", round(x$statistic, 3), round(x$p.value,3)))
  cat(sprintf("CI: [%s, %s]\n", round(x$conf.int[1],3), round(x$conf.int[2], 3)))
  cat(sprintf("Mean group 0: %s\nMean group 1: %s\n\n", round(x$estimate[1],3), round(x$estimate[2],3)))
  cat(paste(paste(rep("-",40), collapse = "") %>% noquote, "\n"))
}

outcome <- function(dv){
  print(dv)
  x <- res %>% map(dv)
  for(i in 1:2){cat(tidy_t(x[[i]]))}
  cat(paste(paste(rep("+",80), collapse = "") %>% noquote, "\n"))
}

invisible(lapply(dvs, function(dv){outcome(dv)}))

rm(tidy_t, outcome, dvs)
# ----------------------------------- #
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# MILITARY DIPLOMACY MAP                                                  ----
#-----------------------------------------------------------------------------#
plt_mil <- ggplot(data = mil) +
  geom_sf(aes(fill = val), color = "gray70", size = 0.1) +
  scale_fill_viridis_d(na.translate = F) +
  theme(
    panel.background = element_rect(fill = NA, size = 0.1, color = "black"),
    panel.grid       = element_blank(),
    axis.text        = element_blank(),
    axis.ticks       = element_blank(),
    legend.title     = element_blank(),
    legend.position  = "bottom",
    legend.direction = "horizontal",
    legend.key.size    = unit(2, "mm"),
    legend.margin      = margin(-3.5, 0, 0, 1, "mm"),
    legend.text        = element_text(size = 12/.pt),
    plot.title         = element_text(size = 16/.pt),
    plot.subtitle      = element_text(size = 14/.pt),
    plot.caption       = element_text(size = 12/.pt)
  ) +
  labs(title = "US & China - Military health aid sample")

ggsave(plot     = plt_mil,
       filename = "Results/Figures/APSA-2021_military-sample.png",
       width    = 3.5,
       height   = 2.0,
       units    = "in",
       dpi      = 350)
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# SAVE                                                                    ----
#-----------------------------------------------------------------------------#
#save.image()
#rm(list = ls())
#-----------------------------------------------------------------------------#
