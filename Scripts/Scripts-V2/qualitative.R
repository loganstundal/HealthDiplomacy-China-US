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
library(descr)
#---------------------------#

#---------------------------#
# Load data
#---------------------------#
d <- read_csv("data/Military-HD-Codings/Sources-BrookRussell-Tidy.csv")

# load("Data/HealthDiplomacy.Rdata")
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
# Convert sample country names to ccodes
# ----------------------------------- #
us_mil <- countrycode::countrycode(us_mil, "country.name", "cown")
cn_mil <- countrycode::countrycode(cn_mil, "country.name", "cown")
# ----------------------------------- #


# ----------------------------------- #
# SF data
# ----------------------------------- #
mil <- cshapes::cshp(date = as.Date("2016-06-01"), useGW = FALSE) %>%
  # mutate(cname = countrycode::countrycode(cowcode, "cown", "country.name")) %>%
  # dplyr::select(cname) %>%
  rename(ccode = cowcode) %>%
  mutate(val = case_when(ccode %in% us_mil & ccode %in% cn_mil ~ "Both",
                         ccode %in% us_mil ~ "US military",
                         ccode %in% cn_mil ~ "CN military",
                         TRUE ~ "Neither")) %>%
  mutate(val = factor(val, levels = c("US military", "CN military",
                                      "Both", "Neither"))) %>%
  rmapshaper::ms_simplify(., keep = 0.05, keep_shapes = TRUE) %>%
  st_transform(., crs = "+proj=robin")
# ----------------------------------- #


# ----------------------------------- #
# Factors to explore military health presence
# ----------------------------------- #
dvs <- c("dem_life_expec_ln", "dem_inf_death_rate_ln",
         "dis_daly_ncd_rate_ln", "dis_daly_cd_rate_ln",
         "wealth_imports_ln", "wealth_exports_ln", "wealth_gdp_ln",
         "vdem",
         "ally_US.nonagg", "ally_CN.nonagg"
         )

health <- d %>%
  st_drop_geometry() %>%
  dplyr::select(ccode, !!dvs) %>%
  mutate(ccode = as.integer(as.character(ccode))) %>%
  group_by(ccode) %>%
  summarize(across(.cols = all_of(dvs[1:8]),
                   .fns  = mean),
            across(.cols = starts_with("ally_"),
                   .fns  = ~as.numeric(str_detect(max(as.numeric(.x)), "2")))) %>%
  ungroup

mil <- left_join(mil, health, by = "ccode") %>%
  mutate(us_mil = case_when(ccode %in% us_mil ~ 1, TRUE ~ 0),
         cn_mil = case_when(ccode %in% cn_mil ~ 1, TRUE ~ 0)) %>%
  drop_na()

rm(health)
# ----------------------------------- #
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# MILITARY PATTERNS                                                       ----
#-----------------------------------------------------------------------------#

# ----------------------------------- #
# Ally Status
# ----------------------------------- #
# Does having a US non-aggression pact predict US Military health involvement?
# YES - sample of countries with US military health aid less likely to have US non-agg
CrossTable(x = mil$ally_US.nonagg,
           y = mil$us_mil,
           chisq      = TRUE,
           expected   = TRUE,
           row.labels = TRUE)
mil$country_name[(mil$us_mil & !mil$ally_US.nonagg)]

# Does having a CN non-aggression pact predict US Military health involvement?
# NO
CrossTable(x = mil$ally_CN.nonagg,
           y = mil$us_mil,
           chisq      = TRUE,
           expected   = TRUE,
           row.labels = TRUE)

# Does having a CN non-aggression pact predict CN Military health involvement?
# NO
CrossTable(x = mil$ally_CN.nonagg,
           y = mil$cn_mil,
           chisq      = TRUE,
           expected   = TRUE,
           row.labels = TRUE)

# Does having a US non-aggression pact predict CN Military health involvement?
# YES - sample of countries with CN military health aid less likely to have US non-agg
CrossTable(x = mil$ally_US.nonagg,
           y = mil$cn_mil,
           chisq      = TRUE,
           expected   = TRUE,
           row.labels = TRUE)
mil$country_name[(mil$cn_mil & !mil$ally_US.nonagg)]
# ----------------------------------- #

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
  for(i in 1:2){
    cat(ifelse(i == 1, "US\n", "CN\n"))
    cat(tidy_t(x[[i]]))
    }
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
  geom_sf(aes(fill = val), color = "gray50", size = 0.1) +
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
    legend.text        = element_text(size = 20/.pt),
    # plot.title         = element_text(size = 16/.pt),
    # plot.subtitle      = element_text(size = 14/.pt),
    # plot.caption       = element_text(size = 12/.pt)
  )
# +
#   labs(title = "US & China - Military health aid sample")

ggsave(plot     = plt_mil,
       filename = "Results/Figures/APSA-2021_military-sample.png",
       width    = 6.5,
       height   = 3.0,
       units    = "in",
       dpi      = 350)
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# SAVE                                                                    ----
#-----------------------------------------------------------------------------#
#save.image()
#rm(list = ls())
#-----------------------------------------------------------------------------#
