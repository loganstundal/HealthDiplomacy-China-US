
library(plm)

data("Grunfeld", package = "plm")
y <- data.frame(split(Grunfeld$inv, Grunfeld$firm)) # individuals in columns

purtest(y, pmax = 4, exo = "intercept", test = "madwu")

## same via pseries interface
pGrunfeld <- pdata.frame(Grunfeld, index = c("firm", "year"))
purtest(pGrunfeld$inv, pmax = 4, exo = "intercept", test = "madwu")

## same via formula interface
purtest(inv ~ 1, data = Grunfeld, index = c("firm", "year"), pmax = 4, test = "madwu")




z2 <- pdata.frame(z2, index = c("ccode", "year"))

z2 <- z %>% select(ccode, year, usa) %>% filter(year %in% c(2010:2015)) %>%
  group_by(ccode) %>%
  mutate(test = all(usa == 0)) %>%
  ungroup %>%
  filter(!test) %>%
  select(-test)
y <- data.frame(split(z2$usa, z2$ccode))
purtest(y, pmax = 4, exo = "intercept", test = "madwu")


z2 <- z %>%
  select(ccode, year, chn) %>%
  group_by(ccode) %>%
  mutate(test = all(chn == 0)) %>%
  ungroup %>%
  filter(!test)

purtest(chn ~ 1, data = z2, index = c("ccode", "year"), pmax = 4, test = "madwu")




z <- z %>% mutate(fuckyou = paste(ccode, year, sep = "_"))
