#############################
#
# Last modified: Oct 10 2018
# Purpose: Try to categorize weather data
#
# INPUTS: ../_data/tidy/td_corn-ylds.csv
#         ../_data/tidy/td_wea.csv
#
# OUPUTS: 
#         
#
##############################

rm(list = ls())
library(tidyverse)

path <- rstudioapi::getActiveDocumentContext()$path
Encoding(path) <- "UTF-8"
setwd(dirname(path))


# read in data for reference ----------------------------------------------

crn <- read_csv("../_data/tidy/td_corn-ylds.csv")
wea <- read_csv("../_data/tidy/td_wea.csv")


# Find long-term means ----------------------------------------------------

# Hmm. 
wyr <- wea %>%
  group_by(site, year) %>%
  summarise(mT_C = mean(avgT_oC, na.rm = T),
            totP_mm = sum(precip_mm, na.rm = T),
            mP_mm = mean(totP_mm, na.rm = T)) %>%
  group_by(site) %>%
  mutate(LT_T = mean(mT_C),
         LT_P = mean(mP_mm))

wyr %>%
  ggplot(aes(mT_C, mP_mm)) + 
  geom_point() + 
  geom_hline(yintercept = wyr$LT_P[1]) + 
  geom_vline(xintercept = wyr$LT_T[1]) + 
  geom_label(aes(label = year))

ggsave("../_figs/wea-yrs.png")

# Try scaling w/in pipe
wyr2 <- wea %>%
  group_by(site, year) %>%
  summarise(mT_C = mean(avgT_oC, na.rm = T),
            totP_mm = sum(precip_mm, na.rm = T),
            mP_mm = mean(totP_mm, na.rm = T)) %>%
  mutate(scT_C = scale(mT_C),
         scP_mm = scale(mP_mm))          

wyr2 %>%
  ggplot(aes(scT_C, scP_mm)) + 
  geom_point() +
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) +
  geom_label(aes(label = year))

ggsave("../_figs/wea-yrs-scaled.png")
