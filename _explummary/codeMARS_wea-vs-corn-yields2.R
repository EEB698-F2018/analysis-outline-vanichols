#############################
#
# Last modified: Oct 8 2018
# Purpose: 
#   Look at Marsden Farm weather (from https://mesonet.agron.iastate.edu/request/coop/fe.phtml)
#   Compare to yields
#
# INPUTS: _data/tidy/td_wea.csv
#         _data/tidy/td_corn-ylds.csv
#
# OUPUTS: 
#         
#
##############################


# Clear environ and load libs ---------------------------------------------

rm(list=ls())
library(tidyverse)
library(lubridate)
library(corrplot)
library(PerformanceAnalytics)
library(lme4)
library(ggplot2)
library(rlme)

# Set wd to wherever current file is kep ----------------------------------

path <- rstudioapi::getActiveDocumentContext()$path
Encoding(path) <- "UTF-8"
setwd(dirname(path))


# Read in files -----------------------------------------------------------


wea <- read_csv("../_data/tidy/td_wea.csv") 
cyld <- read_csv("../_data/tidy/td_corn-ylds.csv") 


# Different ways of summarising weather -----------------------------------

wsmy <- wea %>% 
  mutate(SDD = highT_oC - 30,
         SDD = ifelse(SDD<0, 0, SDD)) %>%
  group_by(year) %>%
  summarise(cSDD = sum(SDD, na.rm = T),
            cP_m = sum(precip_mm))

dat <- cyld %>% left_join(wsmy)

write_csv(dat, "../_data/tidy/td_yld-wea-smy.csv")
