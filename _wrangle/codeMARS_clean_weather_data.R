#############################
#
# Last modified: Oct 8 2018
# Purpose: 
#   Look at Marsden Farm weather (from https://mesonet.agron.iastate.edu/request/coop/fe.phtml)
#   Clean up
#
# INPUTS: _data/peeps/dat_ML_wea_1987-2017.csv
#
# OUPUTS: _data/tidy/td_wea.csv
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


wea <- read_csv("../_data/peeps/dat_ML_wea_1987-2017.csv", skip=0) %>%
  # make columns I want
  mutate(year = year(day),
         site = "Marsden") %>%
  rename(highT_oC = highc,
         lowT_oC = lowc,
         precip_mm = precipmm,
         rad_mj = narr_srad) %>%
  select(site, year, doy, highT_oC, lowT_oC, precip_mm, rad_mj) %>%
  mutate(avgT_oC =(lowT_oC + highT_oC)/2)

write_csv(wea, "../_data/tidy/td_wea.csv")
