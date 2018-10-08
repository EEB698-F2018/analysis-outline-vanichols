#############################
#
# Last modified: Oct 8 2018
# Purpose: Clean/process yield data from Matt
# 
#
# INPUTS: _data/peeps/dat_ML-corn-yields-2003-17.csv
#       
#
# OUPUTS: 
#         
#
##############################

rm(list=ls())
library(tidyverse)

path <- rstudioapi::getActiveDocumentContext()$path
Encoding(path) <- "UTF-8"
setwd(dirname(path))

cyld <- read_csv("../_data/peeps/dat_ML-corn-yields-2003-17.csv", skip=4) %>%
  #change yields to dry matter basis
  mutate(dm_Mgha = Mg_ha * (1-0.155)) %>%
  select(-bu_ac, -Mg_ha)

write_csv(cyld, "../_data/tidy/td_corn-ylds.csv")
