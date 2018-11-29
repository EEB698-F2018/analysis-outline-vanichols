#########################
##
## Date of creation: May 28 2018
## Date last modified: Aug 27 2018
##                     Nov 17 2018 - include conf intervals in LOO ests
##
## Author: Gina
## Purpose: Do sensitivity analyses on results - leave one study out
##          
## Inputs: 
##
## Outputs: ../_data_prcsd/datp_LOO_all.csv 
##
## NOTE: 
##
#########################


##### Clear environment and load packages #####
rm(list=ls())
library(tidyverse)
library(lubridate)
library(lme4)

##### Set working directory to wherever this file is kept #####
path <- rstudioapi::getActiveDocumentContext()$path
Encoding(path) <- "UTF-8"
setwd(dirname(path))

#~~~~~~~~~~~~~~~~~
# Get LOO funcitons I wrote -----------------------------------
#~~~~~~~~~~~~~~~~~
source("../_lib/WMcode_functions-LOO-analysis.R")

#~~~~~~~~~~~~~~~~~
# Read in data ------------------------------------------------------------
#~~~~~~~~~~~~~~~~~

# Subset by response, change to yi notation

wow <- read_csv("../_data/tidy/tdat_weedmetadata.csv")

den <- wow %>% filter(!is.na(denLRR)) %>%
  select(-bioLRR, -sbLRR) %>%
  mutate(yi = denLRR)

bio <- wow %>% filter(!is.na(bioLRR)) %>%
  select(-denLRR, -sbLRR) %>%
  mutate(yi = bioLRR)

# Check number of studies in each level
bio %>% group_by(study_no, peren_divYN) %>%
  summarise(hm = mean(study_no))

sb <- wow %>% filter(!is.na(sbLRR)) %>%
  select(-bioLRR, -denLRR) %>%
  mutate(yi = sbLRR)

#~~~~~~~~~~~~~~~~~
# LOO sensitivity on responses ------------------------------------------------------------
#~~~~~~~~~~~~~~~~~

loo_den <- LosoRespFun(mydat = den, myresp = "den")
loo_bio <- LosoRespFun(mydat = bio, myresp = "bio")

loo_resp <- bind_rows(loo_den, loo_bio)


write_csv(loo_resp, "../_data/tidy/LOO_res/td-resp-LOO.csv")


#~~~~~~~~~~~~~~~~~
# LOO sensitivity on density modifiers ------------------------------------------------------------
#~~~~~~~~~~~~~~~~~

# 6 modifiers
# weedmsmt_unit, sys_tillage, climclasslat2, fallow_simpYN, peren_divYN, sys_weedmgmt
dothese <- c("weedmsmt_unit", "sys_tillage", "climclasslat2", "fallow_simpYN", "peren_divYN", "sys_weedmgmt3")

den_wu <- LosoModFun(den, dothese[1], "den")
den_till <- LosoModFun(den, dothese[2], "den")
den_clim <- LosoModFun(den, dothese[3], "den")
den_fallow <- LosoModFun(den, dothese[4], "den")
den_peren <- LosoModFun(den, dothese[5], "den")
den_wm <- LosoModFun(den, dothese[6], "den")

loo_den_mod <- bind_rows(den_wu, den_till, den_clim, den_fallow, 
                        den_peren, den_wm) 

#~~~~~~~~~~~~~~~~~
# LOO sensitivity on biomass modifiers ------------------------------------------------------------
#~~~~~~~~~~~~~~~~~

bio_wu <- LosoModFun(bio, dothese[1], "bio")
bio_till <- LosoModFun(bio, dothese[2], "bio")
bio_clim <- LosoModFun(bio, dothese[3], "bio")

# NOTE: Biomass meausured in simple system fallow only occured in study_no 5
# so if we leave that out there are not 2 levels to test here
#bio_fallow <- LosoModFun(bio, dothese[4])

# NOTE: Biomass meausured in peren_divYN == Y
# occured in 2 studies (6, 63)
bio_peren <- LosoModFun(bio, dothese[5], "bio")
bio_wm <- LosoModFun(bio, dothese[6], "bio")

loo_bio_mod <- bind_rows(bio_wu, bio_till, bio_clim, #bio_fallow, 
                        bio_peren, bio_wm)

#~~~~~~~~~~~~~~~~~
# Write LOO-modifier results ------------------------------------------------------------
#~~~~~~~~~~~~~~~~~

loo_mod <- bind_rows(loo_den_mod, loo_bio_mod)

write_csv(loo_mod, "../_data/tidy/LOO_res/td-mod-LOO.csv")

