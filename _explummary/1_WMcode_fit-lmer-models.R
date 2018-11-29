#########################
#
# Date of creation: Sept 30 2018
# Date last modified: Nov 13 2018 (running w/RAW10)
#                     Nov 15 2018 (running w/updated functions-run-lmer, using lsmeans for CIs)
#                     Nov 27 2018 (fixing lmer-mods-stats)
#                     Nov 29 (made sys_weedmgmt 2 categories)
#
# Author: Gina
# Purpose: Fit linear models to all response variables and all modifiers
#          
#          
# Inputs: ../_data/tidy/tdat_weedmetadata.csv
#
# Outputs: ../_data/tidy/td_lmer-mods-stats (this one I messed up but remade)
#                        td_lmer-resp-stats (bio and den only)
#
# NOTE: Using lme4 and lsmeans contrasts
#       Need confidence intervals on den and bio overall estimates
#
#########################


##### Clear environment and load packages #####
rm(list = ls())
library(tidyverse)
library(lme4)
library(lmerTest)
library(broom)

##### Set working directory to wherever this file is kept #####
path <- rstudioapi::getActiveDocumentContext()$path
Encoding(path) <- "UTF-8"
setwd(dirname(path))

# NOTE: This one's modifer functino is fucked up
source("../_lib/WMcode_functions-run-lmer.R")

##===================================##
#
# Read in data, make subsets as needed, change to lmer input style
#
##===================================##

wow <- read_csv("../_data/tidy/tdat_weedmetadata.csv")

den <- wow %>% filter(!is.na(denLRR)) %>%
  select(-bioLRR, -sbLRR) %>%
  mutate(yi = denLRR)

bio <- wow %>% filter(!is.na(bioLRR)) %>%
  select(-denLRR, -sbLRR) %>%
  mutate(yi = bioLRR)


##===================================##
#
# Run on each response individually
#
##===================================##

# NOTE: Right now there are no confidence intervals
# I don't know how to get them from lsmeans. Pvals should be ok. 

denres <- RunModelNoModsFun(den, "den")
biores <- RunModelNoModsFun(bio, "bio")

# NEED CONFIDENCE INTERVALS
respres <- bind_rows(denres, biores)

write_csv(respres, "../_data/tidy/td_lmer-resp-stats.csv")


##===================================##
#
# Get CIs for each modifier, used in figures, NEEDS ATTENTION
#
##===================================##

# modifiers
dothese <- c("weedmsmt_unit", "sys_tillage", "climclasslat2", 
             "fallow_simpYN", "peren_divYN", "sys_weedmgmt3", "sys_weedmgmt2")

# Density
#~~~~~~~~~~~~
den_wu <- RunModelModsFun(den, dothese[1], "den")
den_till <- RunModelModsFun(den, dothese[2], "den")
den_clim <- RunModelModsFun(den, dothese[3], "den")
den_fall <- RunModelModsFun(den, dothese[4], "den")
den_peren <- RunModelModsFun(den, dothese[5], "den")
#den_wm <- RunModelModsFun(den, dothese[6], "den")
den_wm <- RunModelModsFun(den, dothese[7], "den")

den_mods <- bind_rows(den_wu, den_till, den_clim, den_fall, den_peren, den_wm) %>%
  select(resp, mod_code, desc, everything()) 

# Biomass
#~~~~~~~~~~~~
bio_wu <- RunModelModsFun(bio, dothese[1], "bio")
bio_till <- RunModelModsFun(bio, dothese[2], "bio")
bio_clim <- RunModelModsFun(bio, dothese[3], "bio")
bio_fall <- RunModelModsFun(bio, dothese[4], "bio")
bio_peren <- RunModelModsFun(bio, dothese[5], "bio")
#bio_wm <- RunModelModsFun(bio, dothese[6], "bio")
bio_wm <- RunModelModsFun(bio, dothese[7], "bio")


bio_mods <- bind_rows(bio_wu, bio_till, bio_clim, bio_fall, bio_peren, bio_wm) %>%
  select(resp, mod_code, desc, everything()) 

# Put together and put in format for forest plot merging
res_mods <- bind_rows(den_mods, bio_mods) %>%
  mutate(mod_nice = recode(mod_code,
                           climclasslat2 = "Latitude",
                           fallow_simpYN = "Simple System Fallow",
                           sys_weedmgmt2 = "Weed Control",
                           peren_divYN = "Perennial Inclusion",
                           sys_tillage = "Tillage",
                           weedmsmt_unit = "Weed Unit")) %>%
  mutate(lvls = desc,
         
         # Make it match forest plot stuff, no idea why I did this
         lvls = ifelse(mod_code == "fallow_simpYN" & desc == "N", "fallow_N", lvls),
         lvls = ifelse(mod_code == "fallow_simpYN" & desc == "Y", "fallow_Y", lvls),
         
         lvls = ifelse(mod_code == "peren_divYN" & desc == "N", "peren_N", lvls),
         lvls = ifelse(mod_code == "peren_divYN" & desc == "Y", "peren_Y", lvls),
         
         lvls = recode(lvls, `single species` = "single")) %>%
  
  select(resp, mod_code, mod_nice, lvls, desc, everything())

write_csv(res_mods, "../_data/tidy/td_lmer-mods-stats.csv")

