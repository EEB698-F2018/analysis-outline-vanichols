#########################
##
## Date of creation: Sept 19 2018
## Date last modified: Nov 28 2018 (new 99% cis!)
##
## Author: Gina
## Purpose: Make forest plots using emmeans' 99% confidence intervals
##          
## Inputs: 
##         ../_data/tidy/tdat_weedmetadata.csv (for n vals)
##         ../_data/tidy/td_lmer-mod-stats
##
## Outputs: _figs/actual_figs/WMfig_forest
##
## NOTE: Using emmeans' 99% CIs
##
#########################


# Initialize --------------------------------------------------------------

rm(list = ls())
library(tidyverse)
#library(ggridges)
library(ggstance)

path <- rstudioapi::getActiveDocumentContext()$path
Encoding(path) <- "UTF-8"
setwd(dirname(path))


# Get all the data together -----------------------------------------------


xcis <- read_csv("../_data/tidy/td_lmer-mods-stats.csv")

wow <- read_csv("../_data/tidy/tdat_weedmetadata.csv") %>%
  rename(bio = bioLRR,
         den = denLRR,
         sb = sbLRR) %>%
  gather(bio:sb, key = desc, value = LRR) %>%
  filter(!is.na(LRR))


# Figure 2 ----------------------------------------------------------------

# 4 groups: tillage, herb, peren, fallow, weed msmt


# Extract info for labels (n-vals) ----------------------------------------

#--label # of points in each group
(lab_t <- wow %>%
  filter(sys_tillage != "IC") %>%
  group_by(desc, sys_tillage) %>%
  summarise(ntot = n()) %>%
   rename(resp = desc,
          lvls = sys_tillage) %>%
   mutate(mod_nice = "Tillage",
          mod_code = "sys_tillage"))

(lab_h3 <- wow %>%
    filter(sys_weedmgmt2 != "IC") %>%
    group_by(desc, sys_weedmgmt2) %>%
    summarise(ntot = n()) %>%
    rename(resp = desc,
           lvls = sys_weedmgmt2) %>%
    mutate(mod_nice = "Weed Control",
           mod_code = "sys_weedmgmt2"))

(lab_p <- wow %>%
    filter(peren_divYN != "IC") %>%
    group_by(desc, peren_divYN) %>%
    summarise(ntot = n()) %>%
    rename(resp = desc,
           lvls = peren_divYN) %>%
    mutate(lvls = recode(lvls, N = "peren_N"),
           lvls = recode(lvls, Y = "peren_Y"),
           mod_nice = "Perennial Inclusion",
           mod_code = "peren_divYN"))

(lab_f <- wow %>%
    filter(fallow_simpYN != "IC") %>%
    group_by(desc, fallow_simpYN) %>%
    summarise(ntot = n()) %>%
    rename(resp = desc,
           lvls = fallow_simpYN) %>%
    mutate(lvls = recode(lvls, N = "fallow_N"),
           lvls = recode(lvls, Y = "fallow_Y"),
           mod_nice = "Simple System Fallow",
           mod_code = "fallow_simpYN"))

(lab_wm <- wow %>%
    filter(weedmsmt_unit != "IC") %>%
    group_by(desc, weedmsmt_unit) %>%
    summarise(ntot = n()) %>%
    rename(resp = desc,
           lvls = weedmsmt_unit) %>%
    mutate(lvls = recode(lvls, `single species` = "single"),
           mod_nice = "Weed Unit",
           mod_code = "weedmsmt_unit"))

(lab_cl2 <- wow %>%
    filter(!is.na(climclasslat2)) %>%
    group_by(desc, climclasslat2) %>%
    summarise(ntot = n()) %>%
    rename(resp = desc,
           lvls = climclasslat2) %>%
    mutate(mod_nice = "Latitude",
           mod_code = "climclasslat2"))


# Combine all labs
(lab_mods <- bind_rows(lab_t, lab_h3, lab_p, lab_f, lab_wm, lab_cl2))


# Make dataframe to use for fig -------------------------------------------

(fig_mods <- lab_mods %>%
    left_join(xcis) %>%
    
    # Change to % control
    mutate(per = exp(est)*100 - 100,
           plow = exp(ci_99low)*100 - 100,
           pup = exp(ci_99up)*100 - 100) %>%
   
    mutate(lvls = recode(lvls, NT = "Zero-Tillage",
                         tilled = "Tilled",
                         herb_Y = "Herbicides",
                         herb_N = "Other",
                         #none = "None",
                         peren_N = "No",
                         peren_Y = "Yes",
                         fallow_N = "No",
                         fallow_Y = "Yes",
                         community = "Community",
                         single = "Single",
                         temperate = "Temperate",
                         `sub/tropical` = "Tropical"),
           mod_nice = recode(mod_nice,
                             `Simple System Fallow` = "Simple\nSystem\nFallow",
                             `Perennial Inclusion` = "Perennial\nInclusion",
                             `Weed Control` = "Weed\nControl"),
           mod_nice = factor(mod_nice, levels = c("Weed Unit",
                                         "Tillage",
                                         "Latitude",
                                         "Simple\nSystem\nFallow",
                                         "Perennial\nInclusion",
                                         "Weed\nControl"))) %>%
   mutate(sigcolor = ifelse( (ci_99up > 0 & ci_99low < 0), "NS", "sig")) 
  )



# Remove seed bank from fig -----------------------------------------------

fig_mods2 <- fig_mods %>% filter(resp != "sb") %>%
  mutate(resp_nice = resp,
         resp_nice = recode(resp_nice, bio = "Weed Biomass"),
         resp_nice = recode(resp_nice, den = "Weed Density"),
         resp_nice = factor(resp_nice, levels = c("Weed Density", "Weed Biomass")))





# Make figure -------------------------------------------------------------


ggplot(fig_mods2, aes(per, lvls)) + 
  geom_linerangeh(aes(xmin = plow, xmax = pup, color = sigcolor), size = 6) +
  geom_point(size = 2, color = "black") + 
  
  guides(color = F) +
  
  geom_text(aes(x = -150, y = lvls, label = paste("n", ntot, sep = "=")), 
            fontface = "italic", color = "black", hjust = 0, size = 3) + 
  
  
  geom_text(aes(x = per, y = lvls, label = paste(round(per,0), "%")), 
            fontface = "italic", color = "black", hjust = -0.2, size = 3) + 
  
  #geom_text(aes(x = 275, y = 1, label = mod_pval), 
  #          fontface = "italic", color = "black", hjust = 1) +
  
  geom_vline(xintercept = 0, linetype = "dashed") + 
  

  theme_classic() + 
  theme(panel.border = element_rect(color = "black", fill = NA)) +
  
  scale_color_manual(values = c("gray80", "pink")) + 

  labs(y = NULL, x = paste("Percent Change In Weeds With Crop Diversification [%]\nUpdated", Sys.Date(), "99%CI") ) + 
  facet_grid(mod_nice ~ resp_nice, scales = "free") #+
  #coord_cartesian(xlim = c(-100, 175)) 
  
  
ggsave("../_figs/actual_figs/WMfig_forest-2cats.png") 
ggsave("../../../DW_CropRotWeeds_Meta/MANUSCRIPT SECTIONS/figs_for_pub/Fig3_forest-2cats.png") 






