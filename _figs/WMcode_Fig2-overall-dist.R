#########################
##
## Date of creation: Oct 13 2018
## Date last modified: Nov 28 2018 (use 99% CIs from emmeans)
##
## Author: Gina
## Purpose: Make forest plots using contest (from emmeans) 99% confidence intervals
##          
## Inputs: 
##         tdat_weedmetadata.csv (for n vals)
##         tdat_tdat_lmer-resp-stats
##
## Outputs: WMfig_forest-ggridges
##          WMfig_forest-ggridges-pchange
##
## NOTE: 
##
#########################

# Initialize --------------------------------------------------------------

rm(list = ls())
library(tidyverse)
library(ggridges)
library(ggstance)

path <- rstudioapi::getActiveDocumentContext()$path
Encoding(path) <- "UTF-8"
setwd(dirname(path))


# Data --------------------------------------------------------------------

# Means, CIs, pvals
(cis <- read_csv("../_data/tidy/td_lmer-resp-stats.csv") %>%
   
  # Change to % control
  mutate(pavg = exp(est)*100-100,
         plow = exp(ci_99low)*100-100,
         pup = exp(ci_99up)*100-100) %>%
   
  mutate(resp_nice = mod,
         resp_nice = recode(resp_nice, bio = "Weed Biomass"),
         resp_nice = recode(resp_nice, den = "Weed Density"),
         resp_nice = recode(resp_nice, sb = "Weed Seed Bank")) %>%
  # Don't want seed bank any more
  filter(mod != "sb") %>%
  select(-desc))

# Distribution of values
(wow <- read_csv("../_data/tidy/tdat_weedmetadata.csv") %>%
  rename(bio = bioLRR,
         den = denLRR,
         sb = sbLRR) %>%
  gather(bio:sb, key = desc, value = LRR) %>%
  filter(!is.na(LRR)) %>%
  select(desc, LRR) %>%
  # Make nice labels
  mutate(resp_nice = desc,
         resp_nice = recode(resp_nice, bio = "Weed Biomass"),
         resp_nice = recode(resp_nice, den = "Weed Density"),
         resp_nice = recode(resp_nice, sb = "Weed Seed Bank")) %>%
  # Change to %s
  mutate(pLRR = exp(LRR)*100 - 100) %>%
    
  # Don't want seed bank any more
  filter(desc != "sb") %>%
  rename(mod = desc)) 


# Figure 2 ----------------------------------------------------------------
# Biomass and density ggridges plots?


# Get n for labels and add to cis
(stats_resp <- wow %>%
  group_by(mod) %>%
  summarise(ntot = n()) %>%
  left_join(cis) %>%
   mutate(resp_nice = factor(resp_nice, levels = c("Weed Density", "Weed Biomass"))))


ggplot(data = stats_resp, aes(pavg, resp_nice)) +
  geom_density_ridges(data = wow, aes(pLRR, resp_nice), fill = "red2", 
                      alpha = 0.3, scale = 0.9) + 
  geom_linerangeh(aes(xmin = plow, xmax = pup, y = resp_nice), color = "black", size = 3) +
  geom_point(size = 1, color = "white")  + 
  
  geom_text(x = 300, aes(y = resp_nice, label = paste("Mean = ", round(pavg, 1), "%", sep = "")), 
            fontface = "italic", vjust = -4, hjust = 0, size = 10)  +
  
  geom_text(x = 300, aes(y = resp_nice, label = paste("n", ntot, sep = " = ")), 
            fontface = "italic", vjust = -3, hjust = 0, size = 8)  +

  geom_text(x = 300, aes(y = resp_nice, label = paste("updated ", Sys.Date())), 
            fontface = "italic", vjust = -1.5, hjust = 0, size = 8)  +
  
  geom_vline(xintercept = 0, linetype = "dashed")  +
  labs(y = NULL, x = "Percent of Simple System Weeds [%]") + 
  scale_y_discrete(expand = c(0.1, 0.1)) +
  theme_classic() +
  theme(text = element_text(size = 20)) + 
  guides(fill = F, color = F) + 
  scale_color_manual(values = c("purple", "orange"))


ggsave("../_figs/actual_figs/WMfig_overall-dist.png") 
ggsave("../../../DW_CropRotWeeds_Meta/MANUSCRIPT SECTIONS/figs_for_pub/Fig2_overall-dist.png") 


