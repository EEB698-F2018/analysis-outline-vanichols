#########################
##
## Date of creation: March 31 2018
## Most recent modification: Nov 28 2018 (hadn't removed seed bank values)
##
## Author: Gina
## Purpose: Create world map of meta-data
##
## Inputs: 
## Outputs:
#########################


# Clear environment and load packages
rm(list = ls())
library(tidyverse)
library(lubridate)
library(readxl) # used to read Excel files
library(maps) # pre-loaded maps package

path <- rstudioapi::getActiveDocumentContext()$path
Encoding(path) <- "UTF-8"
setwd(dirname(path))

paste('Temperature (',~degree,'C)',sep='')

# Data --------------------------------------------------------------------

datraw <- read_csv("../_data/tidy/tdat_weedmetadata.csv") %>%
  gather(bioLRR:sbLRR, key = resp, value = LRR) %>%
  filter(!is.na(LRR)) %>%
  filter(resp != "sbLRR") %>% # get rid of seedbank values
  filter(!is.na(lat)) # Harker....?


myns <- datraw %>%
  group_by(climclasslat2) %>%
  summarise(n = n())

subn <- unname(as.numeric(myns[1,2]))
tmptn <- unname(as.numeric(myns[2,2]))

dat <- datraw %>%
  mutate(climclasslat2 = recode(climclasslat2,
                                `sub/tropical` = (paste('(Sub)Tropical (<35?), n = ', subn)),
                                temperate = (paste('Temperate (>35?), n = ', tmptn))))


# World Map ---------------------------------------------------------------


map.w <- map_data('world')

ggplot() +
  geom_polygon(data = map.w, aes(x = long, y = lat, group = group), fill = "white") +
  #geom_path(data = map.w, color="gray", aes(x = long, y = lat, group=group))  +
  
  geom_jitter(data = dat, pch = 21, size = 3, color = "black",
             aes(x = long, y = lat, fill = climclasslat2)) +
  
  theme(legend.justification = c(0,0), 
        legend.position = c(0, 0),
        legend.box.margin = margin(c(10,10,10,10)),
  
        legend.text = element_text(size = rel(0.9)),
        legend.title = element_text(size = rel(1.2), face = "bold"),
        legend.background = element_rect(linetype = 1, size = 0.5, colour = 1),
        
        axis.text = element_blank()) + 
  scale_fill_manual(values = c("blue2", "gold")) +
  
  labs(fill = "Latitude Class", x = NULL, y = NULL)

ggsave("../_figs/actual_figs/WMfig_world-map.png", height = 4.5, width = 7)
ggsave("../../../DW_CropRotWeeds_Meta/MANUSCRIPT SECTIONS/figs_for_pub/1_world-map.png", height = 4.5, width = 7)
