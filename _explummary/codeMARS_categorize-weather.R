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
library(lubridate)
library(ggrepel)

path <- rstudioapi::getActiveDocumentContext()$path
Encoding(path) <- "UTF-8"
setwd(dirname(path))


# read in data for reference ----------------------------------------------

crn <- read_csv("../_data/tidy/td_corn-ylds.csv")
wea <- read_csv("../_data/tidy/td_wea.csv")


# Find long-term means ----------------------------------------------------

# Preceip
pcp <- wea %>% 
  # Define a water year as Oct-Oct
  #mutate(year = ifelse(doy > 274, year + 1, year)) %>%
  # Keep precip from specific days
  filter(doy >182 & doy < 244 ) %>%
  group_by(year) %>%
  summarise(totP_mm = sum(precip_mm, na.rm = T)) %>%
  mutate(scP_mm = scale(totP_mm))

deg <- wea %>% 
  filter(doy >182 & doy < 244 ) %>%
  group_by(year) %>%
  summarise(myT_C = mean(avgT_oC)) %>%
  mutate(scT_C = scale(myT_C))

mywea <- pcp %>% left_join(deg)

mywea %>%
  ggplot(aes(scT_C, scP_mm)) + 
  geom_point() +
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) +
  geom_label(aes(label = year))



# Quantify yield diffs ----------------------------------------------------

# What if I divide them into 2 groups based on spring rain, THEN look at July/Aug temps

crn %>%
  mutate(dm_Mgha = dm_Mgha*1.15) %>%
  spread(trt, value = dm_Mgha) %>%
  mutate(up4 = C4 - C2,
         up3 = C3 - C2) %>%
  group_by(year) %>%
  summarise(up4 = (mean(up4)),
            gb = ifelse(up4 > 0, "positive", "negative"),
            up4 = abs(up4)) %>%

  left_join(mywea) %>%
  
  ggplot(aes(scT_C, scP_mm)) + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) +
  geom_point(aes(size = up4, fill = gb), pch = 21) +
  geom_text_repel(aes(label = year)) + 
  
  annotate(geom = "text", label = "Hot and Dry", 
           x = 1.25, y = -1.25, fontface = "italic", color = "gray70")  +
  annotate(geom = "text", label = "Hot and Wet", 
           x = 1.25, y = 1.25, fontface = "italic", color = "gray70")  +
  annotate(geom = "text", label = "Cool and Dry", 
           x = -1.25, y = -1.25, fontface = "italic", color = "gray70")  +
  annotate(geom = "text", label = "Cool and Wet", 
           x = -1.25, y = 1.25, fontface = "italic", color = "gray70")  +
  coord_cartesian(xlim = c(-2.5, 2.5), ylim = c(-2.5, 2.5)) +
  scale_fill_manual(values = c("red", "blue")) +
  theme_bw() + 
  labs(x = "Jul-Aug Temp\nStandard Deviations From Long Term Mean",
       y = "Jul-Aug Precip\nStandard Deviations From Long Term Mean",
       size = "Size of Yield Differential\n4-year vs 2-year\n[Mg/ha]",
       fill = "Yield Differential Sign")
