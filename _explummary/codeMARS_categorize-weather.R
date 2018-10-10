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

# Quantify data to simulate meaningfully ----------------------------------

crnsmy <-
  crn %>%
  group_by(trt) %>%
  summarise(yld = mean(dm_Mgha, na.rm = T),
            yld_sd = sd(dm_Mgha, na.rm = T))

MakeParms <- function(myn){
fmeans <- crnsmy$yld
fsds <- crnsmy$yld_sd
fns <- rep(myn,3)

fparms <- tibble(mean = fmeans,
                  sd = fsds,
                  n = fns)
p1 <- 
  fparms %>% 
  pmap(rnorm)

mysim <- tibble(n = myn,
                c2y = p1[[1]],
                c3y = p1[[2]],
                c4y = p1[[3]])  
return(mysim)
}

# use pmap (from purrr) to sample from different distributions
# See how significantly different they are w/10 points

MakeParms(10) %>%
  bind_rows(MakeParms(20)) %>%
  bind_rows(MakeParms(30)) %>%
  gather(c2y:c4y, key = rot, value = yld) %>%
  ggplot(aes(rot, yld)) + 
  stat_summary(aes(color = factor(n))) + 
  facet_grid(~n)
