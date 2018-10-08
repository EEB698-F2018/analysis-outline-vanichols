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
