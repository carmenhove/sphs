#PROJECT SET-UP

install.packages("pacman")
library(pacman)

pacman::p_load(devtools, tidyverse, openxlsx, brms, here,
               calecopal, cowplot, report, ggpubr, extrafont)
               
font_import()

source("./R/project_functions.R")

options(scipen = 999)
