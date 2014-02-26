# source("./step1/BioScreen.R")
load("./step1/result.RData")
save(allQOL, resolution, file = './step2/ini.RData')
rm(list = ls())