# source("./step0/DB_get_data.R")
load("./step0/result.RData")
save(fssc2, fullTable3, TABLES, file = './step1/ini.RData')
rm(list = ls())