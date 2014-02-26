setwd('~/Dropbox/QOL - BIOSCREEN/step3/')

RUN_GET_PARAMS = !(file.exists('./coefficients.csv')
                   | file.exists('./data_all.csv'))

if (RUN_GET_PARAMS){
  warning('INIT: Parameters not founds... Creating the parameters from the database.')
  source('./get_PCA_params.R')
}

source("./convert_input.R")
source('./make_plot.R')


# Things to do 
# update the curve color
# add plot with different width
# add the flame plot