#### All the functions needed for MSBioScreen-R ######

### Packages ####
setwd("~/Dropbox/research/MSBioScreen/MSPrediction-R/Data Scripts")
filePath <- 'data/predData.h5'
filePathPython <- '../../MSPrediction-Python/data/'
binarizePath<- 'data/binarize.RData'
diagnoPath <- 'data/diagno.RData'
psPath<- 'data/ps.RData'
ppPath<-'data/pp.RData'
apPath<- 'data/ap.RData'
require(ggplot2)
require(rhdf5)
require(MASS)
require(fitdistrplus)
require(plyr)
require(RMySOL)

Binarize <- function(df, target){
  # to Binarize a target column of a datafram
  med <- median(df[, target])
  bin <- df
  bin[, target] <- ifelse(bin[, target]<= med, 0 , 1)
  as.data.frame(bin)
}

DataProcessing <- function(df, target = ""){
  # Processing a data frame to make everything (0, 1] except the target col
  if (target == ""){
    loc <- ncol(df)+1
  } else{
    loc <- grep(target, colnames(df))
  }
  processing <- df
  processing <- apply(processing, 2, function(x) (x-min(x))/(max(x)-min(x)))
  processing[,-loc] <- apply(processing[,-loc], c(1,2), function(x) if (x == 0) x+1e-12 else x)
  as.data.frame(processing)
}


### Testing add column rate of everything ####

calcRate<-function(df, colName, tColName, index){
  # Add a rate column of a target column within index, an identity
  #
  # Args: 
  #   df: dataset
  #   colName: target col name
  #   tColName: time col name
  #   index: id col name
  #
  # Returns:
  #   
  
  nvisits <- nrow(df)
  newColName <- paste(colName, "Rate", sep = "") 
  df[,newColName] <- NA
  for(i in 1:(nvisits-1)){
    dDiff <- df[i+1, colName] - df[i, colName]
    dDay <- as.numeric(as.Date(df[i+1,tColName]) - as.Date(df[i,tColName]))
    dYear <- dDay/365
    if (df[i+1, index] == df[i, index] ){
      df[i+1, newColName] <- dDiff/dYear
    }
  }
  newdf <- df
  return(newdf)
}


###### Plotting Functions for Statistical Analysis#############

gendist<-function(somedf, plotfunc, target, filename){
  # Plotting distribution (histogram/density) given a dataframe and a target column, and a filename
  # Args:
  #    somdf: dataframe
  #    plotfunc: geom_histogram or geom_density
  #    target: target column
  #    filename: filename given to the hitogram to generate
  #
  # Returns:
  #   hitogram on target column
  pfname = deparse(substitute(plotfunc))
  filepath = paste("plots/",paste(filename, pfname, sep = "_"), ".pdf",sep="")
  somedf_noNA <- somedf[!is.na(somedf[target]),]
  ggplot(somedf_noNA) + plotfunc(aes_string(x=target))
  ggsave(file=filepath)
}


generateCPDF<-function(somedf, plotfunc, target){
  # Plot Conditional PDF 
  dfname = deparse(substitute(somedf))
  pfname = deparse(substitute(plotfunc))
  for (i in colnames(somedf)){
    if (i != target){
      if (pfname == "geom_density"){
        ggplot(somedf) + plotfunc(aes_string(x=i, group = target, fill= target), alpha = 0.8)
      } else {
        ggplot(somedf) + plotfunc(aes_string(x=i, group = target, fill = target), position = "dodge")
      }
      ggsave(file = paste("plots/",paste(dfname, "cpdf",i, sep = "_"), ".pdf", sep=""))
      #ggsave(file = fpath)
    }
  }
}


fitCPDF<- function(df, xname, yname, f, plotfunc, method, start = NULL){
  # Fitting Conditional pdf on top of histogram, and save that plot.
  #
  # Args: 
  #   df: dataset
  #   xname: target col name
  #   yname: condition col name
  #   f: string that indicated the density function put on top of the histogram
  #   plotfunc: original data plot, geom_hitogram or geom_density
  #   method: the character string coding for the fitting method
  #
  # Returns:
  #   Model conditional pdf
  dfname = deparse(substitute(df))
  pfname = deparse(substitute(plotfunc))
  xcol <- df[[xname]]
  ycol <- df[[yname]]
  xrange <- unique(xcol)
  yrange <- unique(ycol)
  xlen <- length(xrange)
  ylen <- length(yrange)
  sf <- list()
  for (i in range(1,ylen)){
    condData <- xcol[ycol == yrange[i]]
    params <- c(fitdist(condData, f, method, start = start)$estimate)
    # This step is IMPORTANT
    dfunc = match.fun(paste('d', f, sep = ''))
    if(pfname == "geom_histogram"){
      sf[[i]] = stat_function(fun = dfunc, n = xlen, args = params, lty = "dashed")
    }else{
      sf[[i]] = stat_function(fun = dfunc, args = params, lty = "dashed")
    }
  }
  if (pfname == "geom_histogram"){
    ggplot(df) + 
    plotfunc(aes_string(y="..density.. * 0.1", x=xname, group = yname, fill=yname), binwidth=0.1, position="dodge", width = 2) + 
    sf
  } else {
    if (f == "beta"){
      ggplot(df) + 
        plotfunc(aes_string(x=xname, group = yname, group = yname, fill=yname), alpha = .8) + 
        sf + scale_x_continuous(limits = c(0,1))
    } else {
      ggplot(df) + 
        plotfunc(aes_string(x=xname, group = yname, group = yname, fill=yname), alpha = .8) + 
        sf
    }
  }
 # Save plots
 fpath = paste("plots/",paste(dfname, "fitcpdf", xname,"on",yname, f, sep = "_"), ".pdf", sep="")
 ggsave(file = fpath)
}


# # List of density functions
# densfunlist <- c("norm", "exp", "gamma", "nbinom", "geom", "beta", "logis", "nbinorm")
# # TODO??
# PickBestFit <- function(x, method){
#   # Fit the best density functions out of densfunlist
#   # Args:
#   #   x: a numeric vector
#   #   method: the character string coding for the fitting method
#   # Returns:
#   #   fitdistr obj with the best performance based on estimated standard error mean
#   errlist <- list()
#   for (f in densfunlist){
#     obj <- fitdist(x, f)
#     stderror <- obj$sd[1]
#     errlist[stderror] <- obj
#   }
# }