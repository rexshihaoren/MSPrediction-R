setwd("~/Dropbox/research/MSBioScreen/MSPrediction-R/Data Scripts")
# get modified-fam2
require(ggplot2)
require(rhdf5)
require(MASS)
require(fitdistrplus)
modfam2<-read.csv("step3/data_all.csv")
# fetch original fam2
load("step1/result.RData")

# Merge fam2, modfam2, fullTable3

#First change fam2 colname "VisitId" to "VisitID"
colnames(fam2)[1] = "VisitID"
#merged = merge(fam2, modfam2, by = "VisitID", all.y = TRUE)
merged = merge(fam2, modfam2)
merged <- merge(merged, fullTable3)


# only 5 cols, group1~3, relative-pain, enjoylife
modfam2<-modfam2[,9:13]



# get rid visitID
fam2<-fam2[,-1]

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
  ggplot(somedf) + plotfunc(aes_string(x=target))
  # ggsave(file="plots/modfam2.pdf")
  ggsave(file=filepath)
}
gendist(modfam2, geom_histogram, "EnjoyLife", "modfam2")
gendist(fam2, geom_histogram, "EnjoyLife", "fam2")
gendist(fullTable3, geom_histogram, "ActualEDSS", "fullTable3")
gendist(fullTable3, geom_density, "ActualEDSS", "fullTable3")

# Binarize EnjoyLife
# find the enjoy life median
medianEL<- median(modfam2$EnjoyLife)
# binarize the modfam2, fam2
modfam2$EnjoyLife<-ifelse(modfam2$EnjoyLife <= medianEL, 0, 1)
fam2$EnjoyLife<-ifelse(fam2$EnjoyLife <= medianEL, 0, 1)
# Plot PDF after binarize
gendist(modfam2, geom_histogram,"EnjoyLife", "bin_modfam2")
gendist(fam2, geom_histogram,"EnjoyLife", "bin_fam2")

#save binarized fam2,modfam2, merged in HDF5 format
h5createFile('data/predData.h5')
# h5write(fam2, "data/predData.h5","fam2")
# h5write(modfam2,"data/predData.h5","modfam2")
# h5write(merged, "data/predData.h5", "merged")

# Plot Conditional PDF 
generateCPDF<-function(somedf, plotfunc, target){
  dfname = deparse(substitute(somedf))
  pfname = deparse(substitute(plotfunc))
  for (i in colnames(somedf)){
    if (i != target){
      if (pfname == "geom_density"){
        ggplot(somedf) + plotfunc(aes_string(x=i, group = target, fill= target), alpha = 0.8)
      } else {
        ggplot(somedf) + plotfunc(aes_string(x=i, group = target, fill = target), position = "dodge")
      }
      fpath = paste("plots/",paste(dfname, "cpdf",i, sep = "_"), ".pdf", sep="")
      ggsave(file = fpath)
    }
  }
}
generateCPDF(modfam2,geom_density, "EnjoyLife")
generateCPDF(fam2, geom_histogram, "EnjoyLife")

fitCPDF<- function(df, xname, yname, f, plotfunc, method){
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
  yrange = range(ycol)
  sf <- list()
  for (i in range(1,length(yrange))){
    condData <- xcol[ycol == yrange[i]]
    params <- c(fitdist(condData, f, method)$estimate)
    # This step is IMPORTANT
    dfunc = match.fun(paste('d', f, sep = ''))
    sf[[i]] = stat_function(fun = dfunc, args = params, lty = "dashed")
  }
  if (pfname == "geom_histogram"){
    # It is important here that binwidth is 1, so the y is actually density!!!
#     bw <- 0.01
    ggplot(df) + 
    plotfunc(aes_string(y="..density.. * 0.1", x=xname, group = yname, fill=yname), binwidth=0.1, position="dodge", width = 2) + 
    sf
  } else {
    ggplot(df) + 
    plotfunc(aes_string(x=xname, group = yname, group = yname, fill=yname), alpha = .8) + 
    sf
  }
 # Save plots
 fpath = paste("plots/",paste(dfname, "fitcpdf", xname,"on",yname, f, sep = "_"), ".pdf", sep="")
 ggsave(file = fpath)
}


# List of density functions
densfunlist <- c("norm", "exp", "gamma", "nbinom", "geom", "beta", "logis", "nbinorm")
# TODO??
PickBestFit <- function(x, method){
  # Fit the best density functions out of densfunlist
  # Args:
  #   x: a numeric vector
  #   method: the character string coding for the fitting method
  # Returns:
  #   fitdistr obj with the best performance based on estimated standard error mean
  errlist <- list()
  for (f in densfunlist){
    obj <- fitdist(x, f)
    stderror <- obj$sd[1]
    errlist[stderror] <- obj
  }
}


###Examples
# Plot Fitted Histogram for fam2 with 'norm' normal distribution
for (cname in colnames(fam2)){
  if (cname != "EnjoyLife"){
    fitCPDF(fam2, cname, "EnjoyLife", "norm", geom_histogram, "mle")
  }
}

# Plot Fitted density for modfam2 with 'norm' normal distribution
for (cname in colnames(modfam2)){
  if (cname != "EnjoyLife"){
    print(cname)
    fitCPDF(modfam2, cname, "EnjoyLife", "gamma", geom_density, "mme")
  }
}