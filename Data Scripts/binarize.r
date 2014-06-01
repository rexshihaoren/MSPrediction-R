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

# plotting histogram given a dataframe and a target column, and a filename
genhisto<-function(somedf, target, filename){
  filepath = paste("plots/",filename, ".pdf",sep="")
  ggplot(somedf) + geom_histogram(aes_string(x=target))
  # ggsave(file="plots/modfam2.pdf")
  ggsave(file=filepath)
}
genhisto(modfam2, "EnjoyLife", "modfam2")
genhisto(fam2, "EnjoyLife", "fam2")

# Binarize EnjoyLife
# find the enjoy life median
medianEL<- median(modfam2$EnjoyLife)
# binarize the modfam2, fam2
modfam2$EnjoyLife<-ifelse(modfam2$EnjoyLife <= medianEL, 0, 1)
fam2$EnjoyLife<-ifelse(fam2$EnjoyLife <= medianEL, 0, 1)
# Plot PDF after binarize
genhisto(modfam2, "EnjoyLife", "bin_modfam2")
genhisto(fam2, "EnjoyLife", "bin_fam2")

#save binarized fam2,modfam2, merged in HDF5 format
h5createFile('data/predData.h5')
h5write(fam2, "data/predData.h5","fam2")
h5write(modfam2,"data/predData.h5","modfam2")
h5write(merged, "data/predData.h5", "merged")

# Plot Conditional PDF 
generateCPDF<-function(somedf, plotfunc, target){
  dfname = deparse(substitute(somedf))
  pfname = deparse(substitute(plotfunc))
  for (i in colnames(somedf)){
    if (i != target){
      if (pfname == "geom_density"){
        ggplot(somedf) + plotfunc(aes_string(x=i, group = target, color = target))
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

fitCPDF<- function(df, xname, yname, f){
  # Fitting Conditional pdf on top of histogram, and save that plot.
  #
  # Args: 
  #   df: dataset
  #   xname: target col name
  #   yname: condition col name
  #   f: string that indicated the density function put on top of the histogram
  #
  # Returns:
  #   Model conditional pdf
  dfname = deparse(substitute(df))
  xcol <- df[[xname]]
  ycol <- df[[yname]]
  yrange = range(ycol)
  sf <- list()
  for (i in range(1,length(yrange))){
    condData <- xcol[ycol == yrange[i]]
    params <- c(fitdist(condData, f)$estimate)
    # This step is IMPORTANT
    dfunc = match.fun(paste('d', f, sep = ''))
    sf[[i]] = stat_function(fun = dfunc, args = params)
  }
  # It is important here that binwidth is 1, so the y is actually density!!!
 ggplot(df) + 
   geom_histogram(aes_string(y="..density..", x=xname, group = yname, fill=yname), binwidth=1, position="dodge") + sf
 # Save plots
 fpath = paste("plots/",paste(dfname, "fitcpdf", xname,"on",yname, f, sep = "_"), ".pdf", sep="")
 ggsave(file = fpath)
}

# Plot Fitted Histogram for fam2 with 'norm' normal distribution
for (cname in colnames(fam2)){
  if (cname != "EnjoyLife"){
    fitCPDF(fam2, cname, "EnjoyLife", "norm")
  }
}
