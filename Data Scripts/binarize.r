setwd("~/Dropbox/research/MSBioScreen/MSPrediction-R/Data Scripts")
# get modified-fam2
require(ggplot2)
require(rhdf5)
modfam2<-read.csv("step3/data_all.csv")
# only 5 cols, group1~3, relative-pain, enjoylife
modfam2<-modfam2[,9:13]
# fetch original fam2
load("step1/result.RData")
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
# ggplot(modfam2) + geom_histogram(aes(x=EnjoyLife))
# ggsave(file="plots/bin_modfam2.pdf")
# ggplot(fam2) + geom_histogram(aes(x=EnjoyLife))
# ggsave(file="plots/bin_fam2.pdf")
#save binarized fam2,modfam2 in HDF5 format
h5createFile('data/predData.h5')
h5write(fam2, "data/predData.h5","fam2")
h5write(modfam2,"data/predData.h5","modfam2")

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