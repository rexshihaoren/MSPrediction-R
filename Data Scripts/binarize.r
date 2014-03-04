setwd("~/Dropbox/research/MSBioScreen/MSPrediction-R/Data Scripts")
# get modified-fam2
require(ggplot2)
modfam2<-read.csv("step3/data_all.csv")
# only 5 cols, group1~3, relative-pain, enjoylife
modfam2<-modfam2[,9:13]
# fetch original fam2
load("step1/result.RData")
# get rid visitID
fam2<-fam2[,-1]
# plotting PDF
ggplot(modfam2) + geom_histogram(aes(x=EnjoyLife))
ggsave(file="plots/modfam2.pdf")
ggplot(fam2) + geom_histogram(aes(x=EnjoyLife))
ggsave(file="plots/fam2.pdf")
# find the enjoy life median
medianEL<- median(modfam2$EnjoyLife)
# binarize the modfam2, fam2
modfam2$EnjoyLife<-ifelse(modfam2$EnjoyLife <= medianEL, 0, 1)
fam2$EnjoyLife<-ifelse(fam2$EnjoyLife <= medianEL, 0, 1)
# Plot PDF after binarize
ggplot(modfam2) + geom_histogram(aes(x=EnjoyLife))
ggsave(file="plots/bin_modfam2.pdf")
ggplot(fam2) + geom_histogram(aes(x=EnjoyLife))
ggsave(file="plots/bin_fam2.pdf")
#save binarized fam2,modfam2
save(fam2, file="data/bin_fam2.RData")
save(modfam2, file="data/bin_modfam2.RData")
# Plot CDF 
generateCDF<-function(somedf, plotfunc){
  dfname = deparse(substitute(somedf))
  pfname = deparse(substitute(plotfunc))
  for (i in colnames(somedf)){
    if (i != "EnjoyLife"){
      if (pfname == "geom_density"){
        ggplot(somedf) + plotfunc(aes_string(x=i, group = "EnjoyLife", color = "EnjoyLife"))
      } else {
        ggplot(somedf) + plotfunc(aes_string(x=i, group = "EnjoyLife", fill = "EnjoyLife"), position = "dodge")
      }
      fpath = paste("plots/",paste(dfname, "cdf",i, sep = "_"), ".pdf", sep="")
      ggsave(file = fpath)
    }
  }
}
generateCDF(modfam2,geom_density)
generateCDF(fam2, geom_histogram)