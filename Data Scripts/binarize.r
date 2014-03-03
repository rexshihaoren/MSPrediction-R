#setwd("~/Dropbox/research/MSBioScreen/MSPrediction-R/Data Scripts")
# get modified-fam2
modfam2<-read.csv("step3/data_all.csv")
# only 5 cols, group1~3, relative-pain, enjoylife
modfam2<-modfam2[,9:13]
# fetch original fam2
load("step1/result.RData")
# get rid visitID
fam2<-fam2[,-1]
# plotting PDF
mf<-ggplot(modfam2, aes(x=EnjoyLife))
mf + geom_histogram()
ggsave(file="modfam2.pdf")
f<-ggplot(fam2,aes(x=EnjoyLife))
f + geom_density()
ggsave(file="fam2.pdf")
# reorder fam2 and modfam2 by EnjoyLife
modfam2<-modfam2[with(modfam2, order(EnjoyLife)), ]
fam2<-fam2[with(fam2, order(EnjoyLife)), ]
# binarize the modfam2, fam2
nmfrow <- nrow(modfam2)
modfam2$EnjoyLife[1:nmfrow/2] <- 0
modfam2$EnjoyLife[(nmfrow/2+1):nmfrow] <- 1
nfrow <- nrow(fam2)
fam2$EnjoyLife[1:nfrow/2] <- 0
fam2$EnjoyLife[(nfrow/2+1):nfrow] <-1
# Plot PDF after binarize
mf<-ggplot(modfam2, aes(x=EnjoyLife))
mf + geom_histogram()
ggsave(file="bin_modfam2.pdf")
f<-ggplot(fam2,aes(x=EnjoyLife))
f + geom_density()
ggsave(file="bin_fam2.pdf")
#save binarized fam2,modfam2
save(fam2, file="bin_fam2.RData")
save(modfam2, file="bin_modfam2.RData")