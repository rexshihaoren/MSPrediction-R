#########Some additional features to add########
setwd("~/Dropbox/research/MSBioScreen/MSPrediction-R/Data Scripts")
source("helper.r")
load("step0/result.RData")
load(ppPath)
DMT<-read.table("tableDMT.csv")
file.create(apPath)
####### reconstruct additional parameters ########
apnames <- c("MSSS", "GM_Volume", "OpticNeuritis", "VitaminD_Level", "Smoking")
ap <- fullTable32[c("VisitID", "ExamDate", "EPICID", apnames)]

ap[, "MSSSRate"] <- NA
ap[, "GMVRate"] <- NA
ap[, "PrevMSSS"] <- NA
ap[, "PrevGMV"] <- NA
ap[, "PrevOpticNeuritis"]<-NA
ap[, "PrevMSSSRate"] <- NA
ap[, "PrevGMVRate"] <- NA
ap[, "PrevVDL"] <- NA
ap[, "PrevSmoking"] <- NA
ap[order(ap$EPICID, ap$ExamDate),]
nvisits <- nrow(ap)
for(i in 1:(nvisits-1)){
  dMSSS <- ap[i+1, "MSSS"] - ap[i, "MSSS"]
  dGMV <- ap[i+1, "GM_Volume"] - ap[i, "GM_Volume"]
  dDay <-as.numeric(as.Date(ap[i+1,]$ExamDate) - as.Date(ap[i,]$ExamDate))
  dYear <- dDay/365
  if (ap[i+1, "EPICID"] == ap[i, "EPICID"] ){
    ap[i+1, "MSSSRate"] <- dMSSS/dYear
    ap[i+1, "GMVRate"] <- dGMV/dYear
    ap[i+1, "PrevMSSS"] <- ap[i, "MSSS"]
    ap[i+1, "PrevGMV"] <- ap[i, "GM_Volume"]
    ap[i+1, "PrevOpticNeuritis"]<-ap[i, "OpticNeuritis"]
    ap[i+1, "PrevVDL"] <- ap[i, "VitaminD_Level"]
    ap[i+1, "PrevSmoking"]<-ap[i, "Smoking"]
    ap[i+1, "PrevMSSSRate"] <- ap[i, "MSSSRate"]
    ap[i+1, "PrevGMVRate"] <- ap[i, "GMVRate"]
  }
}

#Remove
ap <- ap[, ! colnames(ap) %in% c("MSSS", "GM_Volume", "OpticNeuritis", "MSSSRate", "GMVRate", "VitaminD_Level", "Smoking")]
# merge with ppidd
apidd<-merge(ap,ppidd)
#apnoNA<-apnoidd[complete.cases(apnoNA),]
# Seperate those with EPICID, VisitID and ExamDate and those without
# diagnoap <-apnoNA[, !(names(apnoNA)%in%c("ExamDate","VisitID", "EPICID"))]
diagnoap <-apidd[, !(names(apidd)%in%c("ExamDate","VisitID", "EPICID"))]
### Save #####
save(diagnoap, apidd, file=apPath)
h5write(diagnoap, filePath,"diagnoap")
file.copy(filePath, filePathPython, overwrite = TRUE)
