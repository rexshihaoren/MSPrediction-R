#########Some additional features to add########
setwd("~/Dropbox/research/MSBioScreen/MSPrediction-R/Data Scripts")
source("helper.r")
load("step0/result.RData")
load(ppPath)
DMT<-read.table("tableDMT.csv")
file.create(apPath)
####### reconstruct additional parameters ########
apnames <- c("MSSS", "GM_Volume", "OpticNeuritis", "VitaminD_Level", "Smoking")
fullTable32 <- fullTable32[!duplicated(fullTable32$VisitID),]
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
ap <- ap[order(ap$EPICID, ap$ExamDate),]

# Dataframe where 0 is for PrevXXRate
ap0<-ap
#
nvisits <- nrow(ap)
newEPIC <- 0
for(i in 1:(nvisits-1)){
  dMSSS <- ap[i+1, "MSSS"] - ap[i, "MSSS"]
  dGMV <- ap[i+1, "GM_Volume"] - ap[i, "GM_Volume"]
  dDay <-as.numeric(as.Date(ap[i+1,]$ExamDate) - as.Date(ap[i,]$ExamDate))
  dYear <- dDay/365
  oldEPIC <- ap[i, "EPICID"]
  newEPIC <- ap[i+1, "EPICID"]
  if (i ==1) {
    newEPIC <- newEPIC+1
    ap0[i, "PrevMSSSRate"] <- 0
    ap0[i, "PrevGMVRate"] <- 0
  }
  if (oldEPIC != newEPIC) {
    newEPIC <- newEPIC+1
    ap0[i+1, "PrevMSSSRate"] <- 0
    ap0[i+1, "PrevGMVRate"] <- 0
  } else {
    ap0[i+1, "MSSSRate"] <- dMSSS/dYear
    ap0[i+1, "GMVRate"] <- dGMV/dYear
    ap0[i+1, "PrevMSSS"] <- ap0[i, "MSSS"]
    ap0[i+1, "PrevGMV"] <- ap0[i, "GM_Volume"]
    ap0[i+1, "PrevMSSSRate"] <- ap0[i, "MSSSRate"]
    ap0[i+1, "PrevGMVRate"] <- ap0[i, "GMVRate"]
  }
  if (oldEPIC == newEPIC){
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

# Extract VisitID, PrevMSSSRate and PrevGMVRate from ap0
ap0idd<- ap0[, c('VisitID','PrevMSSSRate','PrevGMVRate')]

#Remove
ap <- ap[, ! colnames(ap) %in% c("MSSS", "GM_Volume", "OpticNeuritis", "MSSSRate", "GMVRate", "VitaminD_Level", "Smoking")]
# merge with ppidd
apidd<-merge(ap,ppidd)
#apnoNA<-apnoidd[complete.cases(apnoNA),]
# Seperate those with EPICID, VisitID and ExamDate and those without
# diagnoap <-apnoNA[, !(names(apnoNA)%in%c("ExamDate","VisitID", "EPICID"))]
diagnoap <-apidd[, !(names(apidd)%in%c("ExamDate","VisitID", "EPICID"))]

# merge diagno0idd's PrevEDSSRate, pp0idd's PrevSiena_PBVCRate, and ap0idd's PrevMSSSRate and PrevGMVRate with everything else in apidd
load(diagnoPath)
load(ppPath)

ap0idd <- merge(diagno0idd, ap0idd)
ap0idd <- merge(ap0idd, pp0idd)

ap0idd <- merge(ap0idd, apidd[, !(names(apidd)%in%c('PrevEDSSRate','PrevSiena_PBVCRate','PrevMSSSRate','PrevGMVRate'))])

diagno0ap <- ap0idd[,!(names(ap0idd)%in%c("ExamDate","VisitID", "EPICID"))]
### Save #####
save(diagnoap, apidd, ap0idd, diagno0ap, file=apPath)
h5write(diagnoap, filePath,"diagnoap")
file.copy(filePath, filePathPython, overwrite = TRUE)
