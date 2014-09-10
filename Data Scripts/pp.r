###### Add more Previous year parameters to diagno #######

####### Load more features from fullTable32 and DMT #########
setwd("~/Dropbox/research/MSBioScreen/MSPrediction-R/Data Scripts")
source("helper.r")
load("step0/result.RData")
load(psPath)
DMT<-read.table("tableDMT.csv")
file.create(ppPath)
####### reconstruct previous year parameters ########
ppnames <- c("DiseaseDuration", "Siena_PBVC", "New_T2_Lesions")
fullTable32 <- fullTable32[!duplicated(fullTable32$VisitID),]
pp <- fullTable32[c("VisitID", "ExamDate", "EPICID", ppnames)]
pp <- merge(DMT[c("VisitID", "TreatmentMolecule", "TreatmentType")], pp)


pp[, "Siena_PBVCRate"] <- NA
pp[, "PrevSiena_PBVC"] <- NA
pp[, "PrevSiena_PBVCRate"] <- NA
pp[, "PrevTreatmentM"] <- NA
pp[, "PrevTreatmentT"] <- NA
pp[, "PrevDiseaseDuration"] <- NA
pp[, "PrevNew_T2_Lesions"]<-NA
pp <- pp[order(pp$EPICID, pp$ExamDate),]

# Dataframe where 0 is for PrevXXRate
pp0 <- pp
#
nvisits <- nrow(pp)
for(i in 1:(nvisits-1)){
  dSPBVC <- pp[i+1, "Siena_PBVC"] - pp[i, "Siena_PBVC"]
  dDay <-as.numeric(as.Date(pp[i+1,]$ExamDate) - as.Date(pp[i,]$ExamDate))
  dYear <- dDay/365
  oldEPIC <- pp[i, "EPICID"]
  newEPIC <- pp[i+1, "EPICID"]
  if (i == 1) {
    pp0[i, "PrevSiena_PBVCRate"] <-0
  }
  if (oldEPIC != newEPIC){
    pp0[i+1, "PrevSiena_PBVCRate"] <-0
  } else {
    pp0[i+1, "PrevTreatmentT"]<-pp0[i,"TreatmentType"]
    pp0[i+1, "PrevTreatmentM"]<-pp0[i, "TreatmentMolecule"]
    pp0[i+1, "Siena_PBVCRate"] <- dSPBVC/dYear
    pp0[i+1, "PrevSiena_PBVC"] <- pp0[i, "Siena_PBVC"]
    pp0[i+1, "PrevSiena_PBVCRate"] <- pp0[i, "Siena_PBVCRate"]   
  }
  if (oldEPIC == newEPIC){
    pp[i+1, "PrevTreatmentT"]<-pp[i,"TreatmentType"]
    pp[i+1, "PrevTreatmentM"]<-pp[i, "TreatmentMolecule"]
    pp[i+1, "Siena_PBVCRate"] <- dSPBVC/dYear
    pp[i+1, "PrevSiena_PBVC"] <- pp[i, "Siena_PBVC"]
    pp[i+1, "PrevSiena_PBVCRate"] <- pp[i, "Siena_PBVCRate"]
    pp[i+1, "PrevDiseaseDuration"] <- pp[i, "DiseaseDuration"]
    pp[i+1, "PrevNew_T2_Lesions"]<-pp[i, "New_T2_Lesions"] 
  }
}

# Extract VisitID and PrevSiena_PBVCRate from pp0
pp0idd<- pp0[, c('VisitID','PrevSiena_PBVCRate')]

#Remove
pp <- pp[, ! colnames(pp) %in% c("Siena_PBVC", "DiseaseDuration", "New_T2_Lesions", "Siena_PBVCRate", "TreatmentType", "TreatmentMolecule")]
#Siena_PBVC remove 0 or NA
# pp <- pp[pp["PrevSiena_PBVC"]!=0, ]
# pp <- pp[!is.na(pp["PrevSiena_PBVC"]), ]
# merge with psidd
ppidd<-merge(pp,psidd)
# Seperate those with EPICID, VisitID and ExamDate and those without
diagnopp <-ppidd[, !(names(ppidd)%in%c("ExamDate","VisitID", "EPICID"))]
### Save #####
save(diagnopp, ppidd, pp0idd, file=ppPath)
h5write(diagnopp, filePath,"diagnopp")
file.copy(filePath, filePathPython, overwrite = TRUE)