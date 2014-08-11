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
pp <- fullTable32[c("VisitID", "ExamDate", "EPICID", ppnames)]
#pp <- merge(DMT[c("VisitID", "TreatmentMolecule", "TreatmentType")], pp)


pp[, "Siena_PBVCRate"] <- NA
pp[, "PrevSiena_PBVC"] <- NA
pp[, "PrevSiena_PBVCRate"] <- NA
#pp[, "PrevTreatmentM"] <- NA
#pp[, "PrevTreatmentT"] <- NA
pp[, "PrevDiseaseDuration"] <- NA
pp[, "PrevNew_T2_Lesions"]<-NA
pp[order(pp$EPICID, pp$ExamDate),]
nvisits <- nrow(pp)
for(i in 1:(nvisits-1)){
  dSPBVC <- pp[i+1, "Siena_PBVC"] - pp[i, "Siena_PBVC"]
  dDay <-as.numeric(as.Date(pp[i+1,]$ExamDate) - as.Date(pp[i,]$ExamDate))
  dYear <- dDay/365
  if (pp[i+1, "EPICID"] == pp[i, "EPICID"] ){
    pp[i+1, "Siena_PBVCRate"] <- dSPBVC/dYear
    pp[i+1, "PrevSiena_PBVC"] <- pp[i, "Siena_PBVC"]
    pp[i+1, "PrevSiena_PBVCRate"] <- pp[i, "Siena_PBVCRate"]
    pp[i+1, "PrevDiseaseDuration"] <- pp[i, "DiseaseDuration"]
    pp[i+1, "PrevNew_T2_Lesions"]<-pp[i, "New_T2_Lesions"]
    #pp[i+1, "PrevTreatmentM"] <- pp[i+1, "TreatmentMolecule"]
    #p[i+1, "PrevTreatmentT"] <- pp[i+1, "TreatmentType"]
  }
}

#Remove
pp <- pp[, ! colnames(pp) %in% c("Siena_PBVC", "DiseaseDuration", "New_T2_Lesions")]
#Siena_PBVC remove 0 or NA
pp <- pp[pp["PrevSiena_PBVC"]!=0, ]
pp <- pp[!is.na(pp["PrevSiena_PBVC"]), ]
# merge with psidd
ppnoNA<-merge(pp,psidd)
ppnoNA<-ppnoNA[complete.cases(ppnoNA),]
# Seperate those with EPICID, VisitID and ExamDate and those without
ppidd <- ppnoNA
diagnopp <-ppnoNA[, !(names(ppnoNA)%in%c("ExamDate","VisitID", "EPICID"))]
### Save #####
save(diagnopp, ppidd, file=ppPath)
h5write(data.frame(diagnopp), filePath,"diagnopp")
file.copy(filePath, filePathPython, overwrite = TRUE)
