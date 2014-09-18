#### Gather features into groups for later merging into dataframes ###

##### Set up ####################
setwd("~/Dropbox/research/MSBioScreen/MSPrediction-R/Data Scripts")
source("help.r")
f <- h5createFile(filePath)
# If 'data/predData.h5' exists, overwrite it
if (! f){
  file.remove(filePath)
  h5createFile(filePath)
}

file.create(rPath)
##### Fetch Raw Data #####
#
# fam2, modfam2, fullTable32, fullTable3, and DMT
# Fetch fullTable32
load("step0/result.RData")
fullTable32 <- fullTable32[!duplicated(fullTable32$VisitID),]
# fetch original fam2 and fullTable3
load("step1/result.RData")
# fetch modfam2
modfam2<-read.csv("step3/data_all.csv")
# fetch DMT
DMT<-read.table("tableDMT.csv")

###### fam related ###################
# create RData path
# file.create(famPath)
#First change fam2 colname "VisitId" to "VisitID"
fam2 <- rename(fam2, c("VisitId"="VisitID"))
# For modfam2, get rid of column "X", and change "RelativePain" column to "modRelativePain" to avoid confusion when merging modfam2 and fam2 
modfam2["X"] <- NULL
modfam2 <- rename(modfam2, c("RelativePain" = "modRelativePain"))

# For modfam2, only preserve 5 cols for analytic purpose: group1~3, relative-pain, enjoylife
modfam2<-modfam2[c("VisitID","group1", "group2", "group3", "modRelativePain","EnjoyLife")]

##### Core Related Prep#############
#
# Core:  
# "PrevEDSS" 
# "PrevEDSSRate"
# "PrevMSSS"
# "PrevMSSSRate"
# 
# EDSS Related from fullTable3, MSSS Rated from fullTable32
# 
# file.create(corePath)
coreNames <- c("PrevEDSS", "PrevEDSSRate", "PrevMSSS", "PrevMSSSRate")
# The senario where PrevEDSSRate is NA 
coreNANames <- c("PrevEDSS", "PrevEDSSRateNA", "PrevMSSS", "PrevMSSSRate")
core <- merge (fullTable3[c("VisitID", "ActualEDSS")], fullTable32[c("VisitID", "MSSS")])
core[, c(coreNames)] <- NA

#### MRI Related Prep
#
# MRI:
# "PrevSiena_PBVC"
# "PrevSiena_PBVCRate" 
# "PrevGMV"
# "PrevGMVRate"
# "PrevNew_T2_Lesions"
# 
# From fullTable32
# 
# file.create(MRIPath)
MRINames<- c("PrevSiena_PBVC", "PrevSiena_PBVCRate", "PrevGMV", "PrevGMVRate", "PrevNew_T2_Lesions")
MRI <- fullTable32[c("VisitID", "Siena_PBVC", "GM_Volume", "New_T2_Lesions")]
MRI[, MRINames] <- NA

#### Examination Related Prep
# Examination: 
# "AgeAtExam"
# "PrevOpticNeuritis"
# "PrevVDL"
# "PrevDiseaseDuration"
#
# From fullTable32
# file.create(examPath)
examNames <- c("AgeAtExam","PrevOpticNeuritis", "PrevVDL", "PrevDiseaseDuration")
exam <- fullTable32[c("VisitID", "AgeAtExam", "OpticNeuritis", "VitaminD_Level", "DiseaseDuration")]
exam[, c("PrevOpticNeuritis", "PrevVDL", "PrevDiseaseDuration")] <- NA

### Treatment Related Prep
# Treatment:
# "PrevRecTreatment"
# "PrevTreatmentMCount"
# "PrevTreatmentTCount"
# 
# From DMT
# DMT described all the treatment
# file.create(treatmentPath)
treatmentNames <- c("PrevRecTreatment", "PrevTreatmentMCount", "PrevTreatmentTCount")
treatment <- DMT[c("VisitID", "TreatmentMolecule", "TreatmentType")]


##### Target ########
#Add target column 'ModEDSS' (modified EDSS), denoting whether EDSS increased
#if ignore abs dEDSSS < 0.5, or decrease = > Class 0; Otherwise => Class 1
# file.create(targetPath)
targetNames <- c("ModEDSS")
target <- fullTable3[,c("VisitID","EPICID", "ExamDate", "ActualEDSS")]
target <- target[order(target$EPICID, target$ExamDate),]
target[, "ModEDSS"] <- NA
nvisits <- nrow(target)
for(i in 1:(nvisits-1)){
  dEDSS <- target[i+1, "ActualEDSS"] - target[i, "ActualEDSS"]
  if (target[i+1, "EPICID"] == target[i, "EPICID"] ){
    if (abs(dEDSS) <= .5){
      target[i+1, "ModEDSS"] <- 0
    } else {
      if (dEDSS< 0){
        target[i+1, "ModEDSS"] <- 0
      } else {
        target[i+1, "ModEDSS"] <- 1
      }
    }
  }
}


#### Merge and Iterate through the Visits and generate PrevXX and PrevXXRate #####
# "PrevEDSS", "PrevEDSSRate", "PrevMSSS", "PrevMSSSRate"
# "PrevSiena_PBVC", "PrevSiena_PBVCRate", "PrevGMV", "PrevGMVRate", "PrevNew_T2_Lesions"
# "PrevOpticNeuritis", "PrevVDL", "PrevDiseaseDuration"
# "PrevRecTreatment", "PrevTreatmentM", "PrevTreatmentT"

# MAKE SURE the VisitID in target always presents
temp <- merge(target, core)
temp <- merge(temp, exam)
temp <- merge(temp, MRI)
# Order temp for calculating PrevXXRate
temp <- temp[order(temp$EPICID, temp$ExamDate),]
# unique VisitID's in DMT
DMTVisitIDs <- unique(treatment[["VisitID"]])
nvisits <- nrow(temp)
EPICIDls <- unique(temp$EPICID)
# Initialize "PrevRecTreatment", "PrevTreatmentMCount", "PrevTreatmentTCount" in temp
temp[, c("PrevRecTreatment", "PrevTreatmentMCount", "PrevTreatmentTCount")] <- NA
# colnames that include "PrevXXRate"
PrevRateNames <- names(temp)[grep("Rate",names(temp))]
# Temporary cols XXRate for calculating PrevXXRate
RateNames <- sapply(PrevRateNames, function(x) substring(x, 5, nchar(x)), USE.NAMES = F)
temp[, RateNames]<-NA
# New colname PrevEDSSRateNA for alternative PrevEDSSRate
temp[, "PrevEDSSRateNA"] <- NA
for(i in 1:(nvisits-1)){
  dEDSS <- temp[i+1, "ActualEDSS"] - temp[i, "ActualEDSS"]
  dMSSS <- temp[i+1, "MSSS"] - temp[i, "MSSS"]
  dSPBVC <- temp[i+1, "Siena_PBVC"] - temp[i, "Siena_PBVC"]
  dGMV <- temp[i+1, "GM_Volume"] - temp[i, "GM_Volume"]
  dDay <-as.numeric(as.Date(temp[i+1,]$ExamDate) - as.Date(temp[i,]$ExamDate))
  dYear <- dDay/365
  newEPIC <- temp[i+1, "EPICID"]
  oldEPIC <- temp[i, "EPICID"]
  if (i == 1){
    temp[i, RateNames] <- 0
    temp[i+1, PrevRateNames] <- 0
    temp[i+1, "PrevEDSSRateNA"] <- NA
  }
  if (oldEPIC != newEPIC){
    temp[i+1, RateNames] <- 0
    temp[i+2, PrevRateNames] <- 0
    temp[i+2, "PrevEDSSRateNA"] <- NA
  }else{
    temp[i+1, "PrevEDSS"] <- temp[i, "ActualEDSS"]
    temp[i+1, "PrevMSSS"] <- temp[i, "MSSS"]
    temp[i+1, "PrevSiena_PBVC"] <- temp[i, "Siena_PBVC"]
    temp[i+1, "PrevGMV"] <- temp[i, "GM_Volume"]
    temp[i+1, "PrevNew_T2_Lesions"] <- temp [i, "New_T2_Lesions"]
    temp[i+1, "PrevOpticNeuritis"] <- temp[i, "OpticNeuritis"]
    temp[i+1, "PrevVDL"] <- temp[i, "VitaminD_Level"]
    temp[i+1, "PrevDiseaseDuration"] <- temp[i, "DiseaseDuration"]
    temp[i+1, "PrevRecTreatment"] <- temp[i, "VisitID"] %in% DMTVisitIDs
    temp[i+1, "PrevTreatmentTCount"] <- length(unique(treatment[treatment$VisitID == temp[i, "VisitID"], "TreatmentType"]))
    temp[i+1, "PrevTreatmentMCount"] <- length(unique(treatment[treatment$VisitID == temp[i, "VisitID"], "TreatmentMolecule"]))
    temp[i+1, "EDSSRate"] <- dEDSS/dYear
    temp[i+1, "MSSSRate"] <- dMSSS/dYear
    temp[i+1, "Siena_PBVCRate"] <- dSPBVC/dYear
    temp[i+1, "GMVRate"] <- dGMV/dYear
    temp[i+1, PrevRateNames] <- temp[i, RateNames]
    temp[i+1, "EDSSRateNA"] <- dEDSS/dYear
    temp[i+1, "PrevEDSSRateNA"] <- temp[i, "EDSSRateNA"]
  }
}

#test <-temp[,c("EPICID", "VisitID", "ActualEDSS", "PrevEDSS", "EDSSRate", "PrevEDSSRate", "PrevEDSSRateNA")]
####### Static ######
#
# "AgeOfOnset"
# "Overweight"
# "OnsetToYr5RelapseCount"
# 
#  From fullTable32
# file.create(staticPath)
staticNames <- c("AgeOfOnset", "Overweight", "OnsetToYr5RelapseCount")
static <- fullTable32[,c("VisitID","AgeOfOnset", "Overweight", "OnsetToYr5RelapseCount")]

####### Genetics ########
#
# "DRB1"
# "DRB1:EDSS"
# "DRB1:DiseaseDuration"
# "DRB1:AgeAtExam"
# 
# PrevEDSS, PrevDiseaseDuration and AgeAtExam from temp
# DRB1_1501 from fullTable32
# file.create(geneticsPath)
geneticsNames <- c("DRB1", "DRB1:EDSS", "DRB1:DiseaseDuration", "DRB1:AgeAtExam")
genetics <- merge(fullTable32[,c("VisitID", "DRB1_1501")], temp[,c("VisitID","PrevEDSS", "PrevDiseaseDuration", "AgeAtExam")])

genetics[, 'DRB1:EDSS'] <- genetics[,'DRB1_1501']*genetics[,'PrevEDSS']
genetics[, 'DRB1:DiseaseDuration'] <- genetics[,'DRB1_1501']*genetics[,'PrevDiseaseDuration']
genetics[, 'DRB1:AgeAtExam'] <- genetics[,'DRB1_1501']*genetics[,'AgeAtExam']
# rename "DRB1_1501" to "DRB1"
genetics <- rename(genetics, c("DRB1_1501"="DRB1"))

###### Merge and Split ######
# Remove those with ModEDSS NA
temp <- temp[!is.na(temp$ModEDSS),]
temp <- merge(temp, static)
temp <- merge(temp, genetics)
core <- temp[,c("VisitID", coreNames)]
coreNA <- temp[, c("VisitID", coreNANames)]
MRI <- temp[, c("VisitID", MRINames)]
exam <- temp[, c("VisitID", examNames)]
treatment <- temp[, c("VisitID", treatmentNames)]
genetics <- temp[, c("VisitID", geneticsNames)]
static <- temp[, c("VisitID", staticNames)]
target <- temp[,c("VisitID", targetNames)]

##### save grouped features #####
# save(fam2, modfam2, file=famPath)
# save(core, coreNA, file = corePath)
# save(MRI, file = MRIPath)
# save(exam, file = examPath)
# save(treatment, file = treatmentPath)
# save(target, file = targetPath)
# save(static, file = staticPath)
# save(genetics, file = geneticsPath)
save(fam2, modfam2, core, coreNA, MRI, exam, treatment, target, static, genetics, file = rPath)







