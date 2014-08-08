#############################################################
################# EDSS and Disgnostic ##################
## This should follow binarize.r ran ###

source("helper.r")
# merged <- h5read(filePath, "merged")
# fam2<-h5read(filePath, "fam2")
# modfam2 <- h5read (filePath, "modfam2")
load(binarizePath)
file.create(diagnoPath)
##################Calculate EDSS Rate + PrevEDSS + PrevEDSSRate cols###########
merged_updated <- merged[order(merged$EPICID, merged$ExamDate),]
##Treatment##
# DMT described all the treatment
DMT<-read.table("tableDMT.csv")
#DMTex <-DMT[,c("VisitID","START", "END", "TreatmentID")]
# List of all VisitIDs
DMTVisitIDs <- unique(DMT[["VisitID"]])
# Add Empty EDSSRate, PrevEDSS, PrevEDSSRate, RecTreatment
merged_updated[, "EDSSRate"] <- NA
merged_updated[, "PrevEDSS"] <- NA
merged_updated[, "PrevEDSSRate"] <- NA
merged_updated[, "RecTreatment"] <- NA

nvisits <- nrow(merged_updated)
for(i in 1:(nvisits-1)){
  dEDSS <- merged_updated[i+1, "ActualEDSS"] - merged_updated[i, "ActualEDSS"]
  dDay <-as.numeric(as.Date(merged_updated[i+1,]$ExamDate) - as.Date(merged_updated[i,]$ExamDate))
  dYear <- dDay/365
  if (merged_updated[i+1, "EPICID"] == merged_updated[i, "EPICID"] ){
    merged_updated[i+1, "EDSSRate"] <- dEDSS/dYear
    merged_updated[i+1, "PrevEDSS"] <- merged_updated[i, "ActualEDSS"]
    merged_updated[i+1, "PrevEDSSRate"] <- merged_updated[i, "EDSSRate"]
  
  }
  merged_updated[i, "RecTreatment"] <- merged_updated[i, "VisitID"] %in% DMTVisitIDs
}

######### Add one column 'ModEDSS' (modified EDSS), denoting whether EDSS increased ########

#if ignore abs dEDSSS < 0.5, or decrease = > Class 0; Otherwise => Class 1
# Add Imprecision col 
merged_updated[, "Imprecision"] <- NA

merged_updated[, "ModEDSS"] <- NA
for(i in 1:(nvisits-1)){
  dEDSS <- merged_updated[i+1, "ActualEDSS"] - merged_updated[i, "ActualEDSS"]
  dDay <- as.numeric(as.Date(merged_updated[i+1,]$ExamDate) - as.Date(merged_updated[i,]$ExamDate))
  dYear <- dDay/365
  if (merged_updated[i+1, "EPICID"] == merged_updated[i, "EPICID"] ){
    if (abs(dEDSS) <= .5){
      merged_updated[i+1, "Imprecision"] <- 1
      merged_updated[i+1, "ModEDSS"] <- 0
    } else {
      merged_updated[i+1, "Imprecision"] <- 0
      if (dEDSS< 0){
        merged_updated[i+1, "ModEDSS"] <- 0
      } else {
        merged_updated[i+1, "ModEDSS"] <- 1
      }
    }
  }
}

# DatePrep to use QOL(n) + EDSSRate(n-1) + EDSS(n-1) to predict ModEDSS: Diagnostic, n is exam date #
diagnoColName <- unique(c("EPICID", "ExamDate", "PrevEDSS","ActualEDSS", "PrevEDSSRate", "EDSSRate", "ModEDSS", "Imprecision", "RecTreatment", colnames(fam2)))
diagno <- merged_updated[diagnoColName] 

#### Question:  Inclusion of mofam2/fam2 in training data & speed tradeoff #####

# Provide alternatives: if for every patient, we do tranformation from fam2 to modfam2, then we only include modfam2 in traning

diagnoeffColName <- unique(c("EPICID", "ExamDate", "PrevEDSS","ActualEDSS", "PrevEDSSRate", "EDSSRate", "ModEDSS", "Imprecision", "RecTreatment", colnames(modfam2)))
diagnoeff <- merged_updated[diagnoeffColName] 

### In real life, without the Physician, we wouldn't know ActualEDSS (therefore Imprecision) or EDSSRate, we only know PrevEDSSRate, PrevEDSS
# diagno and diagnoeff w/ date
# EPICID is useless for static prediction as well
drop <- c('EPICID', 'ExamDate', 'ActualEDSS','EDSSRate', 'Imprecision')
diagnostatic <- diagno
diagnostatic <- diagnostatic[,!(names(diagnostatic) %in% drop)]
diagnoeffstatic <- diagnoeff
diagnoeffstatic <- diagnoeffstatic[,!(names(diagnoeffstatic) %in% drop)]

### For diagnoeffstatic, the dataset primarily used in this analysis, we remove patient's initial visit, because for now we can't predict without PrevEDSS;
diagnoeffstatic<-diagnoeffstatic[- which( is.na(diagnoeffstatic['PrevEDSS'])),]

### For the patient record with PrevEDSSRate NA, we assume it's 0;
diagnoeffstatic['PrevEDSSRate'][is.na(diagnoeffstatic['PrevEDSSRate']),] <- 0

targetColList <- c("group1","group2", "group3","modRelativePain", "EnjoyLife" )
diagnorate <- diagnoeff
for (col in targetColList){
  diagnorate <- calcRate(diagnorate, col, "ExamDate", "EPICID")
}
diagnorate<-diagnorate[- which( is.na(diagnorate['PrevEDSS'])),]
diagnorate['PrevEDSSRate'][is.na(diagnorate['PrevEDSSRate']),] <- 0
diagnorateeffstatic <- diagnorate

diagnorateeffstatic <- diagnorateeffstatic[,!(names(diagnorateeffstatic) %in% drop)]
diagnorateeffstatic <- diagnorateeffstatic[,!(names(diagnorateeffstatic) %in% targetColList)]

# same for diagnostatic
diagnostatic<-diagnostatic[- which( is.na(diagnostatic['PrevEDSS'])),]
diagnostatic['PrevEDSSRate'][is.na(diagnostatic['PrevEDSSRate']),] <- 0

get <- c('PrevEDSS','ModEDSS','PrevEDSSRate')
diagnotryget <- diagnostatic[get]

##### save to diagno.RData ###
save(merged_updated, diagno, diagnostatic, diagnorateeffstatic, diagnoeff, diagnotryget, diagnoeffstatic, file=diagnoPath)
###### h5 save #######
h5write(diagnorateeffstatic, filePath,"diagnorateeffstatic")
h5write(merged_updated, filePath,"merged_updated")
h5write(diagno, filePath,"diagno")
h5write(diagnostatic, filePath,"diagnostatic")
h5write(diagnoeff, filePath,"diagnoeff")
h5write(diagnoeffstatic, filePath,"diagnoeffstatic")
h5write(diagnotryget, filePath,"diagnotryget")
file.copy(filePath, filePathPython, overwrite = TRUE)

# ####### Add more features from fullTable32 #########
# load("step0/result.RData")

# # patient specific
# pc <- c('AgeOfOnset', 'Gender', 'DRB1_1501', 'OnsetToYr5RelapseCount')
# # pc <- fullTable32[c(features, "VisitID")]
# # previous year parameters (+ Siena_PBVC gradient + meds)
# pp <- c('DiseaseDuration','Siena_PBVC', 'New_T2_Lesions')

