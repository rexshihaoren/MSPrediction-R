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
# Add Empty EDSSRate, PrevEDSS, PrevEDSSRate, RecTreatment (Recieved treatment)
merged_updated[, "EDSSRate"] <- NA
merged_updated[, "PrevEDSS"] <- NA
merged_updated[, "PrevEDSSRate"] <- NA
merged_updated[, "RecTreatment"] <- NA

nvisits <- nrow(merged_updated)
EPICIDls <- unique(merged_updated$EPICID)
for(i in 1:(nvisits-1)){
  dEDSS <- merged_updated[i+1, "ActualEDSS"] - merged_updated[i, "ActualEDSS"]
  dDay <-as.numeric(as.Date(merged_updated[i+1,]$ExamDate) - as.Date(merged_updated[i,]$ExamDate))
  dYear <- dDay/365
  # oldEPIC <- merged_updated[i, "EPICID"]
  # newEPIC <- merged_updated[i+1, "EPICID"]
  # if (i == 1){
  #   merged_updated[i, "EDSSRate"] <- 0
  #   merged_updated[i, "PrevEDSSRate"] <- 0
  # }
  # if (oldEPIC != newEPIC){
  #   merged_updated[i+1, "EDSSRate"] <- 0
  #   merged_updated[i+1, "PrevEDSSRate"] <- 0
  # }else{
  #   merged_updated[i+1, "EDSSRate"] <- dEDSS/dYear
  #   merged_updated[i+1, "PrevEDSS"] <- merged_updated[i, "ActualEDSS"]
  #   merged_updated[i+1, "PrevEDSSRate"] <- merged_updated[i, "EDSSRate"]
  # }
  if (merged_updated[i, "EPICID"] == merged_updated[i+1, "EPICID"]){
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
# Remove those with ModEDSS NA
merged_updated <- merged_updated[!is.na(merged_updated$ModEDSS),]
# DatePrep to use QOL(n) + EDSSRate(n-1) + EDSS(n-1) to predict ModEDSS: Diagnostic, n is exam date #
diagnoColName <- unique(c("VisitID","EPICID", "ExamDate", "PrevEDSS","ActualEDSS", "PrevEDSSRate", "EDSSRate", "ModEDSS", "Imprecision", "RecTreatment", colnames(fam2)))
diagno <- merged_updated[diagnoColName] 

#### Question:  Inclusion of mofam2/fam2 in training data & speed tradeoff #####

# Provide alternatives: if for every patient, we do tranformation from fam2 to modfam2, then we only include modfam2 in traning

diagnomodColName <- unique(c("VisitID","EPICID", "ExamDate", "PrevEDSS","ActualEDSS", "PrevEDSSRate", "EDSSRate", "ModEDSS", "Imprecision", "RecTreatment", colnames(modfam2)))
diagnomod <- merged_updated[diagnomodColName]

### In real life, without the Physician, we wouldn't know ActualEDSS (therefore Imprecision) or EDSSRate, we only know PrevEDSSRate, PrevEDSS
# EPICID is useless for prediction as well
drop <- c('ActualEDSS','EDSSRate', 'Imprecision')
diagno <- diagno[,!(names(diagno) %in% drop)]
diagnomod <- diagnomod[,!(names(diagnomod) %in% drop)]

#### For diagnomod, try calculate rate for everything in the targetColList
targetColList <- c("group1","group2", "group3","modRelativePain", "EnjoyLife" )
diagnomodrate <- diagnomod
for (col in targetColList){
  diagnomodrate <- calcRate(diagnomodrate, col, "ExamDate", "EPICID")
}

# ### For diagnomod, we remove patient's initial visit, because for now we can't predict without PrevEDSS;For the patient record with PrevEDSSRate NA, we assume it's 0;
# diagnomod<-diagnomod[- which( is.na(diagnomod['PrevEDSS'])),]
# diagnomod['PrevEDSSRate'][is.na(diagnomod['PrevEDSSRate']),] <- 0
# # same for diagno with fam2 only
# diagno<-diagno[- which( is.na(diagno['PrevEDSS'])),]
# diagno['PrevEDSSRate'][is.na(diagno['PrevEDSSRate']),] <- 0

# ### For diagnorate, we remove patient's initial visit, because for now we can't predict without PrevEDSS;For the patient record with PrevEDSSRate NA, we assume it's 0;
# diagnomodrate<-diagnomodrate[- which( is.na(diagnomodrate['PrevEDSS'])),]
# diagnomodrate['PrevEDSSRate'][is.na(diagnomodrate['PrevEDSSRate']),] <- 0



# Seperate those with EPICID, VisitID and ExamDate and those without
diagnoidd <- diagno
diagno <-diagno[, !(names(diagno)%in%c("ExamDate","VisitID", "EPICID"))]
diagnomodidd <- diagnomod
diagnomod <-diagnomod[, !(names(diagnomod)%in%c("ExamDate","VisitID", "EPICID"))]
diagnomodrateidd <- diagnomodrate
diagnomodrate <-diagnomodrate[, !(names(diagnomodrate)%in%c("ExamDate","VisitID", "EPICID"))]

# Simplified version with only 'PrevEDSS','ModEDSS','PrevEDSSRate'
get <- c('PrevEDSS','ModEDSS','PrevEDSSRate')
diagnosim <- diagno[get]

##### save to diagno.RData ###
save(merged_updated, diagno, diagnoidd, diagnomod, diagnomodidd, diagnomodrate, diagnomodrateidd, diagnosim, file=diagnoPath)
###### h5 save #######
h5write(merged_updated, filePath,"merged_updated")
h5write(diagno, filePath,"diagno")
h5write(diagnoidd, filePath,"diagnoidd")
h5write(diagnomod, filePath,"diagnomod")
h5write(diagnomodidd, filePath,"diagnomodidd")
h5write(diagnomodrate, filePath,"diagnomodrate")
h5write(diagnomodrateidd, filePath,"diagnomodrateidd")
h5write(diagnosim, filePath,"diagnosim")
file.copy(filePath, filePathPython, overwrite = TRUE)
