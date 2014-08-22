###### Add patient specific (ps) to diagno #######

####### Load more features from fullTable32 #########
setwd("~/Dropbox/research/MSBioScreen/MSPrediction-R/Data Scripts")
source("helper.r")
load("step0/result.RData")
load(diagnoPath)
file.create(psPath)
####### merge patient specific (ps) to diagno ###
#psnames <- c('AgeOfOnset', 'Gender', 'DRB1_1501', 'OnsetToYr5RelapseCount')
psnames <- c('AgeOfOnset', 'Overweight')
ps <- fullTable32[c(psnames, "VisitID")]
ps <- merge(ps, diagnoidd)

# digitize gender
#ps[["Gender"]]<- ifelse(ps[["Gender"]] == "M", 1, 0)

#Overweight remove NA and digitize
#ps <- ps[!is.na(ps["Overweight"]), ]
#Overweight digitize
ps[["Overweight"]]<- ifelse(ps[["Overweight"]] == "Yes", 1, 0)
# Seperate those with EPICID, VisitID and ExamDate and those without
psidd <- ps
diagnops <-ps[, !(names(ps)%in%c("ExamDate","VisitID", "EPICID"))]

### Save #####
save(diagnops, psidd, file=psPath)
h5write(diagnops, filePath,"diagnops")
file.copy(filePath, filePathPython, overwrite = TRUE)
