###### Add patient specific's to diagno #######

####### Add more features from fullTable32 #########
setwd("~/Dropbox/research/MSBioScreen/MSPrediction-R/Data Scripts")
source("helper.r")
load("step0/result.RData")
load(diagnoPath)
file.create(psPath)
####### Addpatient specific to merged_updated
psnames <- c('AgeOfOnset', 'Gender', 'DRB1_1501', 'OnsetToYr5RelapseCount')
ps <- fullTable32[c(psnames, "VisitID")]
ps <- merge(ps, diagnoidd)

# Seperate those with EPICID, VisitID and ExamDate and those without
psidd <- ps
ps <-ps[, !(names(ps)%in%c("ExamDate","VisitID", "EPICID"))]

### Save
save(ps, psidd, file=psPath)
h5write(ps, filePath,"ps")
file.copy(filePath, filePathPython, overwrite = TRUE)
# previous year parameters (+ Siena_PBVC gradient + meds)
#ppnames <- c('DiseaseDuration','Siena_PBVC', 'New_T2_Lesions')