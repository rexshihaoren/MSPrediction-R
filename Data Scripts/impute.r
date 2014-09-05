###### Impute dataset ######
#
#####Load diagnoap, apidd and DMwR package for kNNImpute#####
setwd("~/Dropbox/research/MSBioScreen/MSPrediction-R/Data Scripts")
source("helper.r")
load(apPath)
file.create(imputePath)
##### Use KNN to impute everything except ModEDSS column #####
diagnofinal <- KnnImputeXY(diagnoap, c("ModEDSS"))
diagno0final <- KnnImputeXY(diagno0ap,c("ModEDSS"))
###### If only complete columns #####
diagnonoNA <- diagnoap[,colSums(is.na(diagno0ap))==0]
### Save #####
save(diagnofinal, diagno0final, diagnonoNA, file=imputePath)
h5write(diagnofinal, filePath,"diagnofinal")
h5write(diagno0final, filePath,"diagno0final")
h5write(diagnonoNA, filePath, "diagnonoNA")
file.copy(filePath, filePathPython, overwrite = TRUE)