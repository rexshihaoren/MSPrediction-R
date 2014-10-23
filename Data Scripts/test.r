#### This program generate the testing datasets to evaluate PrevXXRate = 0 or NA for the first record of every patient in python####

## Set up
setwd("~/Dropbox/research/MSBioScreen/MSPrediction-R/Data Scripts")
source('gather.r')
# Create h5 path
f <- h5createFile(filePath)
# If 'data/predData.h5' exists, overwrite it
if (! f){
  file.remove(filePath)
  h5createFile(filePath)
}
load(gatherPath)
file.create(testPath)
####combine<-function(dfs, imp= F, cut = F, rmcols = NULL, tgt)
#EDSS <- merge(target, core[,c("VisitID","PrevEDSS")])
EDSS <- combine(dfs = list(target, core[,c("VisitID","PrevEDSS")]), imp = F, cut = F, rmcols = NULL, tgt = "ModEDSS")
EDSSwEDSSRate <- combine(dfs = list(target, core[, c("VisitID", "PrevEDSS", "PrevEDSSRate")]))
# Can't do this, knnImpute can't do 2 cols
#EDSSwEDSSRate_Imp <- combine(dfs = list(target, coreNA[, c("VisitID", "PrevEDSS", "PrevEDSSRateNA")]), imp = T)

CorewoRate <- combine(dfs = list(target, core), rmcols = c("PrevEDSSRate", "PrevMSSSRate"))
Core <- combine(dfs = list(target, core))
Core_Imp <- combine(dfs = list(target, coreNA), imp = T)
#save(EDSS,EDSSwEDSSRate, EDSSwEDSSRate_Imp,CorewoRate,Core,Core_Imp, file = testPath)
save(EDSS,EDSSwEDSSRate,CorewoRate,Core,Core_Imp, file = testPath)
testList <- c("EDSS", "EDSSwEDSSRate", "CorewoRate", "Core", "Core_Imp")
# This version put "new" in front of everything in PredData_Impr0-4.h5
# for (i in testList){
# 	h5write(get(i), filePath, getDfName(i, newModEDSS))
# }
# This version doesn't
for (i in testList){
	h5write(get(i), filePath, i)
}
file.copy(filePath, filePathPython, overwrite = TRUE)