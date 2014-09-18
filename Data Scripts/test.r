#### This program generate the testing datasets to evaluate PrevXXRate = 0 or NA for the first record of every patient in python####

## Set up
setwd("~/Dropbox/research/MSBioScreen/MSPrediction-R/Data Scripts")
source("help.r")
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
EDSSwEDSSRate_Imp <- combine(dfs = list(target, coreNA[, c("VisitID", "PrevEDSS", "PrevEDSSRateNA")]), imp = T)

CorewoRate <- combine(dfs = list(target, core), rmcols = c("PrevEDSSRate", "PrevMSSSRate"))
Core <- combine(dfs = list(target, core))
Core_Imp <- combine(dfs = list(target, coreNA), imp = T)
save(EDSS,EDSSwEDSSRate, EDSSwEDSSRate_Imp,CorewoRate,Core,Core_Imp, file = testPath)
h5write(EDSS, filePath,"EDSS")
h5write(EDSSwEDSSRate, filePath,"EDSSwEDSSRate")
h5write(EDSSwEDSSRate_Imp, filePath, "EDSSwEDSSRate_Imp")
h5write(CorewoRate, filePath, "CorewoRate")
h5write(Core, filePath, "Core")
h5write(Core_Imp, filePath, "Core_Imp")
file.copy(filePath, filePathPython, overwrite = TRUE)