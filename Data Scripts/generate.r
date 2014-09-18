#### This program generate everything from gather.r into diserable dataframes ######
#
# DATASETS:
# EDSS
# EDSS&EDSSRate (PrevXXRate = 0)
# EDSS&EDSSRate_Imp (PrevXXRate = NA)
# Core-Rate
# Core
# Core_Imp
# Pick one for core after testing.
# (For the following just use PrevXXRate = 0)
# Core&fam
# Core&modfam
# Core&Static&fam
# Core&Static&Treatments
# Core$Static&Genetics
# Core&Static&MRI-T2L
# Core&Static&MRI_Imp
# Core&Static&MRI_Cut
# Core&Static&Examination-VDL
# Core&Static&Examination_Imp
# Core&Static&Examination_Cut
# Core&Static&Examination&MRI-T2L-VDL
# Core&Static&Examination&MRI-T2L_Cut
# Core&Static&Examination&MRI-T2L_Imp
# Core&Static&Examination&MRI-VDL_Cut
# Core&Static&Examination&MRI-VDL_Imp
# Core&Static&Examination&MRI_Imp
#
# Set up
setwd("~/Dropbox/research/MSBioScreen/MSPrediction-R/Data Scripts")
source("help.r")
# Create h5 path
f <- h5createFile(filePath)
# If 'data/predData.h5' exists, overwrite it
if (! f){
  file.remove(filePath)
  h5createFile(filePath)
}

load(rPath)

####combine<-function(dfs, imp= F, cut = F, rmcols = NULL, tgt)
#EDSS <- merge(target, core[,c("VisitID","PrevEDSS")])
EDSS <- combine(dfs = list(target, core[,c("VisitID","PrevEDSS")]), imp = F, cut = F, rmcols = NULL, tgt = "ModEDSS")
EDSSwEDSSRate <- combine(dfs = list(target, core[, c("VisitID", "PrevEDSS", "PrevEDSSRate")]))
EDSSwEDSSRate_Imp <- combine(dfs = list(target, coreNA[, c("VisitID", "PrevEDSS", "PrevEDSSRateNA")]), imp = T)

CorewoRate <- combine(dfs = list(target, core), rmcols = c("PrevEDSSRate", "PrevMSSSRate"))
Core <- combine(dfs = list(target, core))
Core_Imp <- combine(dfs = list(target, coreNA), imp = T)

h5write(EDSS, filePath,"EDSS")
h5write(EDSSwEDSSRate, filePath,"EDSSwEDSSRate")
h5write(EDSSwEDSSRate_Imp, filePath, "EDSSwEDSSRate_Imp")
h5write(CorewoRate, filePath, "CorewoRate")
h5write(Core, filePath, "Core")
h5write(Core_Imp, filePath, "Core_Imp")

file.copy(filePath, filePathPython, overwrite = TRUE)



# Core_Imp <- merge(target, )
# bit <- 0
# # If bit == 1, EDSS&EDSSRate_Imp's performance > EDSS$EDSSRate
# if (bit == 1){
# 	core <- coreNA
# }
# CorewFam <- merge(Core, fam2)
# CorewmodFam <- merge(Core, modfam2)
# CorewStatic <- merge(Core, static)
# CorewStaticwFam <- merge(CorewFam, static)
# CorewStaticwTreatment <- merge(CorewStatic, treatment)
# CorewStaticwGenetics <- merge(CorewStatic, genetics)
# CorewStaticwMRI <- merge(CorewStatic, MRI[, c()])




