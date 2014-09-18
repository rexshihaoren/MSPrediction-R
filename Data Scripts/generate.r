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
# f <- h5createFile(filePath)
# If 'data/predData.h5' exists, overwrite it
# if (! f){
#   file.remove(filePath)
#   h5createFile(filePath)
# }
load(testPath)
load(gatherPath)

####combine<-function(dfs, imp= F, cut = F, rmcols = NULL, tgt)
#EDSS <- combine(dfs = list(target, core[,c("VisitID","PrevEDSS")]), imp = F, cut = F, rmcols = NULL, tgt = "ModEDSS")
bit <- 0
# # If bit == 1, EDSS&EDSSRate_Imp's performance > EDSS$EDSSRate
if (bit == 1){
	core <- coreNA
}
CorewFam <- combine(dfs = list(target, core, fam2))
CorewmodFam <- combine(dfs = list(target, core, modfam2))
CorewStatic <- combine(dfs = list(target, core, static))
CorewStaticwFam <- combine(dfs = list(target, core, static, fam2))
CorewStaticwTreatment <- combine(dfs = list(target, core, static, treatment))
CorewStaticwGenetics <- combine(dfs = list(target, core, static, genetics))

CorewStaticwMRIwoT2L <- combine(dfs = list(target, core, static, MRI), rmcols = c("PrevNew_T2_Lesions"))
CorewStaticwMRI_Imp <- combine(dfs = list(target, core, static, MRI), imp = T)
CorewStaticwMRI_Cut <- combine(dfs = list(target, core, static, MRI), cut = T)

CorewStaticwExamwoVDL <- combine(dfs = list(target, core, static, exam), rmcols = c("PrevVDL"))
CorewStaticwExam_Imp <- combine(dfs = list(target, core, static, exam), imp = T)
CorewStaticwExam_Cut <- combine(dfs = list(target, core, static, exam), cut = T)


CorewStaticwExamwMRIwoT2LwoVDL <- combine(dfs = list(target, core, static, exam, MRI), rmcols = c("PrevNew_T2_Lesions", "PrevVDL"))
CorewStaticwExamwMRIwoT2L_Cut <- combine(dfs = list(target, core, static, exam, MRI), cut = T, rmcols = c("PrevNew_T2_Lesions"))
CorewStaticwExamwMRIwoT2L_Imp <- combine(dfs = list(target, core, static, exam, MRI), imp = T, rmcols = c("PrevNew_T2_Lesions"))
CorewStaticwExamwMRIwoVDL_Cut <- combine(dfs = list(target, core, static, exam, MRI), cut = T, rmcols = c("PrevVDL"))
CorewStaticwExamwMRIwoVDL_Imp <- combine(dfs = list(target, core, static, exam, MRI), imp = T, rmcols = c("PrevVDL"))
CorewStaticwExamwMRI_Imp <- combine(dfs = list(target, core, static, exam, MRI), imp = T)

#h5write(Core_Imp, filePath, "Core_Imp")
h5write(CorewFam, filePath,"CorewFam")
h5write(CorewmodFam, filePath,"CorewmodFam")
h5write(CorewStatic, filePath,"CorewStatic")
h5write(CorewStaticwFam, filePath,"CorewStaticwFam")
h5write(CorewStaticwTreatment, filePath,"CorewStaticwTreatment")
h5write(CorewStaticwGenetics, filePath,"CorewStaticwGenetics")
h5write(CorewStaticwMRIwoT2L, filePath,"CorewStaticwMRIwoT2L")
h5write(CorewStaticwMRI_Imp, filePath,"CorewStaticwMRI_Imp")
h5write(CorewStaticwMRI_Cut, filePath,"CorewStaticwMRI_Cut")
h5write(CorewStaticwExamwoVDL, filePath,"CorewStaticwExamwoVDL")
h5write(CorewStaticwExam_Imp, filePath,"CorewStaticwExam_Imp")
h5write(CorewStaticwExam_Cut, filePath,"CorewStaticwExam_Cut")
h5write(CorewStaticwExamwMRIwoT2LwoVDL, filePath,"CorewStaticwExamwMRIwoT2LwoVDL")
h5write(CorewStaticwExamwMRIwoT2L_Cut, filePath,"CorewStaticwExamwMRIwoT2L_Cut")
h5write(CorewStaticwExamwMRIwoT2L_Imp, filePath,"CorewStaticwExamwMRIwoT2L_Imp")
h5write(CorewStaticwExamwMRIwoVDL_Cut, filePath,"CorewStaticwExamwMRIwoVDL_Cut")
h5write(CorewStaticwExamwMRIwoVDL_Imp , filePath,"CorewStaticwExamwMRIwoVDL_Imp ")
h5write(CorewStaticwExamwMRI_Imp, filePath,"CorewStaticwExamwMRI_Imp")
file.copy(filePath, filePathPython, overwrite = TRUE)




