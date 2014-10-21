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
# Core&Static-Overweight
# Core&Static_Imp
# Core&Static_Cut
#   -> Static = Static-OverWeight
# 
# Core&Static&fam
# Core&Static&Treatments
# 
# CorewStaticwGenetics_Imp
# Core$Static&Genetics_Cut
# 
# Core&Static&MRI_Imp
# 
# Core&Static&ExamwoVDL_Cut (for PrevOpticNeuritis)
# 	exam <- exam-PrevOpticNeuritis
# 	
# Core&Static&Examination-VDL
# Core&Static&Examination_Imp
# Core&Static&Examination_Cut
# 
# Core&Static&Examination&MRI-T2L-VDL_Imp
# Core&Static&Examination&MRI-T2L_Cut(VDL)_Imp
# Core&Static&Examination&MRI-T2L_Imp
# Core&Static&Examination&MRI-VDL_Cut(T2L)_Imp
# Core&Static&Examination&MRI-VDL_Imp
# Core&Static&Examination&MRI_Imp
#
# Set up
setwd("~/Dropbox/research/MSBioScreen/MSPrediction-R/Data Scripts")
source("test.r")
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
bit1 <- 0
# If bit1 == 1, EDSS&EDSSRate_Imp's performance > EDSS$EDSSRate
if (bit1 == 1){
	core <- coreNA
}
CorewFam_Cut <- combine(dfs = list(target, core, fam2), cut = T)
CorewmodFam_Cut <- combine(dfs = list(target, core, modfam2), cut = T)
CorewStaticwoOW <- combine(dfs = list(target, core, static), rmcols = c("Overweight"))
CorewStatic_Imp <- combine(dfs = list(target, core, static), imp = T)
CorewStatic_Cut <- combine(dfs = list(target, core, static), cut = T)

static <- static[, !names(static)%in%c("Overweight")]

CorewStaticwFam_Cut <- combine(dfs = list(target, core, static, fam2), cut = T)
CorewStaticwTreatment <- combine(dfs = list(target, core, static, treatment))

# Too many NA's for genetics
CorewStaticwGenetics_Imp <- combine(dfs = list(target, core, static, genetics), imp = T)
CorewStaticwGenetics_Cut <- combine(dfs = list(target, core, static, genetics), cut = T)

CorewStaticwMRI_Imp <- combine(dfs = list(target, core, static, MRI), imp = T)

# Cut "PrevOpticNeuritis"
CorewStaticwExamwoVDL_Cut <- combine(dfs = list(target, core, static, exam), rmcols = c("PrevVDL"), cut =T)
# Use exam without "PrevOpticNeuritis"
exam <- exam[, ! names(exam)%in%c("PrevOpticNeuritis")]


CorewStaticwExamwoVDL <- combine(dfs = list(target, core, static, exam), rmcols = c("PrevVDL"))
CorewStaticwExam_Imp <- combine(dfs = list(target, core, static, exam), imp = T)
CorewStaticwExam_Cut <- combine(dfs = list(target, core, static, exam), cut = T)


CorewStaticwExamwMRIwoT2LwoVDL_Imp <- combine(dfs = list(target, core, static, exam, MRI), imp = T, rmcols = c("PrevNew_T2_Lesions", "PrevVDL"))
CorewStaticwExamwMRIwoT2L_CutVDL_Imp <- combine(dfs = list(target, core, static, exam, MRI), cut = T, cutcols = c("PrevVDL"), imp = T, rmcols = c("PrevNew_T2_Lesions"))
CorewStaticwExamwMRIwoT2L_Imp <- combine(dfs = list(target, core, static, exam, MRI), imp = T, rmcols = c("PrevNew_T2_Lesions"))
CorewStaticwExamwMRIwoVDL_CutT2L_Imp <- combine(dfs = list(target, core, static, exam, MRI), cut = T, cutcols = c("PrevNew_T2_Lesions"),imp = T, rmcols = c("PrevVDL"))
CorewStaticwExamwMRIwoVDL_Imp <- combine(dfs = list(target, core, static, exam, MRI), imp = T, rmcols = c("PrevVDL"))
CorewStaticwExamwMRI_Imp <- combine(dfs = list(target, core, static, exam, MRI), imp = T)


#### H5 Save
#h5write(Core_Imp, filePath, "Core_Imp")
saveList <- c("CorewFam_Cut","CorewmodFam_Cut", "CorewStaticwoOW", "CorewStatic_Imp", "CorewStatic_Cut", "CorewStaticwFam_Cut", "CorewStaticwTreatment", "CorewStaticwGenetics_Imp", "CorewStaticwGenetics_Cut", "CorewStaticwMRI_Imp", "CorewStaticwExamwoVDL_Cut", "CorewStaticwExamwoVDL", "CorewStaticwExam_Imp", "CorewStaticwExam_Cut", "CorewStaticwExamwMRIwoT2LwoVDL_Imp", "CorewStaticwExamwMRIwoT2L_CutVDL_Imp", "CorewStaticwExamwMRIwoT2L_Imp", "CorewStaticwExamwMRIwoVDL_CutT2L_Imp", "CorewStaticwExamwMRIwoVDL_Imp", "CorewStaticwExamwMRI_Imp")
for (i in saveList){
	h5write(get(i), filePath, getDfName(i, newModEDSS))
}
file.copy(filePath, filePathPython, overwrite = TRUE)