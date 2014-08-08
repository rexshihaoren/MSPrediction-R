###### Binarize fam2, modfam2 "EnjoyLife" Coloumn, w/ possible Dataprocessing to force data value to 0 to 1########
# Create
# Family of modfam2
# Family of fam2
# merged (merged from modfam2, fam2, and fullTalbe3)
# #######################################################
# 
##### Set up ####################
setwd("~/Dropbox/research/MSBioScreen/MSPrediction-R/Data Scripts")
source("helper.r")
f <- h5createFile(filePath)
# If 'data/predData.h5' exists, overwrite it
if (! f){
  file.remove(filePath)
  h5createFile(filePath)
}
# create RData path
file.create(binarizePath)
#############################################
# get modified-fam2
modfam2<-read.csv("step3/data_all.csv")
# fetch original fam2
load("step1/result.RData")

####### Merge fam2, modfam2, fullTable3 ######
#First change fam2 colname "VisitId" to "VisitID"
fam2 <- rename(fam2, c("VisitId"="VisitID"))
# colnames(fam2)[colnames(fam2) == "VisitId"] <- "VisitID"

# For modfam2, get rid of column "X", and change "RelativePain" column to "modRelativePain" to avoid confusion when merging modfam2 and fam2 
modfam2["X"] <- NULL
modfam2 <- rename(modfam2, c("RelativePain" = "modRelativePain"))
merged <- merge(fam2, modfam2)
merged <- merge(merged, fullTable3)

# For modfam2, only preserve 5 cols for analytic purpose: group1~3, relative-pain, enjoylife
modfam2<-modfam2[c("group1", "group2", "group3", "modRelativePain","EnjoyLife")]
# For fam2, get rid of VisitID
fam2["VisitID"]<- NULL

# Binarize
modfam2_bin <-Binarize(modfam2, "EnjoyLife")
fam2_bin <- Binarize(fam2, "EnjoyLife")

modfam2_processing <- DataProcessing(modfam2_bin, "EnjoyLife")
fam2_processing <- DataProcessing(fam2_bin, "EnjoyLife")

# Save fam2, modfam2, fam2_bin, modfam2_bin, modfam2_processing, fam2_processing, and  merged in HDF5 format
save(fam2, modfam2, merged, fam2_bin, modfam2_bin, modfam2_processing, fam2_processing, fullTable3, file=binarizePath)
h5write(fam2, filePath,"fam2")
h5write(modfam2,filePath,"modfam2")
h5write(merged, filePath, "merged")
h5write(fam2_bin, filePath,"fam2_bin")
h5write(modfam2_bin, filePath,"modfam2_bin")
h5write(modfam2_processing, filePath,"modfam2_processing")
h5write(fam2_processing, filePath,"fam2_processing")
# Copy predData.h5 to python folder
file.copy(filePath, filePathPython, overwrite = TRUE)
