###### Impute dataset ######
#
#####Load diagnoap, apidd and DMwR package for kNNImpute#####
setwd("~/Dropbox/research/MSBioScreen/MSPrediction-R/Data Scripts")
source("helper.r")
load(apPath)
file.create(imputePath)
require(DMwR)
##### Use KNN to impute if there is NA in this column #####
diagnofinal <- knnImputation(as.data.frame(apply(diagnoap, c(1,2), as.numeric)), k =4)
### Save #####
save(diagnofinal, file=imputePath)
h5write(diagnofinal, filePath,"diagnofinal")
file.copy(filePath, filePathPython, overwrite = TRUE)