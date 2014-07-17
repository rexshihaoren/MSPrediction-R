setwd("~/Dropbox/research/MSBioScreen/MSPrediction-R/Data Scripts")
# get modified-fam2
require(ggplot2)
require(rhdf5)
require(MASS)
require(fitdistrplus)
require(plyr)
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


Binarize <- function(df, target){
  # to Binarize a target column of a datafram
  med <- median(df[[target]])
  bin <- df
  bin[[target]] <- ifelse(bin[[target]] <= med, 0,1)
  return(bin)
}

modfam2_bin <-Binarize(modfam2, "EnjoyLife")
fam2_bin <- Binarize(fam2, "EnjoyLife")



DataProcessing <- function(df, target = ""){
  # Processing a data frame to make everything (0, 1] except the target col
  if (target == ""){
    loc <- ncol(df)+1
  } else{
    loc <- grep(target, colnames(df))
  }
  processing <- df
  processing <- apply(processing, 2, function(x) (x-min(x))/(max(x)-min(x)))
  processing[,-loc] <- apply(processing[,-loc], c(1,2), function(x) if (x == 0) x+1e-12 else x)

  as.data.frame(processing)
}
modfam2_processing <- DataProcessing(modfam2_bin, "EnjoyLife")
fam2_processing <- DataProcessing(fam2_bin, "EnjoyLife")

# Save fam2, modfam2, fam2_bin, modfam2_bin, modfam2_processing, fam2_processing, and  merged in HDF5 format
filePath <- 'data/predData.h5'
f <- h5createFile(filePath)
# If 'data/predData.h5' exists, overwrite it
if (! f){
  file.remove(filePath)
  h5createFile(filePath)
}
h5write(fam2, filePath,"fam2")
h5write(modfam2,filePath,"modfam2")
h5write(merged, filePath, "merged")
h5write(fam2_bin, filePath,"fam2_bin")
h5write(modfam2_bin, filePath,"modfam2_bin")
h5write(modfam2_processing, filePath,"modfam2_processing")
h5write(fam2_processing, filePath,"fam2_processing")
# Copy predData.h5 to python folder
filePathPython <- '../../MSPrediction-Python/data/'
file.copy(filePath, filePathPython, overwrite = TRUE)


###### Functions for Statistical Analysis

gendist<-function(somedf, plotfunc, target, filename){
  # Plotting distribution (histogram/density) given a dataframe and a target column, and a filename
  # Args:
  #    somdf: dataframe
  #    plotfunc: geom_histogram or geom_density
  #    target: target column
  #    filename: filename given to the hitogram to generate
  #
  # Returns:
  #   hitogram on target column
  pfname = deparse(substitute(plotfunc))
  filepath = paste("plots/",paste(filename, pfname, sep = "_"), ".pdf",sep="")
  somedf_noNA <- somedf[!is.na(somedf[target]),]
  ggplot(somedf_noNA) + plotfunc(aes_string(x=target))
  ggsave(file=filepath)
}


generateCPDF<-function(somedf, plotfunc, target){
  # Plot Conditional PDF 
  dfname = deparse(substitute(somedf))
  pfname = deparse(substitute(plotfunc))
  for (i in colnames(somedf)){
    if (i != target){
      if (pfname == "geom_density"){
        ggplot(somedf) + plotfunc(aes_string(x=i, group = target, fill= target), alpha = 0.8)
      } else {
        ggplot(somedf) + plotfunc(aes_string(x=i, group = target, fill = target), position = "dodge")
      }
      fpath = paste("plots/",paste(dfname, "cpdf",i, sep = "_"), ".pdf", sep="")
      ggsave(file = fpath)
    }
  }
}


fitCPDF<- function(df, xname, yname, f, plotfunc, method, start = NULL){
  # Fitting Conditional pdf on top of histogram, and save that plot.
  #
  # Args: 
  #   df: dataset
  #   xname: target col name
  #   yname: condition col name
  #   f: string that indicated the density function put on top of the histogram
  #   plotfunc: original data plot, geom_hitogram or geom_density
  #   method: the character string coding for the fitting method
  #
  # Returns:
  #   Model conditional pdf
  dfname = deparse(substitute(df))
  pfname = deparse(substitute(plotfunc))
  xcol <- df[[xname]]
  ycol <- df[[yname]]
  xrange <- unique(xcol)
  yrange <- unique(ycol)
  xlen <- length(xrange)
  ylen <- length(yrange)
  sf <- list()
  for (i in range(1,ylen)){
    condData <- xcol[ycol == yrange[i]]
    print(condData)
    params <- c(fitdist(condData, f, method, start = start)$estimate)
    # This step is IMPORTANT
    dfunc = match.fun(paste('d', f, sep = ''))
    if(pfname == "geom_histogram"){
      sf[[i]] = stat_function(fun = dfunc, n = xlen, args = params, lty = "dashed")
    }else{
      sf[[i]] = stat_function(fun = dfunc, args = params, lty = "dashed")
    }
  }
  if (pfname == "geom_histogram"){
    ggplot(df) + 
    plotfunc(aes_string(y="..density.. * 0.1", x=xname, group = yname, fill=yname), binwidth=0.1, position="dodge", width = 2) + 
    sf
  } else {
    if (f == "beta"){
      ggplot(df) + 
        plotfunc(aes_string(x=xname, group = yname, group = yname, fill=yname), alpha = .8) + 
        sf + scale_x_continuous(limits = c(0,1))
    } else {
      ggplot(df) + 
        plotfunc(aes_string(x=xname, group = yname, group = yname, fill=yname), alpha = .8) + 
        sf
    }
  }
 # Save plots
 fpath = paste("plots/",paste(dfname, "fitcpdf", xname,"on",yname, f, sep = "_"), ".pdf", sep="")
 ggsave(file = fpath)
}


# # List of density functions
# densfunlist <- c("norm", "exp", "gamma", "nbinom", "geom", "beta", "logis", "nbinorm")
# # TODO??
# PickBestFit <- function(x, method){
#   # Fit the best density functions out of densfunlist
#   # Args:
#   #   x: a numeric vector
#   #   method: the character string coding for the fitting method
#   # Returns:
#   #   fitdistr obj with the best performance based on estimated standard error mean
#   errlist <- list()
#   for (f in densfunlist){
#     obj <- fitdist(x, f)
#     stderror <- obj$sd[1]
#     errlist[stderror] <- obj
#   }
# }


### Examples #########

gendist(modfam2, geom_histogram, "EnjoyLife", "modfam2")
gendist(fam2, geom_histogram, "EnjoyLife", "fam2")
# Plot PDF after binarize
gendist(modfam2_bin, geom_histogram,"EnjoyLife", "modfam2_bin")
gendist(fam2_bin, geom_histogram,"EnjoyLife", "fam2_bin")

gendist(fullTable3, geom_histogram, "ActualEDSS", "fullTable3")
gendist(fullTable3, geom_density, "ActualEDSS", "fullTable3")
generateCPDF(modfam2_bin,geom_density, "EnjoyLife")
generateCPDF(fam2_bin, geom_histogram, "EnjoyLife")
# Plot Fitted Histogram for fam2 with 'norm' normal distribution
for (cname in colnames(fam2_bin)){
  if (cname != "EnjoyLife"){
    fitCPDF(fam2_bin, cname, "EnjoyLife", "norm", geom_histogram, "mle")
  }
}

# Plot Fitted density for modfam2 with 'norm' normal distribution
for (cname in colnames(modfam2_bin)){
  if (cname != "EnjoyLife"){
    print(cname)
    fitCPDF(modfam2_bin, cname, "EnjoyLife", "gamma", geom_density, "mme")
  }
}


# Plot Fitted density for modfam2 with 'norm' normal distribution
for (cname in colnames(modfam2_processing)){
  if (cname != "EnjoyLife"){
    print(cname)
    fitCPDF(modfam2_processing, cname, "EnjoyLife", "beta", geom_density, "mme")
  }
}



for (cname in colnames(fam2_bin)){
  if (cname != "EnjoyLife"){
    print(cname)
    fitCPDF(fam2_bin, cname, "EnjoyLife", "pois", geom_histogram, "mme")
  }
}

for (cname in colnames(fam2_bin)){
  if (cname != "EnjoyLife"){
    print(cname)
    fitCPDF(fam2_bin, cname, "EnjoyLife", "nbinom", geom_histogram, "mme")
  }
}

#############################################################
################# EDSS and Disgnostic ##################

##################Calculate EDSS Rate + PrevEDSS + PrevEDSSRate cols###########
merged_updated <- merged[order(merged$EPICID, merged$ExamDate),]
##Treatment##
# DMT described all the treatment
DMT<-read.table("tableDMT.csv")
#DMTex <-DMT[,c("VisitID","START", "END", "TreatmentID")]
# List of all VisitIDs
DMTVisitIDs <- unique(DMT[["VisitID"]])
# Add Empty EDSSRate, PrevEDSS, PrevEDSSRate, RecTreatment
merged_updated[, "EDSSRate"] <- NA
merged_updated[, "PrevEDSS"] <- NA
merged_updated[, "PrevEDSSRate"] <- NA
merged_updated[, "RecTreatment"] <- NA

nvisits <- nrow(merged_updated)
for(i in 1:(nvisits-1)){
  dEDSS <- merged_updated[i+1, "ActualEDSS"] - merged_updated[i, "ActualEDSS"]
  dDay <- as.numeric(as.Date(merged_updated[i+1,]$ExamDate) - as.Date(merged_updated[i,]$ExamDate))
  dYear <- dDay/365
  if (merged_updated[i+1, "EPICID"] == merged_updated[i, "EPICID"] ){
    merged_updated[i+1, "EDSSRate"] <- dEDSS/dYear
    merged_updated[i+1, "PrevEDSS"] <- merged_updated[i, "ActualEDSS"]
    merged_updated[i+1, "PrevEDSSRate"] <- merged_updated[i, "EDSSRate"]
  
  }
  merged_updated[i, "RecTreatment"] <- merged_updated[i, "VisitID"] %in% DMTVisitIDs
}

######### Add one column 'ModEDSS' (modified EDSS), denoting whether EDSS increased ########

#if ignore abs dEDSSS < 0.5, or decrease = > Class 0; Otherwise => Class 1
# Add Imprecision col 
merged_updated[, "Imprecision"] <- NA

merged_updated[, "ModEDSS"] <- NA
for(i in 1:(nvisits-1)){
  dEDSS <- merged_updated[i+1, "ActualEDSS"] - merged_updated[i, "ActualEDSS"]
  dDay <- as.numeric(as.Date(merged_updated[i+1,]$ExamDate) - as.Date(merged_updated[i,]$ExamDate))
  dYear <- dDay/365
  if (merged_updated[i+1, "EPICID"] == merged_updated[i, "EPICID"] ){
    if (abs(dEDSS) <= .5){
      merged_updated[i+1, "Imprecision"] <- 1
      merged_updated[i+1, "ModEDSS"] <- 0
    } else {
      merged_updated[i+1, "Imprecision"] <- 0
      if (dEDSS< 0){
        merged_updated[i+1, "ModEDSS"] <- 0
      } else {
        merged_updated[i+1, "ModEDSS"] <- 1
      }
    }
  }
}


# DatePrep to use QOL(n) + EDSSRate(n-1) + EDSS(n-1) to predict ModEDSS: Diagnostic, n is exam date #
diagnoColName <- unique(c("EPICID", "ExamDate", "PrevEDSS","ActualEDSS", "PrevEDSSRate", "EDSSRate", "ModEDSS", "Imprecision", "RecTreatment", colnames(fam2), colnames(modfam2)))
diagno <- merged_updated[diagnoColName] 

#### Question:  Inclusion of mofam2/fam2 in training data & speed tradeoff #####

# Provide alternatives: if for every patient, we do tranformation from fam2 to modfam2, then we only include modfam2 in traning

diagnoeffColName <- unique(c("EPICID", "ExamDate", "PrevEDSS","ActualEDSS", "PrevEDSSRate", "EDSSRate", "ModEDSS", "Imprecision", "RecTreatment", colnames(modfam2)))
diagnoeff <- merged_updated[diagnoeffColName] 

### In real life, without the Physician, we wouldn't know ActualEDSS (therefore Imprecision) or EDSSRate, we only know PrevEDSSRate, PrevEDSS
# diagno and diagnoeff w/ date
# EPICID is useless for static prediction as well
drop <- c('EPICID', 'ExamDate', 'ActualEDSS','EDSSRate', 'Imprecision')
diagnostatic <- diagno
diagnostatic <- diagnostatic[,!(names(diagnostatic) %in% drop)]
diagnoeffstatic <- diagnoeff
diagnoeffstatic <- diagnoeffstatic[,!(names(diagnoeffstatic) %in% drop)]

### For diagnoeffstatic, the dataset primarily used in this analysis, we remove patient's initial visit, because for now we can't predict without PrevEDSS;
diagnoeffstatic<-diagnoeffstatic[- which( is.na(diagnoeffstatic['PrevEDSS'])),]

### For the patient record with PrevEDSSRate NA, we assume it's 0;
diagnoeffstatic['PrevEDSSRate'][is.na(diagnoeffstatic['PrevEDSSRate']),] <- 0


# Some Ploting for merged_updated
gendist(merged_updated, geom_histogram, "EDSSRate", "merged_updated")
gendist(merged_updated, geom_density, "EDSSRate", "merged_updated")
gendist(merged_updated, geom_histogram, "ModEDSS", "merged_updated")

# Some plotting for diagnostatic and diagnoefstatic
generateCPDF(diagnostatic, geom_histogram, "ModEDSS")
generateCPDF(diagnoeffstatic, geom_histogram, "ModEDSS")
generateCPDF(diagnostatic, geom_density, "ModEDSS")
generateCPDF(diagnoeffstatic, geom_density, "ModEDSS")

### Testing add column rate of everything ####

calcRate<-function(df, colName, tColName, index){
  # Add a rate column of a target column within index, an identity
  #
  # Args: 
  #   df: dataset
  #   colName: target col name
  #   tColName: time col name
  #   index: id col name
  #
  # Returns:
  #   
  nvisits <- nrow(df)
  newColName <- paste(colName, "Rate", sep = "") 
  df[,newColName] <- NA
  for(i in 1:(nvisits-1)){
    dDiff <- df[i+1, colName] - df[i, colName]
    dDay <- as.numeric(as.Date(df[i+1,tColName]) - as.Date(df[i,tColName]))
    dYear <- dDay/365
    if (df[i+1, index] == df[i, index] ){
      df[i+1, newColName] <- dDiff/dYear
    }
  }
  newdf <- df
  return(newdf)
}


targetColList <- c("group1","group2", "group3","modRelativePain", "EnjoyLife" )
diagnorate <- diagnoeff
for (col in targetColList){
  diagnorate <- calcRate(diagnorate, col, "ExamDate", "EPICID")
}
diagnorate<-diagnorate[- which( is.na(diagnorate['PrevEDSS'])),]
diagnorate['PrevEDSSRate'][is.na(diagnorate['PrevEDSSRate']),] <- 0
diagnorateeffstatic <- diagnorate

diagnorateeffstatic <- diagnorateeffstatic[,!(names(diagnorateeffstatic) %in% drop)]
diagnorateeffstatic <- diagnorateeffstatic[,!(names(diagnorateeffstatic) %in% targetColList)]

# h5 save
h5write(diagnorateeffstatic, filePath,"diagnorateeffstatic")
h5write(merged_updated, filePath,"merged_updated")
h5write(diagno, filePath,"diagno")
h5write(diagnostatic, filePath,"diagnostatic")
h5write(diagnoeff, filePath,"diagnoeff")
h5write(diagnoeffstatic, filePath,"diagnoeffstatic")
file.copy(filePath, filePathPython, overwrite = TRUE)
file.copy(filePath, filePathPython, overwrite = TRUE)