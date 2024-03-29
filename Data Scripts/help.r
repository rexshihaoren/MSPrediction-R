#### All the functions needed for MSBioScreen-R ######

### Packages ####

#### Switch ####
# newModEDSS denotes whether to use "new" ModEDSS (allow 0.5 differences for EDSS>4)
if (! exists("newModEDSS"))
  newModEDSS <- F
setwd("~/Dropbox/research/MSBioScreen/MSPrediction-R/Data Scripts")
gatherPath <- 'data/gather.RData'
testPath <- 'data/test.RData'
if(newModEDSS){
  # Imprecision0-4
  filePath <- 'data/predData_Impr0-4.h5'
  filePathPython <- '../../MSPrediction-Python/predData_Impr0-4/'
} else {
  filePath <- 'data/predData.h5'
  filePathPython <- '../../MSPrediction-Python/predData/'
}
# famPath <- 'data/fam.RData'
# corePath <- 'data/core.RData'
# MRIPath <- 'data/MRI.RData'
# examPath <- 'data/exam.Rdata'
# treatmentPath <- 'data/treatment.RData'
# targetPath<- 'data/target.RData'
# geneticsPath<- 'data/genetics.RData'
# staticPath <- 'data/static.RData'
# ppPath<-'data/pp.RData'
# apPath<- 'data/ap.RData'
# imputePath <- 'data/impute.RData'
require(ggplot2)
require(rhdf5)
require(MASS)
require(fitdistrplus)
require(plyr)
require(RMySOL)
# For knn
require(DMwR)

Binarize <- function(df, target){
  # to Binarize a target column of a datafram
  med <- median(df[, target])
  bin <- df
  bin[, target] <- ifelse(bin[, target]<= med, 0 , 1)
  as.data.frame(bin)
}

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

KnnImputeXY<-function(df, targetList, k = 4){
  # KnnImpute matrix [X,y]
  #
  # Args:
  #   df: dataset
  #   target: names of y
  #   k: k for knn, 4 as default
  #
  # Returns:
  #   output: matrix [ImputedX y]
  tempX <- df[, ! names(df)%in%targetList]
  tempy <- df[, targetList]

  tempX <- knnImputation(as.data.frame(apply(tempX, c(1,2), as.numeric)), k = k)
  output <- tempX
  output[, targetList] <- tempy
  as.data.frame(output)
}

combine<-function(dfs, imp = F, cut = F, cutcols = NA, rmcols = NULL, tgt = "ModEDSS", index = "VisitID"){
  # function to combine dataframs, with the option of imputation, cut incomplete cases, and remove certain columns by name
  # 
  # Args:
  #   dfs: list of dataframs
  #   imp: T if imputation
  #   cut: T if only preserve complete cases
  #   cutcols: list of colnames that need to use compete.cases aka Cut
  #   rmcols: list of colnames to remove
  #   tgt: string, target column name
  #   index: string, id col name
  #   
  # Returns:
  #   output: the disired dataset
  #   
  
  output <- dfs[[1]]
  for(i in (2:length(dfs))){
    output <- merge(output, dfs[[i]], by = index, all.x = T, all.y = F)
  }
  output <- output[, ! names(output)%in%rmcols]
  output <- output[, ! names(output) %in% index]
  stopifnot(nrow(output) == nrow(dfs[[1]]))
  if(cut){
    if (is.na(cutcols)){
      output <- output[complete.cases(output),]
    } else {
      output <- output[complete.cases(output[cutcols]),]
    }
  }
  if(imp){
    output <- KnnImputeXY(output, tgt)
  }
  as.data.frame(output)
}

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

getDfName<-function(df, newModEDSS){
  # add "new" in front of df if newModEDSS is True
  # Args: 
  #   df: datasetname
  #   newModEDSS: whether we use the new ModEDSS
  #
  # Returns:
  if(newModEDSS){
    df <- paste("new", df, sep = "")
  }
  return(df)
}


## GetAnalysisGrpStr

# High Potency Group 
HPgroup <- c("Chemo", "Chemo - Cytoxan", "FTY 720" , "FTY 720 (Double-blind)", "Gilenya", "Fingolimod (Gilenya)", "Chemo - Mitoxantrone", "Chemo - Rituximab", "Chemo - Rituxan", "Chemo - Rituximab", "Chemo-Placebo", "Fumaderm" , "BG12", "Tecfidera", "Natalizumab", "Antegren (clinical trial)", "Other - Antegren Clinical Trial")
# Platform Therapy Group
#  NOTE: other - IVIG is lower case
PTgroup <- c("Aubagio", "Interferon - Avonex", "Interferon - Betaseron", "Copaxone", "Glatiramer Acetate (Copolymer-1)", "Extavia", "Interferon", "Interferon - Rebif", "Azathioprine (Immuran)", "Chemo - Imuran", "Chemo - Cyclosporin", "Chemo - Cladribine Trial", "Chemo - Cellcept", "Chemo - Methotrexate", "other - IVIG", "Other - IVMP", "Other - IVSM", "Other - Decadron")
# Other Group
Ogroup <- c("Ampyra", "Doxycycline", "Low Dose Naltrexone",
"Low Dose Naltrexone(Double-Blind)", "MBP8298", "Other - Minocycline", "Other - Oral Steroids", "Other - Plasmapheresis", "Other - T Cell Vaccination Study", "Other - TCR Peptide Trial")

getAnalysisGrpStr<-function(tmls){
 # Determine which Analysis Group this Treamtment Molecule belongs to
 # Args:
 #     tmls: TreatmentMolecule, array of string
 # Returns:
 #     prevAG: Previous Analysis Group String, string
  prevAG <- c()
  # print("tmls:")
  # print(tmls)
  for(tm in tmls){
    if(tm %in% HPgroup){
      prevAG <-c(prevAG, "PrevHighPotencyGrp")
    } else{
      if(tm %in% PTgroup){
        prevAG <- c(prevAG, "PrevPlatformTherapyGrp")
      } else {
        prevAG <- c(prevAG, "PrevOtherGrp")
      }
    }
  }
  # print("prevAG:")
  # print(prevAG)
  return(unique(prevAG))
}

getTreatmentClassStr<-function(ttls){
 # Determine which TreatmentClass this Treamtment Molecule belongs to
 # Args:
 #     ttls: TreatmentType, array of string
 # Returns:
 #     prevTC: Previous Treatment Class String, string
  prevTC <- c()
  for (tt in ttls){
    if(tt == "Chemo & Immunosuppressant"){
      prevTC <-c(prevTC, "PrevImmunosuppressantTyp")
    } else{
      if(tt == "Monoclonal Ab"){
        prevTC <- c(prevTC, "PrevMonoclonalAbTyp")
      } else {
        prevTC0 <- paste("Prev", tt, sep = "")
        prevTC0 <- paste(prevTC0, "Typ", sep = "")
        prevTC<-c(prevTC, prevTC0)
      }
    }
  }
  return(unique(prevTC))
}

###### Plotting Functions for Statistical Analysis#############

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
      ggsave(file = paste("plots/",paste(dfname, "cpdf",i, sep = "_"), ".pdf", sep=""))
      #ggsave(file = fpath)
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
