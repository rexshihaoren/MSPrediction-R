setwd("~/Dropbox/research/MSBioScreen/MSPrediction-R/Data Scripts")
# get modified-fam2
require(ggplot2)
require(rhdf5)
require(MASS)
require(fitdistrplus)
modfam2<-read.csv("step3/data_all.csv")
# fetch original fam2
load("step1/result.RData")

# Merge fam2, modfam2, fullTable3

#First change fam2 colname "VisitId" to "VisitID"
colnames(fam2)[1] <- "VisitID"
# get rid of column "X" in modfam2
modfam2 <- modfam2[-1]


#merged = merge(fam2, modfam2, by = "VisitID", all.y = TRUE)
merged = merge(fam2, modfam2)
#merged <- merge(fam2, modfam2, by = "VisitID")
#merged <- merge(merged, fullTable3, by = "VisitID")


#merged <- merge(fam2, modfam2, by="VisitId", by.y="VisitID")
merged = merge(fam2, modfam2)
#merged <- merge(fam2, modfam2, by="VisitID")
merged <- merge(merged, fullTable3)
#merged <- merge(merged, fullTable3, by="VisitId", by.y="VisitID")
# only 5 cols, group1~3, relative-pain, enjoylife
modfam2<-modfam2[,8:12]

# get rid visitID
fam2<-fam2[,-1]


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

#save fam2, modfam2, fam2_bin, modfam2_bin, modfam2_processing, fam2_processing, and  merged in HDF5 format
filePath <- 'data/predData.h5'
h5createFile(filePath)
h5write(fam2, filePath,"fam2")
h5write(modfam2,filePath,"modfam2")
h5write(merged, filePath, "merged")
h5write(fam2_bin, filePath,"fam2_bin")
h5write(modfam2_bin, filePath,"modfam2_bin")
h5write(modfam2_processing, filePath,"modfam2_processing")
h5write(fam2_processing, filePath,"fam2_processing")

filePathPython <- '../../MSPrediction-Python/data/'
file.copy(filePath, filePathPython)



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


###Examples
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



##################Calculate EDSS Rate###########
merged_updated <- merged[order(merged$EPICID, merged$ExamDate),]
# Add Empty EDSSRate col
merged_updated[, "EDSSRate"] <- NA

nvisits <- nrow(merged_updated)
for(i in 1:(nvisits-1)){
  dEDSS <- merged_updated[i+1, "ActualEDSS"] - merged_updated[i, "ActualEDSS"]
  dDay <- as.numeric(as.Date(merged_updated[i+1,]$ExamDate) - as.Date(merged_updated[i,]$ExamDate))
  dYear <- dDay/365
  if (merged_updated[i+1, "EPICID"] == merged_updated[i, "EPICID"] ){
    merged_updated[i+1, "EDSSRate"] <- dEDSS/dYear
  }
}

### one more col in modified EDSSR, ignore abs dEDSSS <= 0.5; 2 class, increase or others
merged_updated[, "ModEDSSR"] <- NA
for(i in 1:(nvisits-1)){
  dEDSS <- merged_updated[i+1, "ActualEDSS"] - merged_updated[i, "ActualEDSS"]
  dDay <- as.numeric(as.Date(merged_updated[i+1,]$ExamDate) - as.Date(merged_updated[i,]$ExamDate))
  dYear <- dDay/365
  if (merged_updated[i+1, "EPICID"] == merged_updated[i, "EPICID"] ){
    if (abs(dEDSS)<.5){
      merged_updated[i+1, "ModEDSSR"] <- 0
    } else {
      merged_updated[i+1, "ModEDSSR"] <- 1
    }
  }
}

h5write(merged_updated, "data/predData.h5","merged_updated")
gendist(merged_updated, geom_histogram, "EDSSRate", "merged_updated")
gendist(merged_updated, geom_density, "EDSSRate", "merged_updated")
gendist(merged_updated, geom_histogram, "ModEDSSR", "merged_updated")

#########Treatment############
DMT<-read.table("tableDMT.csv")