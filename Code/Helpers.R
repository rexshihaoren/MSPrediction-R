

# Logging -----------------------------------------------------------------

source("Code/Logger.R")


# Project Specific --------------------------------------------------------

outputDir <- "Output"
outputPath <- function(fileName, extension = "csv") {
  filePath <- file.path(outputDir, paste(fileName, extension, sep = "."))
  dir.create(dirname(filePath), showWarnings = FALSE)
  return(filePath)
}


# Data Exploration --------------------------------------------------------

giveMeTable <- function(vec) {
  tt <- table(vec, useNA = "ifany")
  tt[order(tt, decreasing = T)]
}


