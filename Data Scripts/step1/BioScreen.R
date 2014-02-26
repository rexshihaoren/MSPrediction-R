require(grid)
require(pheatmap)
require(entropy)

source('./step1/ini.R')

current_dir =  
  "./step1/"

load(paste(current_dir, 'ini.RData', sep = ''))

#### Defining data / parameters ------------

resolution = c(rep(1, 6), 2, 10, rep(4, 16), 6, 5, 6, 5, 6, 6, 5)
names(resolution) = c('MotorWeakness',  'SensoryDisturbance', 'Ataxia', 'BladderDisturbance', 
                      'BowelDisturbance', 'CogDisturbance', 'VisualLoss', # in visit
                      'RelativePain', 'FeelSad', 'LosingHope', 'EnjoyLife',
                      'TrappedByCondition', 'DepressedAboutCondition', 'FeelUseless', 'FeelOverwhelmed',
                      'LackEnergy', 'FeelTired', 'TroubleStarting', 'TroubleFinishing',
                      'NeedRest', 'TroubleRemembering', 'TroubleConcentrating', 'ThinkingSlower',
                      'TroubleLearning', # in fam
                      'Visual', 'Brainstem', 'Pyramidal', 'Cerebellar',
                      'Sensory', 'Bowel', 'Mental') # in fssc

#### Defining tool functions ------------

## return the row numbers for wich contains NA, NULL or NAN
check_invalid_data = function(dataFrame){
      lapply(
        dataFrame, 
        function(x){
          seq(1:length(x))[is.na(x) | is.nan(x) | is.null(x)]
        })
}

## return the dataframe in [0, 1] scale for all the factors and numeric except some columns
to_01_scale = function(dataFrame) {
  cats = names(dataFrame)
  not_01_scale_columns = cats[!(cats %in% names(resolution))]
  is_01_scale_columns = cats[cats %in% names(resolution)]
  data.frame(subset(dataFrame, select= not_01_scale_columns),
             mapply(
               function(x, y){
                 if (class(x) == "numeric" | class(x) == "integer") {
                   x/resolution[y]
                   } 
                 else if (class(x) == "factor") {
                   if (any(is.na(suppressWarnings(as.numeric(levels(x)))))) {
                     (as.numeric(x)-1) / resolution[y]
                   }
                   else {
                     as.numeric(levels(x))[x] / resolution[y]
                     }
                   }
                 else {
                   x
                   }
                 },
               dataFrame[, is_01_scale_columns],
               is_01_scale_columns))
}

## return the correlation matrix for the dataframe and draw the heatmap for it excluding the VisitId 
get_cor = function(dataFrame, tableName, heatMapName, method = "person", printPNG = T, MIMethod = "default") {
  cats = names(dataFrame)
  cats = cats[cats %in% names(resolution)]
  corEstimator = matrix(nrow = length(cats), ncol = length(cats), byrow = TRUE, dimnames = list(cats, cats) )
  ## fill in the cor table and print out the tables 
  pdf(paste(current_dir, tableName, ".pdf", sep = ""))
  for (var1 in cats){
    for (var2 in cats){
      plot(table(dataFrame[[var1]], dataFrame[[var2]]), main = paste("table of ", var2, " ~ ", var1), )
      if (method == "person") {
        corEstimator[var1, var2] = cor(dataFrame[[var1]], dataFrame[[var2]], use = "complete.obs", method = "pearson")
      } else if (method == "MI") {
        corEstimator[var1, var2] = getMI(dataFrame[[var1]], dataFrame[[var2]], method=MIMethod)
      } else if (method == "logMI") {
        corEstimator[var1, var2] = log(getMI(dataFrame[[var1]], dataFrame[[var2]], method=MIMethod))
      } 
    }
  }
  dev.off()
  Breaks <- switch(method,
                   "logMI"=seq(-3,1,0.2),
                   "MI"=seq(0,1,0.05),
                   "person"=seq(-1, 1, 0.1))
  Colors <- switch(method,
                   "logMI"=colorRampPalette(c("white", "wheat", "orange", "firebrick"))(20),
                   "MI"=colorRampPalette(c("white", "wheat", "orange", "firebrick"))(20),
                   "person"=colorRampPalette(c("darkblue", "blue", "lightblue", "white", "wheat", "orange", "firebrick"))(20))
  DIST <- switch(method,
                 "logMI"=as.dist(-min(corEstimator)-corEstimator),
                 "MI"=as.dist(1-corEstimator^2),
                 "person"=as.dist(1-corEstimator^2))

  
  ## print the heatmap for the correlation
  if (printPNG) png(paste(current_dir, heatMapName, ".png", sep = ""), 1000, 900)
  pheatmap(corEstimator, clustering_distance_rows=DIST, clustering_distance_cols=DIST, 
           cluster_rows=T, cluster_cols = T, 
           breaks=Breaks,
           color = Colors,
           legend = TRUE,
           main = paste(switch(method, "person"="Pearson Correlation", "MI"="Mutual Information", "logMI"="log- Mutual Information"), "for: ", heatMapName), display_numbers = T, fontsize= 12)
  if (printPNG) dev.off()  
  return (corEstimator)
}
  
## return the mutual information between two variables
getMI <- function(x, y, method = "default") {
  XY <- table(x,y)
  MI <- switch(method,
               "shrink" = mi.shrink(XY),
               "default" =,
               mi.empirical(XY))
  return(MI)
}

drawTable <- function(x,y, nameX = "X", nameY="Y") {
  cTable <- table(x,y)
  NR <- length(x)
  px <- table(x)/NR
  py <- table(y)/NR
  pTable <- cTable/NR 
  p2Table <- outer(px,py)
  rTable <- pTable/p2Table
  lTable <- log(rTable)
  pheatmap(mat=lTable, cluster_rows = FALSE, cluster_cols=FALSE, 
           breaks=c(-40,seq(-4, 4, 0.1), 40),
           color=colorRampPalette(c("darkblue", "blue", "lightblue", "white", "wheat", "orange", "firebrick"))(82),
           legend=FALSE,
           main = "Probability Map",
           display_numbers=TRUE, fontsize = 14)
}

#### Perform the analysis ----------------


##########################
## Analysis of fam only ##
##########################

##############################################################
## STEP 1: Clean the dataset; Remove unreasonable data and NAs
##############################################################

fam2 = TABLES$fams

## Only a few NAs. Rows containing NA might not be valid. Should omit these rows for analysis
check_invalid_data(fam2)
fam2 = na.omit(fam2)

## Thinking Slower has a level "21" with only 1 count, might be problem with that. (VID:2456)
## Lossing hope has a level "9" with only 1 count, might be problem with that. (VID: 2767)
lapply(fam2, function(x) levels(factor(x)))
fam2 = fam2[(fam2$ThinkingSlower != 21) & (fam2$LosingHope != 9),]
## Check the general pattern of the data
# quick_analysis_table(fam2)

####################################################
## STEP 2: Find the correlation between 2 parameters
####################################################
fam3 = to_01_scale(fam2)
person_cor = get_cor(fam3, "fam", "fam")
mi_cor = get_cor(fam3, "fam", "fam", method="MI", printPNG=F)
plot(abs(person_cor), mi_cor, col=adjustcolor(ifelse(sign(person_cor)==1, "red", "blue"), alpha = 0.7), pch = 19 )
mi_cor2 = get_cor(fam3, "fam", "fam", method="MI", printPNG=F, MIMethod = "shrink") # doesn't realy make a difference since the number of sample per case is "high"
plot(mi_cor, mi_cor2); abline(a=0, b=1)
logmi_cor = get_cor(fam3, "fam", "fam", method="logMI", printPNG=F) # gives better results
plot(abs(person_cor), logmi_cor, col=adjustcolor(ifelse(sign(person_cor)==1, "red", "blue"), alpha = 0.7), pch = 19)

######################
## Analysis of fssc ##
######################
##############################################################
## STEP 1: Clean the dataset; Remove unreasonable data and NAs
##############################################################

## 10% of the rows contains NAs, not going to delete all NAs
check_invalid_data(fssc2)

## Check the general pattern of the data
# quick_analysis_table(fssc2)

####################################################
## STEP 2: Find the correlation between 2 parameters
####################################################
fssc3 = data.frame("VisitId" = TABLES$fssc$VisitId, to_01_scale(fssc2))
person_cor = get_cor(fssc3, "fssc", "fssc")

################################
## STEP 3: Combine fams and fssc
################################
fams_fssc = merge(fam3, fssc3, by = "VisitId", all.x = TRUE, all.y= TRUE)
person_cor = get_cor(fams_fssc,"fams_fssc", "fams_fssc")


#######################################################
## Analysis of parameters in Visit and other tables  ##
#######################################################

##############################################################
## STEP 1: Clean the dataset; Remove unreasonable data and NAs
##############################################################

## Getting the data 
visitnames <- c('VisitID',
                'EPICID',
                'Gender',
                'ActualEDSS',
                'AgeAtExam',
                'DiseaseDuration',
                'DiseaseType', 
                'DiseaseCourse',
                'MotorWeakness', 
                'SensoryDisturbance', 
                'Ataxia',
                'BladderDisturbance', 
                'BowelDisturbance', 
                'CogDisturbance',
                'VisualLoss')
visit3 = fullTable3[,visitnames] # here gender is not in visit actually
names(visit3)[names(visit3)=="VisitID"] = "VisitId"

## 10% of DiseaseType are NAs, probability not a good parameter for analysis
## A few NAs in Ataxia(6) and VisualLoss(16).
check_invalid_data(visit3)

## check the levels of the data.
## Gender should be a factor
## VisualLoss should be leveled as NO, UNI, BI
## Check ActualEDSS, AgeAtExam, DiseaseDuration are valid
lapply(visit3, levels)
visit3$Gender = factor(visit3$Gender)
visit3$VisualLoss = factor(visit3$VisualLoss, levels = c("NO", "UNI", "BI"))
summary(visit3$ActualEDSS)
summary(visit3$AgeAtExam)
summary(visit3$DiseaseDuration)

## Check the general pattern of the data
# quick_analysis_table(visit3)

####################################################
## STEP 2: Find the correlation between 2 parameters
####################################################
visit4 = to_01_scale(visit3)
person_cor = get_cor(visit4, "visit", "visit")

##################################
## STEP 3: Combine visit with fam
##################################
visit_fam = merge(visit4, fam3, by = "VisitId", all.x = TRUE)
person_cor = get_cor(visit_fam, "visit_fam", "visit_fam")

##################################
## STEP 4: Combine visit with fam
##################################
visit_fssc = merge(visit4, fssc3, by = "VisitId", all.x = TRUE)
person_cor = get_cor(visit_fssc, "visit_fssc", "visit_fssc")

#############################
## STEP 5: Combine everything
#############################
allQOL = merge(visit4, fams_fssc, by = "VisitId", all.x = TRUE)
person_cor = get_cor(allQOL,"allQOL", "allQOL")

save.image(paste(current_dir, "result.RData", sep = ''))
