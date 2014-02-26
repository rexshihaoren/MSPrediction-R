dir_to_get = 
  "./step0/"
  
# Get the functions 
source(paste(dir_to_get, "DB_function.R", sep = ""))

#### Connect to the DB ####
source(paste(dir_to_get, "DB_Connect.R", sep = "")) 

TABLES <- list()
for (s_table in tablenames) {
  TABLES[[s_table]] <- dbReadTable(con, s_table )
}


# Pre-treatment of some tables --------------------------------------------

## treat the visit table
visit2 <- within(TABLES$visit, {
  VisualLoss <- factor(VisualLoss, levels = c('NO', 'UNI', 'BI'))
  Ataxia <- factor(Ataxia, levels = c('NO', 'YES'))
  Myelopathy <- factor(Myelopathy, levels = c('NO', 'YES'))
  OpticNeuritis <- factor(OpticNeuritis, levels = c('N', 'UNI', 'BI', 'BI SIMUL'))
  ATM <- factor(ATM, levels = c('N', 'NONREC', 'RECUR', 'UNK'))
})
visit2 <- data.frame(lapply(visit2, as.vector))

## treat the subject table
subject2 <- within(TABLES$subject, {
  AlcoholConsumption <- factor(AlcoholConsumption)
#   levels(AlcoholConsumption) <- levels(AlcoholConsumption)[c(6, 7, 4, 2, 3, 1, 5)]
  Smoking <- as.numeric(factor(Smoking))
})[, c('Gender', 'AlcoholConsumption', 'Smoking', 'EPICID')]

## treat the eduweight table
eduweight2 <- within(TABLES$eduweight, {
  CaffeineFreq <- relevel(factor(CaffeineFreq),'None')
  YrsEducation <- factor(YrsEducation, levels = c("4 or less", "5 to 10", "10 to 15", "11 to 15", "more than 15" ))
  })
  
## treat the fssc table 
lapply(TABLES$fssc[, !names(TABLES$fssc) %in% 'VisitId'], function(x) table(factor(x)) )
#check the conversion to numeric for fssc, 'x' or 'X' means no possible assessment:
suppressWarnings( lapply(TABLES$fssc[, !names(TABLES$fssc) %in% 'VisitId'], function(x) rbind(levels(factor(x)), as.numeric(levels(factor(x)))) ) )
limits <- c(6,5,5,6,6,6,5) # remove weird values (above the normal)
names(limits) <- c('Pyramidal',
                   'Cerebellar',
                   'Brainstem',
                   'Sensory',
                   'Bowel',
                   'Visual',
                   'Mental')
fssc2 <- subset( TABLES$fssc, select = - c(VisitId))
suppressWarnings(fssc2 <- data.frame(mapply(function(x,y) ifelse(as.numeric(x)<= y, as.numeric(x), NA) , fssc2, limits[names(fssc2)] )) )


# Creation of the "full tables" ------------------------------------------

fullTableRAW <- merge(merge(eduweight2, visit2, by = "EPICID"), subject2, all.x = T, by = "EPICID")

fullTable <- data.frame(lapply(fullTableRAW, as.vector)) # The only table with strings as factors!
# See
plot( ~ VisitStatus, data =fullTable)
barplot (head(as.table(ftable(is.na(fullTable$ActualEDSS),fullTable$VisitStatus)+1),Inf),
         beside = T , log='y', legend = c("DATA", "EDSS = NA"), args.legend = list(y=2000), 
         main = 'dropped data because of unknown EDSS values - log scale')
fT <- within(fullTable, NAEDSS <- factor(is.na(ActualEDSS), labels = c('NORMAL', 'NO EDSS') ) )
plot( ~ VisitStatus + NAEDSS, data =fT)
fT2 <- subset(fT, VisitStatus == 'Scheduled' & NAEDSS == 'NORMAL')
# Skim...
fullTable <- subset(fullTable, VisitStatus=='Complete') # First skimming : keep the complete visits only
fullTable <- fullTable[! is.na(fullTable$ActualEDSS),] # Second skiming : No visit without a valid EDSS

## Same, but add the biomarkers
fullTable2 <- subset(merge(fullTableRAW, TABLES$biomarkers, by = 'EPICID', all.x = T), VisitStatus=='Complete' &  ! is.na(ActualEDSS))

## Add some of the biomarkers and the fssc data
fullTable3 <- subset(
  merge(merge(fullTableRAW, 
              TABLES$biomarkers[, c('EPICID', 'DRB1_1501', 'MSGB', 'MSGBnonMHCsnps')], 
              by = 'EPICID', all.x = T),
        data.frame(fssc2, VisitID = TABLES$fssc$VisitId),
        all.x = T, by="VisitID"),
  VisitStatus=='Complete' &  ! is.na(ActualEDSS))
rownames(fullTable3) <- fullTable3$VisitID

## Add some mri data!!
fullTable32 <- merge(fullTable3, 
                     TABLES$mridetails[,c('VisitID', 'CE', 'CE_Lesion', 'New_T2_Lesions', 'T1_LL', 'T2_LL', 'SIENA.1', 'SIENA.2', 'GM_Volume', 'Siena_PBVC')],
                     by = 'VisitID', all.x = T)

## Add the fams
fullTable33 <- merge(fullTable3, 
                 TABLES$fams,
                 by.x = 'VisitID', by.y = 'VisitId', all.x = T)

## Create a factorised version of the last table
fullTable4 <- data.frame(lapply(fullTable32, as.vector))


## Create a perso DB with SQLite
if (createTable <- FALSE) {
  drv2 <- dbDriver("SQLite")
  con2 <- dbConnect(drv2, dbname="/media/FD/BIOSCREEN/R/DB/persoDB.sqlite")
  dbWriteTable(con2, 'fullTable', fullTable32, overwrite = T)
}

save.image(paste(dir_to_get, "result.RData", sep = ''))

