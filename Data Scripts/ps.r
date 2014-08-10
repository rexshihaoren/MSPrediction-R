# ###### Add patient specific's to diagno #######

# ####### Add more features from fullTable32 #########
# load("step0/result.RData")
# load(diagnoPath)
# ####### Addpatient specific to merged_updated
# psnames <- c('AgeOfOnset', 'Gender', 'DRB1_1501', 'OnsetToYr5RelapseCount')
# ps <- fullTable32[c(psnames, "VisitID")]
# ps <- merge(ps, merged_updated)
# ps <- ps[c(pasnames, colnames(diagnostatic))]
# diagnoeffstatic['PrevEDSSRate'][is.na(diagnoeffstatic['PrevEDSSRate']),] <- 0
# # previous year parameters (+ Siena_PBVC gradient + meds)
# pp <- c('DiseaseDuration','Siena_PBVC', 'New_T2_Lesions')