########################
## Read from database ##
########################

require(RMySQL)  
# con <- dbConnect(MySQL(), user='guest', password='bioguest123', dbname='bioscreen', host='myelin.ucsf.edu')

# get fams table and clean up the table
# fams = dbReadTable(con, 'fams')
fams = na.omit(fams)
fams = fams[(fams$ThinkingSlower != 21) & (fams$LosingHope != 9),]
lapply(fams, function(x) levels(factor(x)))

# get visit table
# visit = dbReadTable(con, 'visit')

# get subject table
# subject = dbReadTable(con, 'subject')

#############################
## Use PCA to convert fams ##
#############################

# get all category values
formula1 = as.formula('~ TroubleConcentrating + TroubleRemembering + ThinkingSlower + TroubleLearning')
pc.cr1 = prcomp(formula1, data = fams[,-1])

formula2 = as.formula('~ NeedRest + LackEnergy + FeelTired + TroubleStarting + TroubleFinishing')
pc.cr2 = prcomp(formula2, data = fams[,-1])

formula3 = as.formula('~ FeelSad + FeelUseless + LosingHope + TrappedByCondition + DepressedAboutCondition + FeelOverwhelmed')
pc.cr3 = prcomp(formula3, data = fams[,-1])

# create the maatrix for rotation parameters
params = c('TroubleConcentrating', 'TroubleRemembering', 'ThinkingSlower', 'TroubleLearning',
           'NeedRest', 'LackEnergy', 'FeelTired', 'TroubleStarting', 'TroubleFinishing',
           'FeelSad', 'FeelUseless', 'LosingHope', 'TrappedByCondition', 'DepressedAboutCondition', 'FeelOverwhelmed',
           'RelativePain','EnjoyLife')
groups = c('group1', 'group2', 'group3', 'RelativePain','EnjoyLife')
coefficients = matrix(0, nrow = 17, ncol = 5, dimnames = list(params, groups))

# fcuntion to scale all rotation parameters to 0-4 scale
scale_rotation = function(rotation){
  return (4 / sum(rotation * rep(4, length(rotation))) * rotation)
}

# get rotation parameters
rotation1 = scale_rotation(pc.cr1$rotation[,1])
rotation2 = scale_rotation(pc.cr2$rotation[,1])
rotation3 = scale_rotation(pc.cr3$rotation[,1])

# update the matrix
coefficients[names(rotation1), 'group1'] = rotation1
coefficients[names(rotation2), 'group2'] = rotation2
coefficients[names(rotation3), 'group3'] = rotation3
coefficients['RelativePain', 'RelativePain'] = 0.4
coefficients['EnjoyLife', 'EnjoyLife'] = 1

pca_converted_fams = data.frame('VisitID'=fams$VisitId, as.matrix(fams[,params]) %*% coefficients)

#########################################
## Merge fams with patients' condition ##
#########################################

# add the table for patients' condition
conditions = visit[,c('VisitID',
                      'EPICID',
                      'ActualEDSS',
                      'AgeAtExam',
                      'DiseaseDuration',
                      'DiseaseCourse')]
more = subject[, c('EPICID', 'Gender')]
conditions = merge(conditions, more, by = 'EPICID', all.x = TRUE)

data_all = merge(conditions, pca_converted_fams, by = 'VisitID', all.y = TRUE)


write.csv(data_all, file = './data_all.csv')
write.csv(coefficients, file = './coefficients.csv')