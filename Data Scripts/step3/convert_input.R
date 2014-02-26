coefficients = as.matrix(read.csv('./coefficients.csv', row.names=1))

usr_data = c('RelativePain' = 2, 
             'FeelSad' = 1, 
             'LosingHope' = 0, 
             'EnjoyLife' = 3, 
             'TrappedByCondition' = 0,
             'DepressedAboutCondition' = 1, 
             'FeelUseless' = 0, 
             'FeelOverwhelmed' = 0, 
             'LackEnergy' = 3, 
             'FeelTired' = 3,
             'TroubleStarting' = 3,
             'TroubleFinishing' = 3,
             'NeedRest' = 2, 
             'TroubleRemembering' =3, 
             'TroubleConcentrating' = 3, 'ThinkingSlower' = 4,
             'TroubleLearning' = 2)

user_data = matrix(usr_data, nrow = 1, ncol = 17)

user_data_pca = user_data %*% coefficients
 