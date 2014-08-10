######## Examples using msplot functions###############
source("helper.r")
###Examples for modfam2, fam2, merged ####
load(binarizePath)
# dfs <- c("modfam2", "fam2", "modfam2_bin", "fam2_bin")
# for (i in dfs)
# 	assign(i, h5read(filePath, i))
gendist(modfam2, geom_histogram, "EnjoyLife", "modfam2")
gendist(fam2, geom_histogram, "EnjoyLife", "fam2")
# Plot PDF after binarize
gendist(modfam2_bin, geom_histogram,"EnjoyLife", "modfam2_bin")
gendist(fam2_bin, geom_histogram,"EnjoyLife", "fam2_bin")

generateCPDF(modfam2_bin,geom_density, "EnjoyLife")
generateCPDF(fam2_bin, geom_histogram, "EnjoyLife")

# Plot Fitted Histogram for fam2 with 'norm' normal distribution
for (cname in colnames(fam2_bin)){
  if (cname != "EnjoyLife"){
    print(cname)
    fitCPDF(fam2_bin, cname, "EnjoyLife", "norm", geom_histogram, "mme")
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

# Plot ActualEDSS
gendist(fullTable3, geom_histogram, "ActualEDSS", "fullTable3")
gendist(fullTable3, geom_density, "ActualEDSS", "fullTable3")

###Examples for merged_update, diagno, diagnomod ####
load(diagnoPath)

# Some Ploting for merged_updated
gendist(merged_updated, geom_histogram, "EDSSRate", "merged_updated")
gendist(merged_updated, geom_density, "EDSSRate", "merged_updated")
gendist(merged_updated, geom_histogram, "ModEDSS", "merged_updated")

# Some plotting on ModEDSS for diagno and diagnomod
generateCPDF(diagno, geom_histogram, "ModEDSS")
generateCPDF(diagnomod, geom_histogram, "ModEDSS")
generateCPDF(diagno, geom_density, "ModEDSS")
generateCPDF(diagnomod, geom_density, "ModEDSS")
generateCPDF(diagnomodrate, geom_density, "ModEDSS")
