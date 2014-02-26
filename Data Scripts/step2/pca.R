require(pheatmap)
source('./step2/ini.R')

scale_by_variance = TRUE

# parameters
current_dir = 
  "./step2/"

load(paste(current_dir, 'ini.RData', sep = ''))

# functions
plot_pca = function(pca, x, y, name, color = adjustcolor('red', alpha = 0.5), xlimit = c(-1, 1), ylimit = c(-1, 1))
{
  plot(pca[['x']][,c(x, y)], pch=19, cex=0.5, col=adjustcolor('blue', alpha = 0.2), main = name, xlim = xlimit, ylim = ylimit)
  arrows(rep(0,4), rep(0,4), pca[['rotation']][,x], pca[['rotation']][,y], col= color, lwd=2)
  text(pca[['rotation']][,x], pca[['rotation']][,y] + 0.03, rownames(pca[['rotation']]), cex = 0.7)  
}

plot_mat = function(mat)
{
  pheatmap(mat, col = 'white', display_numbers = T, legend= F, cluster_rows=F, cluster_cols = F, fontsize=14)
}

# code
data_interested = na.omit(allQOL[, c(1, 9:39)])
if (scale_by_variance){
  result_file_name = 'pca_scaled.pdf'
  groups = sapply(resolution, function(x){
    if (x %in% c(1,2)) {
      1
    } else if (x %in% c(4, 5, 6)){
      2
    } else {
      3
    }
  })
  sds = sapply(data_interested[,-1], sd)
  group_sds = sapply(1:3, function(x) mean(sds[names(groups[groups == x])]))
  data_interested = data.frame('VisitId'= data_interested$VisitId, 
                               mapply(function(x, y) x/group_sds[groups[y]], 
                                      data_interested[, -1], 
                                      names(data_interested[,-1]) )
                               )
} else {
  result_file_name = 'pca.pdf'
}


pdf(paste(current_dir, result_file_name, sep = ''), 7, 5)
# all data
pc_all = prcomp(data_interested[,-1])
cols = c(rep('red', 7), rep('green', 17), rep('blue', 7))
plot_pca(pc_all, 1, 2, 'all QOL', col = adjustcolor(cols, alpha = 0.3), xlimit = c(-0.5, 0.5), ylimit = c(-0.5, 0.5))
legend('right', legend = c('visit', 'fam', 'fssc'), fill = c('red', 'green', 'blue'), cex = 0.6)
plot_pca(pc_all, 3, 4, 'all QOL', col = adjustcolor(cols, alpha = 0.3), xlimit = c(-0.5, 0.5), ylimit = c(-0.5, 0.5))
legend('right', legend = c('visit', 'fam', 'fssc'), fill = c('red', 'green', 'blue'), cex = 0.6)
plot_pca(pc_all, 5, 6, 'all QOL', col = adjustcolor(cols, alpha = 0.3), xlimit = c(-0.5, 0.5), ylimit = c(-0.5, 0.5))
legend('right', legend = c('visit', 'fam', 'fssc'), fill = c('red', 'green', 'blue'), cex = 0.6)
plot(pc_all, main = 'all QOL')

# all fam
pc_all_fam = prcomp(data_interested[,9:25])
plot_pca(pc_all_fam, 1, 2, 'all fam data', xlimit = c(-0.3, 0.5), ylimit = c(-0.4, 0.4))
plot_pca(pc_all_fam, 3, 4, 'all fam data')
plot_pca(pc_all_fam, 5, 6, 'all fam data')
plot(pc_all_fam)
plot_mat(pc_all_fam$rotation)

# fam: first group
formula = as.formula('~ TroubleConcentrating + TroubleRemembering + ThinkingSlower + TroubleLearning')
pc.cr = prcomp(formula, data = data_interested)
plot_pca(pc.cr, 1, 2, 'fam: group1')
plot(pc.cr, main = 'fam: group1')
plot_mat(pc.cr$rotation)

colnames(pc.cr$x) = lapply(colnames(pc.cr$x), function(x) paste('group1.', x, sep = ''))
data_prc = data.frame(pc.cr$x)

# fam: second group
formula = as.formula('~ NeedRest + LackEnergy + FeelTired + TroubleStarting + TroubleFinishing')
pc.cr = prcomp(formula, data = data_interested)
plot_pca(pc.cr, 1, 2, 'fam: group2')
plot(pc.cr, main = 'fam: group2')
plot_mat(pc.cr$rotation)

colnames(pc.cr$x) = lapply(colnames(pc.cr$x), function(x) paste('group2.', x, sep = ''))
data_prc = data.frame(data_prc, pc.cr$x)

# fam: third group
formula = as.formula('~ FeelSad + FeelUseless + LosingHope + TrappedByCondition + DepressedAboutCondition + FeelOverwhelmed')
pc.cr = prcomp(formula, data = data_interested)
plot_pca(pc.cr, 1, 2, 'fam: group3')
plot(pc.cr, main = 'fam: group3')
plot_mat(pc.cr$rotation)

colnames(pc.cr$x) = lapply(colnames(pc.cr$x), function(x) paste('group3.', x, sep = ''))
data_prc = data.frame(data_prc, pc.cr$x)

# fam a PCA on the PCs
data_prc = data.frame(data_prc, 'RelativePain' = data_interested$RelativePain, 'EnjoyLife' = data_interested$EnjoyLife)
pc_all_famPC = prcomp(data_prc)
plot_pca(pc_all_famPC, 1, 2, 'all fam PC', xlimit = c(-0.1, 0.7), ylimit = c(-0.7, 0.6))
plot_pca(pc_all_famPC, 3, 4, 'all fam PC')
plot_pca(pc_all_famPC, 5, 6, 'all fam PC')
plot(pc_all_famPC)
plot_mat(pc_all_famPC$rotation)

# compare the modals
V <- var(data_prc)
if (scale_by_variance){
  max = 7
} else{
  max = 0.5
}
pheatmap(V, cluster_rows=F, cluster_cols=F, legend = T, display_numbers=T, breaks=seq(-max, max, length.out=151),
         color = colorRampPalette(c("darkblue", "blue", "white", "orange", "firebrick"))(150)
)

pheatmap(V[order(diag(V), decreasing=T), order(diag(V), decreasing=T)], cluster_rows=F, cluster_cols=F, legend = T, display_numbers=T, breaks=seq(-max, max, length.out=151),
         color = colorRampPalette(c("darkblue", "blue", "white", "orange", "firebrick"))(150)
)

vars2 <- diag(V)
vars2 <- sort(vars2, decreasing=T)
plot(cumsum(vars2)/sum(vars2), type='b', col='red', ylim = c(0,1))
lines(cumsum(pc_all_fam$sdev^2) / sum(pc_all_fam$sdev^2), col = 'blue')
legend('right', legend = c('cumsum of vars for PCs', 'cumsum of var for PCA of all fam'), fill = c('red', 'blue'), cex = 0.6)

plot(cumsum(vars2), type='b', col='red')
lines(cumsum(pc_all_fam$sdev^2), col = 'blue')
legend('right', legend = c('cumsum of vars for PCs', 'cumsum of var for PCA of all fam'), fill = c('red', 'blue'), cex = 0.6)

# Compare the selected PCs from fam with the rest.

data_famPCs_rest = data.frame(data_interested[,c(1:8, 26:32)], 
                              'fam_group1' = data_prc$group1.PC1, 
                              'fam_group2' = data_prc$group2.PC1,
                              'fam_group3' = data_prc$group3.PC1,
                              'fam_RelativePain' = data_prc$RelativePain,
                              'fam_EnjoyLife' = data_prc$EnjoyLife)

pc_all2 = prcomp(data_famPCs_rest[,-1])
cols = c(rep('red', 7), rep('blue', 7), rep('green', 5))
plot_pca(pc_all2, 1, 2, 'all QOL with fam PCs', col = adjustcolor(cols, alpha = 0.3), xlimit = c(-0.5, 0.5), ylimit = c(-0.5, 0.5))
legend('right', legend = c('visit', 'fam', 'fssc'), fill = c('red', 'green', 'blue'), cex = 0.6)
plot_pca(pc_all2, 3, 4, 'all QOL with fam PCs', col = adjustcolor(cols, alpha = 0.3), xlimit = c(-0.5, 0.5), ylimit = c(-0.5, 0.5))
legend('right', legend = c('visit', 'fam', 'fssc'), fill = c('red', 'green', 'blue'), cex = 0.6)
plot_pca(pc_all2, 5, 6, 'all QOL with fam PCs', col = adjustcolor(cols, alpha = 0.3), xlimit = c(-0.5, 0.5), ylimit = c(-0.5, 0.5))
legend('right', legend = c('visit', 'fam', 'fssc'), fill = c('red', 'green', 'blue'), cex = 0.6)
plot(pc_all2, main = 'all QOL with fam PCs')

plot(cumsum(pc_all2$sdev^2) / sum(pc_all2$sdev^2), type='b', col='red', ylim = c(0,1))
lines(cumsum(pc_all$sdev^2) / sum(pc_all$sdev^2), col = 'blue')
legend('right', legend = c('cumsum of all with fam PCs', 'cumsum of all'), fill = c('red', 'blue'), cex = 0.6)

dev.off()

png(paste(current_dir, "all_famPC.png", sep = ""), 1000, 900)

V2 = cor(data_famPCs_rest[,-1])
pheatmap(V2, clustering_distance_rows=as.dist((1-V2^2)), clustering_distance_cols=as.dist(1-V2^2), 
         cluster_rows=T, cluster_cols = T, 
         breaks=seq(-1, 1, 0.1),
         color = colorRampPalette(c("darkblue", "blue", "lightblue", "white", "wheat", "orange", "firebrick"))(20),
         legend = TRUE,
         main = "Person's correlation for all QOL with fam PCs ", display_numbers = T, fontsize= 12)
dev.off()

names(data_interested)
vars = diag(var(data_interested[-1]))
weights = c(sum(vars[1:7]), sum(vars[8:24]), sum(vars[25:31]))/sum(vars)
names(weights) = c('visit', 'fam', 'fssc')
barplot(weights, ylim = c(0, 0.8), main = 'Weight of total variance for the tables')
