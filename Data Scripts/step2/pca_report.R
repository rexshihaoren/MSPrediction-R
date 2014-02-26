# functions
plot_pca = function(pca, x, y, name, color = adjustcolor('red', alpha = 0.5), xlimit = c(-1, 1), ylimit = c(-1, 1))
{
  plot(pca[['x']][,c(x, y)], pch=19, cex=0.5, col=adjustcolor('blue', alpha = 0.2), main = name, xlim = xlimit, ylim = ylimit)
  arrows(rep(0,4), rep(0,4), pca[['rotation']][,x], pca[['rotation']][,y], col= color, lwd=2)
  text(pca[['rotation']][,x], pca[['rotation']][,y] + 0.03, rownames(pca[['rotation']]), cex = 0.7)  
}

# All QOL
pc_all = prcomp(data_interested[,-1])
cols = c(rep('red', 7), rep('green', 17), rep('blue', 7))
plot_pca(pc_all, 1, 2, 'all QOL', col = adjustcolor(cols, alpha = 0.3), xlimit = c(-0.2, 0.6), ylimit = c(-0.2, 0.6))
legend(0.4, 0.3, legend = c('visit', 'fam', 'fssc'), fill = c('red', 'green', 'blue'))
dev.off()

plot(cumsum(sort(pc_all$sdev^2, decreasing = TRUE)) / sum(pc_all$sdev^2), col='red', ylim = c(0,1), cex = 0.5, pch = 19, 
     main = "cumulative sum of variance for all QOL data", xlab = "PC", ylab = "variance")
lines(cumsum(sort(pc_all$sdev^2, decreasing = TRUE)) / sum(pc_all$sdev^2), col='red')

barplot(pc_all$sdev^2, main = "Variance of each PC for all QOL data")

# fam data
pc_all_fam = prcomp(data_interested[,9:25])
plot_pca(pc_all_fam, 1, 2, 'all fam data', xlimit = c(-0.3, 0.5), ylimit = c(-0.4, 0.4))
plot_pca(pc_all_fam, 3, 4, 'all fam data', xlimit = c(-0.5, 0.5), ylimit = c(-0.8, 0.4))
plot(cumsum(sort(pc_all_fam$sdev^2, decreasing = TRUE)) / sum(pc_all_fam$sdev^2), col='red', ylim = c(0,1), cex = 0.5, pch = 19, 
     main = "Cumulative sum of variance for all fam data", xlab = "PC", ylab = "variance")

lines(cumsum(sort(pc_all_fam$sdev^2, decreasing = TRUE)) / sum(pc_all_fam$sdev^2), col='red')

barplot(pc_all_fam$sdev^2, main = "Variance of each PC for all fam data", ylim = c(0,12))

# Comparing the PCs
plot(cumsum(vars2)/sum(vars2), col='red', ylim = c(0,1), cex = 0.5, pch = 19, ylab = 'variance', xlab = 'PCs', main = "cumulative sum of variance")
lines(cumsum(vars2)/sum(vars2), col='red', ylim = c(0,1))

points(cumsum(pc_all_fam$sdev^2) / sum(pc_all_fam$sdev^2), col = 'blue', cex = 0.5, pch = 19)
lines(cumsum(pc_all_fam$sdev^2) / sum(pc_all_fam$sdev^2), col = 'blue')

vars = sapply(data_interested[9:25], var)
points(cumsum(sort(vars, decreasing=TRUE))/sum(vars), col = 'green', cex = 0.5, pch = 19)
lines(cumsum(sort(vars, decreasing=TRUE))/sum(vars), col = 'green')
abline(v = 5, lty = 2)
abline(0, 1/17, lty = 2)
legend(9.5, 0.5, legend = c('cumsum of vars for PCs', 'cumsum of var for PCs by group', 'cumsum of var for original data frame'), fill = c('blue', 'red', 'green'), cex = 0.6)

cumsum(vars2)[5] / cumsum(pc_all_fam$sdev^2)[5]
text(6, 0.75, '99.3%', cex = 0.7, col = 'red')
