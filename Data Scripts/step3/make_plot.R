all_data_pca = read.csv("./data_all.csv")

all_data_pca =  all_data_pca[,c('group1', 'group2', 'group3', 'RelativePain','EnjoyLife')]

all_data_pca2 =  with(all_data_pca[,c('group1', 'group2', 'group3', 'RelativePain','EnjoyLife')], 
                     data.frame(LackFocus=group1, LackEnergy=group2, Psycho_Down=group3, RelativePain, EnjoyLife))

# sample how the plot looks like
plot(density(all_data_pca$group1, adjust = 3))
plot(density(all_data_pca$group2, adjust = 3))
plot(density(all_data_pca$group3, adjust = 3))
plot(density(all_data_pca$RelativePain, adjust = 3))
plot(density(all_data_pca$EnjoyLife))

hist(all_data_pca$group1, freq = FALSE, breaks = 0:4)
hist(all_data_pca$group2, freq = FALSE, breaks = 0:4)
hist(all_data_pca$group3, freq = FALSE, breaks = 0:4)
hist(all_data_pca$EnjoyLife, freq = FALSE, breaks = 0:4)
hist(all_data_pca$RelativePain, freq = FALSE, breaks = 0:4)

par(las=1)
boxplot(all_data_pca, add=F, horizontal=T, outline=F, at=c(2.5+(0:4)*3), xlim = c(1,16), boxwex = 2, whiskcol='white', staplecol = 'white')
barplot(user_data_pca , horiz = T, add = T, space = 2, ylim = c(1,16), xpd = T, col = adjustcolor('grey', alpha = 0.4),xlim = c(0,4))
# axis(side=2, at = 0:16)
pop_agV <- sapply(all_data_pca, function(x) {q <- quantile(x,prob=c(0.20,0.80), type=7)
                                             m <- mean(x)
                                             return(c(q[1], mean = m, q[2]))})
pop_agV
mapply( function(x, y) lines(rep(x, 2), y=c(-1,1)+y, lwd=3), pop_agV[2,],c(2.5+(0:4)*3))
mapply( function(x, y) lines(rep(x, 2), y=c(-1,1)*0.7+y, lwd=2.5, col = 'red'), pop_agV[1,],c(2.5+(0:4)*3))
mapply( function(x, y) lines(rep(x, 2), y=c(-1,1)*0.7+y, lwd=2.5, col = 'red'), pop_agV[3,],c(2.5+(0:4)*3))

axis(side=2, at = 0:16)
barplot( pop_agV, horiz = T, add=F, ylim = c(1,16), col = adjustcolor(c("yellow", "orange", "red"), alpha = 0.8), xlim = c(0,4), width = 0.6, space = c(0,2), beside=T )
barplot(user_data_pca , horiz = T, add = T, space = 2, ylim = c(1,16), xpd = T, col = adjustcolor('grey', alpha = 0.4),xlim = c(0,4), names.arg=NA)

barplot( pop_agV, horiz = T, add=F, ylim = c(1,20), col = adjustcolor(c("yellow", "orange", "red"), alpha = 0.5), xlim = c(0,4), width = 1, space = c(0,1), beside=T )
barplot(user_data_pca , horiz = T, add = T, width = 3, space = 1/3, ylim = c(1,20), xpd = T, col = adjustcolor('grey', alpha = 0.6),xlim = c(0,4), names.arg=NA)

barplot( rbind(pop_agV,user_data_pca ), horiz = T, add=F, col = adjustcolor(c("yellow", "orange", "red", "black"), alpha = 0.5), xlim = c(0,4), width = 1, space = c(0,1), beside=T )

barplot( pop_agV, horiz = T, add=F, ylim = c(1,20), col = adjustcolor(c("yellow", "orange", "red"), alpha = 0.5), xlim = c(0,4), width = 1, space = c(0,1), beside=T )
mapply( function(x, y) lines(c(0,x), y=rep(y,2), lwd=8, col = adjustcolor("black", alpha = 0.6)), user_data_pca , c(2.5+0:4*4))

barplot( pop_agV, horiz = T, add=F, ylim = c(1,20), col = adjustcolor(c("yellow", "orange", "red"), alpha = 0.5), xlim = c(0,4), width = 1, space = c(0,1), beside=T, border = NA )
mapply( function(x, y) lines(c(0,x), y=rep(y,2), lwd=60, col = adjustcolor("black", alpha = 0.2)), user_data_pca , c(2.5+0:4*4))

barplot( rbind(pop_agV[1,], apply(pop_agV, 2, diff)), horiz = T, add=F, col = adjustcolor(c("blue", "magenta", "red"), alpha = 0.7), xlim = c(0,4), width = 1, space = 0.4, beside=F, border = NA )
barplot( rbind(pop_agV[1,], apply(pop_agV, 2, diff)), horiz = T, add=F, col = adjustcolor(c("red", "orange", "yellow"), alpha = 0.7), xlim = c(0,4), width = 1, space = 0.4, beside=F, border = NA )
barplot( rbind(pop_agV[1,], apply(pop_agV, 2, diff)), horiz = T, add=F, col = adjustcolor(c("black", "grey", "grey90"), alpha = 0.5), xlim = c(0,4), width = 1, space = 0.4, beside=F, border = NA )
mapply( function(x, y) lines(c(0,x), y=rep(y,2), lwd=15, col = adjustcolor("black", alpha = 1)), user_data_pca , c(0.9+0:4*1.4))

offsetF <- -0.04
barplot( rbind(pop_agV[1,]-offsetF, apply(pop_agV, 2, diff)), horiz = T, add=F, col = adjustcolor(c("black", "grey", "grey90"), alpha = 0.5), xlim = c(-0.2,4), width = 1, space = 0.4, beside=F, border = NA, offset = offsetF )
mapply( function(x, y) lines(c(offsetF,x), y=rep(y,2), lwd=15, lend = 1, col = adjustcolor("black", alpha = 1), ), user_data_pca , c(0.9+0:4*1.4))

group1.ordered = unique(sort(all_data_pca$group1))
n = (3312 - cumsum(table(all_data_pca$group1)))/length(all_data_pca$group1)
plot(group1.ordered, n, type = 'l', ylim = c(-1, 1), xaxt = 'n')
points(group1.ordered, -n, type = 'l')
rect(0, -0.1, user_data_pca[1], 0.1, col = 'grey', border = NA)

plot(lowess(group1.ordered, n, iter=5, f=0.3), type = 'l', ylim = c(-1, 1), xaxt = 'n')
points(lowess(group1.ordered, -n, iter=5, f=0.3), type = 'l', ylim = c(-1, 1), xaxt = 'n')
rect(0, -0.1, user_data_pca[1], 0.1, col = 'grey', border = NA)
