# pop_agV <- sapply(all_data_pca2, function(x) {q <- quantile(x,prob=c(0.20, 0.50, 0.80), type=7)
#                                              return(q)})

# plot 1-1
png('plot1-1.png', w=900, h=400)
par(mfrow = c(1,1), mar=c(2,8,0.5,1) + 0.1, cex = 2, mgp=c(2,0.7,0))
# par(mfrow = c(1,1), mar=c(3,6,2,2) + 0.1)
offsetF <- -0.04
graphColors <- c(adjustcolor(c("grey50", "grey", "grey90"), alpha = 0.65), "black")
barplot( rbind(pop_agV[1,]-offsetF, apply(pop_agV, 2, diff)), 
         horiz = T, add=F, col = graphColors[1:3], 
         xlim = c(-0.2,4), width = 1, space = 0.4, beside=F, border = NA, offset = offsetF, las =1 )
mapply( function(x, y) lines(c(offsetF,x), y=rep(y,2), lwd=15, lend = 1, col = graphColors[4] ), user_data_pca , c(0.9+0:4*1.4))
legend('right', legend = c('20%', '50%', '80%', 'Your result'), fill = graphColors, cex = 0.6)
dev.off()

# plot 1-2
barplot( rbind(pop_agV[1,]-offsetF, apply(pop_agV, 2, diff)), 
         horiz = T, add=F, col = adjustcolor(c("black", "grey", "grey90"), alpha = 0.7), 
         xlim = c(-0.2,4), width = 1, space = 0.4, beside=F, border = NA, offset = offsetF, las =1 )
mapply( function(x, y) lines(x = rep(x, 2), y=c(y-0.8,y+0.8), lwd=10, lend = 1, col = adjustcolor("black", alpha = 1), ), 
        user_data_pca , c(0.9+0:4*1.4))
legend('right', legend = c('20%', '50%', '80%', 'Your result'), fill = c("#E5E5E580", "#BEBEBE80", "#00000080", "black"), cex = 0.6)


# plot 1-3
png('plot1-3.png', w=900, h=400)
par(mfrow = c(1,1), mar=c(2,8,0.5,1) + 0.1, cex = 2, mgp=c(2,0.7,0))
barplot( rbind(pop_agV[1,]-offsetF, apply(pop_agV, 2, diff)), 
         horiz = T, add=F, col = adjustcolor(c("black", "grey", "grey90"), alpha = 0.7), 
         xlim = c(-0.2,4), width = 1, space = 0.4, beside=F, border = NA, offset = offsetF, las =1 )
mapply( function(x, y, z) {
  if (x <= z){
    lines(x = rep(x, 2), y=c(y-0.8,y-0.5), lwd=10, lend = 1, col = adjustcolor("black", alpha = 1), )
    lines(x = rep(x, 2), y=c(y+0.8,y+0.5), lwd=10, lend = 1, col = adjustcolor("black", alpha = 1), )
    lines(x = rep(x, 2), y=c(y-0.8,y+0.5), lwd=10, lend = 1, col = adjustcolor("black", alpha = 0.3), )
  } else {
    lines(x = rep(x, 2), y=c(y-0.8,y+0.8), lwd=10, lend = 1, col = adjustcolor("black", alpha = 1), )
  }
}, user_data_pca , c(0.9+0:4*1.4), pop_agV[3,])
legend('right', legend = c('20%', '50%', '80%', 'Your result'), fill = c("#E5E5E580", "#BEBEBE80", "#00000080", "black"), cex = 0.6)
dev.off()

# plot 2-1
barplot( pop_agV, horiz = T, add=F, ylim = c(1,20), col = adjustcolor(c("yellow", "orange", "red"), alpha = 0.5), xlim = c(0,4), width = 1, space = c(0,1), beside=T, las = 1, border = NA)
barplot(user_data_pca , horiz = T, add = T, width = 3, space = 1/3, ylim = c(1,20), xpd = T, col = adjustcolor('grey', alpha = 0.4),xlim = c(0,4), names.arg=NA, border = NA)
legend('right', legend = c('20%', '50%', '80%', 'Your result'), fill = c("#FFFF0080", "#FFA50080", "#FF000080", "#BEBEBE99"), cex = 0.6)

# plot 2-2
barplot( pop_agV, horiz = T, add=F, ylim = c(1,20), col = adjustcolor(c("yellow", "orange", "red"), alpha = 0.5), xlim = c(0,4), width = 1, space = c(0,1), beside=T , las =1, border = NA)
mapply( function(x, y){
  lines(x = rep(x, 2), y=c(y-1.8,y+1.8), lwd=10, lend = 1, col = adjustcolor("black", alpha = 1), )    
}, user_data_pca , c(2.5+0:4*4))
legend('right', legend = c('20%', '50%', '80%', 'Your result'), fill = c("#FFFF0080", "#FFA50080", "#FF000080", "black"), cex = 0.6)

# plot 2-3
png('plot2-3.png', w=900, h=400)
par(mfrow = c(1,1), mar=c(2,8,0.5,1) + 0.1, cex = 2, mgp=c(2,0.7,0))
pop_Agv1 <- pop_agV; pop_Agv1[,5] <- 0
pop_Agv2 <- pop_agV; pop_Agv2[,1:4] <- 0
barplot( pop_Agv1, horiz = T, add=F, ylim = c(1,20), col = adjustcolor(c("yellow", "orange", "red"), alpha = 0.5), xlim = c(0,4), width = 1, space = c(0,1), beside=T , las =1, border = NA, 
         names.arg=paste(colnames(pop_Agv1), round(user_data_pca+0.00001,1), sep='   '))
barplot( pop_Agv2, horiz = T, add=T, ylim = c(1,20), col = adjustcolor(c("red", "orange", "yellow"), alpha = 0.5), xlim = c(0,4), width = 1, xpd = T, names.arg=NA, space = c(0,1), beside=T , las =1, border = NA)
barplot(user_data_pca , horiz = T, add = T, width = 3, space = 1/3, ylim = c(1,20), xpd = T, col = adjustcolor('grey', alpha = 0.4),xlim = c(0,4), names.arg=NA, border = NA)
mapply( function(x, y){
  lines(x = rep(x, 2), y=c(y-1.8,y+1.8), lwd=7, lend = 1, col = adjustcolor("black", alpha = 1), )    
}, user_data_pca , c(2.5+0:4*4))
legend('right', legend = c('20%', '50%', '80%', 'Your result'), fill = c("#FFFF0080", "#FFA50080", "#FF000080", "black"), cex = 0.7)#, inset = -0.02)
dev.off()

# plot 2-3
barplot( pop_agV, horiz = T, add=F, ylim = c(1,20), col = adjustcolor(c("yellow", "orange", "red"), alpha = 0.5), xlim = c(0,4), width = 1, space = c(0,1), beside=T, las =1 )
mapply( function(x, y, a, b, c) {
  if (x > c) {
    lines(x = rep(x, 2), y=c(y-1.8,y+1.8), lwd=10, lend = 1, col = adjustcolor("black", alpha = 1), )    
  } else {
    lines(x = rep(x, 2), y=c(y-1.8,y-1.5), lwd=10, lend = 1, col = adjustcolor("black", alpha = 1), )
    lines(x = rep(x, 2), y=c(y+1.8,y+1.5), lwd=10, lend = 1, col = adjustcolor("black", alpha = 1), )
    lines(x = rep(x, 2), y=c(y+1.5,y+0.5), lwd=10, lend = 1, col = adjustcolor("black", alpha = 0.3), )    
  }
  if (x > b){
    lines(x = rep(x, 2), y=c(y-0.5,y+0.5), lwd=10, lend = 1, col = adjustcolor("black", alpha = 1), )
  } else {
    lines(x = rep(x, 2), y=c(y-0.5,y+0.5), lwd=10, lend = 1, col = adjustcolor("black", alpha = 0.3), )
  }
  if (x > a){
    lines(x = rep(x, 2), y=c(y-0.5,y-1.5), lwd=10, lend = 1, col = adjustcolor("black", alpha = 1), )
  } else {
    lines(x = rep(x, 2), y=c(y-0.5,y-1.5), lwd=10, lend = 1, col = adjustcolor("black", alpha = 0.3), )    
  }
}, user_data_pca , c(2.5+0:4*4), pop_agV[1,], pop_agV[2,], pop_agV[3,])
legend('right', legend = c('20%', '50%', '80%', 'Your result'), fill = c("#FFFF0080", "#FFA50080", "#FF000080", "black"), cex = 0.6)





par(mfrow = c(5, 1), mar=c(0,6,0,2) + 0.1)


flame = function(data, user_data, name)
{
  group.ordered = unique(sort(data))
  n = (length(data) - cumsum(table(data)))/length(data)
  plot(group.ordered, n, type = 'l', ylim = c(-1, 1), xaxt = 'n', yaxt = 'n', ylab = name, frame.plot=F, las = 0)
  points(group.ordered, -n, type = 'l')
  index20 = max(which(n >= quantile(n, 0.2)))
  lines(c(0, group.ordered[index20]), rep(quantile(n, 0.2), 2), lty = 2, col = 'red')
  index50 = max(which(n >= quantile(n, 0.5)))
  lines(c(0, group.ordered[index50]), rep(quantile(n, 0.5), 2), lty = 2, col = 'orange')
  index80 = max(which(n >= quantile(n, 0.8)))
  lines(c(0, group.ordered[index80]), rep(quantile(n, 0.8), 2), lty = 2, col = 'yellow')  
  index = which(group.ordered >=  user_data)[1]
  height = n[index]
  lines(c(0, user_data), c(height, height), col = adjustcolor('grey', alpha = 0.5), lwd = 2)
  abline(v = user_data, col = 'black', lwd = 2)
}
flame(all_data_pca$RelativePain, user_data_pca[4], 'Relative pain')
flame(all_data_pca$EnjoyLife, user_data_pca[5], 'Enjoy life')
flame(all_data_pca$group1, user_data_pca[1], 'Group 1')
legend('right', legend = c('20%', '50%', '80%', 'Your result'), fill = c("#FFFF0080", "#FFA50080", "#FF000080", "black"), cex = 0.6)
flame(all_data_pca$group2, user_data_pca[1], 'Group 2')
flame(all_data_pca$group3, user_data_pca[1], 'Group 3')
