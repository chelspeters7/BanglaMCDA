# Plot distributions for scores


for (hh in 1:5){
  par(mfrow=c(4,5),  oma = c(2, 2, 2, 2), mar=c(1.7,1.7,1.7,1.7))
  for (kk in 1:18){
    hist(beh.x[hh,kk,], col=rgb(1,0,0,0.5), main=criteria[kk], 
         ylim = c(0, 0.03), xlim = c(min(beh.local.sco), max(beh.local.sco)), 
         xlab=criteria[kk],  freq = FALSE)
    hist(nbeh.x[hh,kk,], col=rgb(0,0,1,0.5), add=T,
         ylim = c(0, 0.03), xlim = c(min(nbeh.local.sco), max(nbeh.local.sco)), 
         freq = FALSE)
    box()
  }
  plot.new()
  legend(x = "top", legend = c("Behavior", "Nonbehavior"), 
         col=c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)), lwd=5, cex=1, horiz = FALSE)
  
}

#===========================================================================================
# Testing scoring distributions
p <- array(-999.9,dim = c(5,18))
for (tt in 1:5){
  for (qq in 1:18){
    x <- beh.x[tt,qq,]
    y <- nbeh.x[tt,qq,]
    # Do x and y come from the same distribution? Where the null hypothesis is that x and y are drawn from the same continuous distribution.
    # The smaller the value of p, the less likely x = y. 
    ks <- ks.test(x, y)
    p[tt,qq]<- ks$p.value 
  }
}
q <- quantile(p, probs = scorevalue)
#p[which(p>q)] <- NA

#============================================================

performanceMatrix3 <- matrix(order(p), ncol = 18, nrow = 5)
which.sig <- which(performanceMatrix3 <= 3, arr.ind = TRUE)

#performanceMatrix3 <- performanceMatrix
#performanceMatrix3 <- performanceMatrix3*p
#which.sig <- apply(performanceMatrix3, 2, is.finite)
#which.sig <- which(which.sig == TRUE, arr.ind = TRUE)