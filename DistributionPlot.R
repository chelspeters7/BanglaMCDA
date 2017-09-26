par(mfrow=c(4,5), oma = c(2, 2, 2, 2), mar=c(1.7,1.7,1.7,1.7))
for (h in 1:18){
  hist(beh.x[h,], col=rgb(1,0,0,0.5), main=criteria[h], #rgb(1,0,0,0.5)
       xlab="", xlim = c(0,1.5), freq = FALSE)
  hist(nbeh.x[h,], col=rgb(0,0,1,0.5), #rgb(0,0,1,0.5), 
       add=T, xlim = c(0,1.5), freq = FALSE, breaks = 10)
  box()
}
plot.new()
legend(x = "top",  
       legend = c("Behavior", "Nonbehavior"), 
       col=c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)), lwd=5, cex=1, horiz = FALSE)

# Testing weighting distributions
p <- array(0,dim = c(18))
for (tt in 1:18){
  x <- beh.x [tt,]
  y <- nbeh.x[tt,]
  # Do x and y come from the same distribution? Where the null hypothesis is that x and y are drawn from the same continuous distribution.
  # The smaller the value of p, the less likely x = y. 
  ks <- ks.test(x, y)
  p[tt]<- ks$p.value 
}
q <- quantile(p, probs = weightvalue)
#p[which(p>q)] <- NA
#============================================================
criteria3 <- weights.org
criteria3 <- mutate(criteria3, pvalue = p )
criteria3 <- arrange(criteria3, pvalue)

#criteria3$local <- criteria3$local*p
#criteria3$aca <- criteria3$aca*p
#criteria3$ngo <- criteria3$ngo*p
#which.sig.w <- apply(criteria3$local, 1, is.finite)
#which.sig.w <- which(which.sig.w == TRUE, arr.ind = TRUE)
which.sig.w <- array(NA, dim = 3)
for (count in 1: 3){
which.sig.w[count] <- which(weights.org$criteria == criteria3$criteria[count])
}
