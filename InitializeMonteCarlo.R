# Initial Monte Carlo Simulation


#===========================================================================================
# Create three-dimensional array (5 alternatives x 18 criteria x n monte carlo runs)
my_array <- array(0,dim=c(5,18,n+1))
rownames(my_array) <- alternatives
colnames(my_array) <- criteria
# fill datacube with random numbers pulled from normal distribution
for (i in 1:dim(my_array)[1]) { # for every alternative
  for (j in 1:dim(my_array)[2]) { # for every criteria
    my_array[i,j,1] <-  performanceMatrix.org[i,j] # fill first matrix with original run
    my_array[i,j,-(1)] = rnorm(n,mean=performanceMatrix.org[i,j], 
                               sd = apply(performanceMatrix.org, 2, sd))
  }
}
#===========================================================================================
# Create two-dimensional array for weights
my_weights_local <- array(0,dim=c(18,n+1))
my_weights_ngo <- array(0,dim=c(18,n+1))
my_weights_aca <- array(0,dim=c(18,n+1))
# fill datacube with random numbers pulled from normal distribution
for (i in 1:dim(my_weights_local)[1]) {
  my_weights_local[i,1] <- weights.org$local[i]
  my_weights_ngo[i,1] <- weights.org$ngo[i]
  my_weights_aca[i,1] <- weights.org$aca[i]
  my_weights_local[i,-1] = rnorm(n,mean=weights.org$local[i], sd = sd(weights.org$local))
  my_weights_ngo[i,-1] = rnorm(n,mean=weights.org$ngo[i], sd = sd(weights.org$ngo))
  my_weights_aca[i,-1] = rnorm(n,mean=weights.org$aca[i], sd = sd(weights.org$aca))
}

#===========================================================================================
rank.local <- array(0, dim = c(5,n+1))
rank.ngo <- array(0, dim = c(5,n+1))
rank.aca <- array(0, dim = c(5,n+1))