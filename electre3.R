#ELECTRE III, Chelsea Peters, 6/9/2017

# Electre3_SimpleThresholds(performanceMatrix,
#                           alternatives,
#                           criteria,
#                           minmaxcriteria,
#                           weights$local,
#                           IndifferenceThresholds,
#                           PreferenceThresholds,
#                           VetoThresholds,
#                           mode_def)

# ----------------------------------------
# Needed for permutations
combs <- permutations(n=5,r=2,v=alternatives)
# ----------------------------------------
# Concordance
concord = data.frame(matrix(nrow = length(combs[,1]), ncol = 18))
colnames(concord) = colnames(performanceMatrix)

for (j in 1:20){ # for each permutation
  for (i in 1:18){ # for each criteria
    A <- performanceMatrix[which(rownames(performanceMatrix)==combs[j,1]),i]
    B <- performanceMatrix[which(rownames(performanceMatrix)==combs[j,2]),i]
    C <- B/(IndifferenceThresholds[i]-PreferenceThresholds[i]) + (1-(A+IndifferenceThresholds[i])/(IndifferenceThresholds[i]-PreferenceThresholds[i]))
    if(A+IndifferenceThresholds[i] >= B){concord[j,i] <- 1.0*weights$local[i]}
    if(A+PreferenceThresholds[i] <= B){concord[j,i] <- 0.0*weights$local[i]} 
    if(is.na(concord[j,i])== TRUE){concord[j,i] <- C*weights$local[i]}
  }}

concord_sum <- rowSums(concord)/sum(weights$local) # sum the weights for each permutation
concordance <-data.frame(matrix(nrow = 5, ncol = 5)) # create empty concordance matrix
rownames(concordance) = alternatives
colnames(concordance) = alternatives
cc <- combs[which(concord_sum>mean(concord_sum)),] # determine signifcant values compared to mean of all columns

for(k in 1:nrow(cc)){ # compile significant concordance values
  q = which(rownames(concordance) == cc[k,1])
  p = which(colnames(concordance) == cc[k,2])
  concordance[q,p] <- "TRUE"
}

# ----------------------------------------
# Discordance
discord = data.frame(matrix(nrow = length(combs[,1]), ncol = 18)) 
colnames(discord) = colnames(performanceMatrix)

for (j in 1:20){ # for each permutation
  for (i in 1:18){ # for each criteria
    A <- performanceMatrix[which(rownames(performanceMatrix)==combs[j,1]),i]
    B <- performanceMatrix[which(rownames(performanceMatrix)==combs[j,2]),i]
    C <- ( B - A - PreferenceThresholds[i] )/(VetoThresholds[i]-PreferenceThresholds[i])
      
    if(B>(A+VetoThresholds[i])){discord[j,i] <- 1.0*weights$local[i]}
    if(B<=(A+PreferenceThresholds[i])){discord[j,i] <- 0.0*weights$local[i]} 
    if(is.na(discord[j,i])== TRUE){discord[j,i] <- C*weights$local[i]}
  }}


discord_sum <- rowSums(discord)/sum(weights$local) # sum the weights for each permutation
discordance <-data.frame(matrix(nrow = 5, ncol = 5)) # create empty discordance matrix
rownames(discordance) = alternatives
colnames(discordance) = alternatives
dd <- combs[which(discord_sum<mean(discord_sum)),] # compile signifcant discordance values

for(k in 1:nrow(dd)){
  q = which(rownames(discordance) == dd[k,1])
  p = which(colnames(discordance) == dd[k,2])
  discordance[q,p] <- "TRUE"
}
# ----------------------------------------
# Merged
a <- rep(NA,20)
true_concord <- a
true_discord <- a
true_concord[which(concord_sum>mean(concord_sum))] <- 'TRUE'
true_discord[which(discord_sum<mean(discord_sum))] <- 'TRUE'
local_electre3 <- data.frame(combs, concord_sum, discord_sum, true_concord, true_discord)
# ----------------------------------------
# Credibility Index
credibility <-rep(NA,20)
for (i in 1:20) {
  if(local_electre3$discord_sum[i]<local_electre3$concord_sum[i]){credibility[i] <- local_electre3$concord_sum[i]}
}
credibility2 <- (credibility-median(credibility))/diff(range(credibility))
local_electre3 <- data.frame(combs, concord_sum, discord_sum, true_concord, true_discord, credibility,credibility2)
credibility3 <-data.frame(matrix(nrow = 5, ncol = 5)) # create empty discordance matrix
rownames(credibility3) = alternatives
colnames(credibility3) = alternatives

for(k in 1:20){
  q = which(rownames(credibility3) ==local_electre3[k,1])
  p = which(colnames(credibility3) == local_electre3[k,2])
  credibility3[q,p] <- local_electre3$credibility[k]
}

local_electre3_value <- (rowSums(credibility3,na.rm = TRUE)-median(rowSums(credibility3,na.rm = TRUE)))/diff(range(rowSums(credibility3,na.rm = TRUE)))

# ----------------------------------------
# ----------------------------------------
# ----------------------------------------
# ----------------------------------------
# Concordance
concord = data.frame(matrix(nrow = length(combs[,1]), ncol = 18))
colnames(concord) = colnames(performanceMatrix)

for (j in 1:20){ # for each permutation
  for (i in 1:18){ # for each criteria
    A <- performanceMatrix[which(rownames(performanceMatrix)==combs[j,1]),i]
    B <- performanceMatrix[which(rownames(performanceMatrix)==combs[j,2]),i]
    C <- B/(IndifferenceThresholds[i]-PreferenceThresholds[i]) + (1-(A+IndifferenceThresholds[i])/(IndifferenceThresholds[i]-PreferenceThresholds[i]))
    if(A+IndifferenceThresholds[i] >= B){concord[j,i] <- 1.0*weights$ngo[i]}
    if(A+PreferenceThresholds[i] <= B){concord[j,i] <- 0.0*weights$ngo[i]} 
    if(is.na(concord[j,i])== TRUE){concord[j,i] <- C*weights$ngo[i]}
  }}

concord_sum <- rowSums(concord)/sum(weights$ngo) # sum the weights for each permutation
concordance <-data.frame(matrix(nrow = 5, ncol = 5)) # create empty concordance matrix
rownames(concordance) = alternatives
colnames(concordance) = alternatives
cc <- combs[which(concord_sum>mean(concord_sum)),] # determine signifcant values compared to mean of all columns

for(k in 1:nrow(cc)){ # compile significant concordance values
  q = which(rownames(concordance) == cc[k,1])
  p = which(colnames(concordance) == cc[k,2])
  concordance[q,p] <- "TRUE"
}

# ----------------------------------------
# Discordance
discord = data.frame(matrix(nrow = length(combs[,1]), ncol = 18)) 
colnames(discord) = colnames(performanceMatrix)

for (j in 1:20){ # for each permutation
  for (i in 1:18){ # for each criteria
    A <- performanceMatrix[which(rownames(performanceMatrix)==combs[j,1]),i]
    B <- performanceMatrix[which(rownames(performanceMatrix)==combs[j,2]),i]
    C <- ( B - A - PreferenceThresholds[i] )/(VetoThresholds[i]-PreferenceThresholds[i])
    
    if(B>(A+VetoThresholds[i])){discord[j,i] <- 1.0*weights$ngo[i]}
    if(B<=(A+PreferenceThresholds[i])){discord[j,i] <- 0.0*weights$ngo[i]} 
    if(is.na(discord[j,i])== TRUE){discord[j,i] <- C*weights$ngo[i]}
  }}


discord_sum <- rowSums(discord)/sum(weights$ngo) # sum the weights for each permutation
discordance <-data.frame(matrix(nrow = 5, ncol = 5)) # create empty discordance matrix
rownames(discordance) = alternatives
colnames(discordance) = alternatives
dd <- combs[which(discord_sum<mean(discord_sum)),] # compile signifcant discordance values

for(k in 1:nrow(dd)){
  q = which(rownames(discordance) == dd[k,1])
  p = which(colnames(discordance) == dd[k,2])
  discordance[q,p] <- "TRUE"
}
# ----------------------------------------
# Merged
a <- rep(NA,20)
true_concord <- a
true_discord <- a
true_concord[which(concord_sum>mean(concord_sum))] <- 'TRUE'
true_discord[which(discord_sum<mean(discord_sum))] <- 'TRUE'
ngo_electre3 <- data.frame(combs, concord_sum, discord_sum, true_concord, true_discord)
# ----------------------------------------
# Credibility Index
credibility <-rep(NA,20)
for (i in 1:20) {
  if(ngo_electre3$discord_sum[i]<ngo_electre3$concord_sum[i]){credibility[i] <- ngo_electre3$concord_sum[i]}
}
credibility2 <- (credibility-median(credibility))/diff(range(credibility))
ngo_electre3 <- data.frame(combs, concord_sum, discord_sum, true_concord, true_discord, credibility,credibility2)
credibility3 <-data.frame(matrix(nrow = 5, ncol = 5)) # create empty discordance matrix
rownames(credibility3) = alternatives
colnames(credibility3) = alternatives

for(k in 1:20){
  q = which(rownames(credibility3) ==ngo_electre3[k,1])
  p = which(colnames(credibility3) == ngo_electre3[k,2])
  credibility3[q,p] <- ngo_electre3$credibility[k]
}

ngo_electre3_value <- (rowSums(credibility3,na.rm = TRUE)-median(rowSums(credibility3,na.rm = TRUE)))/diff(range(rowSums(credibility3,na.rm = TRUE)))

# ----------------------------------------
# ----------------------------------------
# ----------------------------------------
# ----------------------------------------
# Concordance
concord = data.frame(matrix(nrow = length(combs[,1]), ncol = 18))
colnames(concord) = colnames(performanceMatrix)

for (j in 1:20){ # for each permutation
  for (i in 1:18){ # for each criteria
    A <- performanceMatrix[which(rownames(performanceMatrix)==combs[j,1]),i]
    B <- performanceMatrix[which(rownames(performanceMatrix)==combs[j,2]),i]
    C <- B/(IndifferenceThresholds[i]-PreferenceThresholds[i]) + (1-(A+IndifferenceThresholds[i])/(IndifferenceThresholds[i]-PreferenceThresholds[i]))
    if(A+IndifferenceThresholds[i] >= B){concord[j,i] <- 1.0*weights$aca[i]}
    if(A+PreferenceThresholds[i] <= B){concord[j,i] <- 0.0*weights$aca[i]} 
    if(is.na(concord[j,i])== TRUE){concord[j,i] <- C*weights$aca[i]}
  }}

concord_sum <- rowSums(concord)/sum(weights$aca) # sum the weights for each permutation
concordance <-data.frame(matrix(nrow = 5, ncol = 5)) # create empty concordance matrix
rownames(concordance) = alternatives
colnames(concordance) = alternatives
cc <- combs[which(concord_sum>mean(concord_sum)),] # determine signifcant values compared to mean of all columns

for(k in 1:nrow(cc)){ # compile significant concordance values
  q = which(rownames(concordance) == cc[k,1])
  p = which(colnames(concordance) == cc[k,2])
  concordance[q,p] <- "TRUE"
}

# ----------------------------------------
# Discordance
discord = data.frame(matrix(nrow = length(combs[,1]), ncol = 18)) 
colnames(discord) = colnames(performanceMatrix)

for (j in 1:20){ # for each permutation
  for (i in 1:18){ # for each criteria
    A <- performanceMatrix[which(rownames(performanceMatrix)==combs[j,1]),i]
    B <- performanceMatrix[which(rownames(performanceMatrix)==combs[j,2]),i]
    C <- ( B - A - PreferenceThresholds[i] )/(VetoThresholds[i]-PreferenceThresholds[i])
    
    if(B>(A+VetoThresholds[i])){discord[j,i] <- 1.0*weights$aca[i]}
    if(B<=(A+PreferenceThresholds[i])){discord[j,i] <- 0.0*weights$aca[i]} 
    if(is.na(discord[j,i])== TRUE){discord[j,i] <- C*weights$aca[i]}
  }}


discord_sum <- rowSums(discord)/sum(weights$aca) # sum the weights for each permutation
discordance <-data.frame(matrix(nrow = 5, ncol = 5)) # create empty discordance matrix
rownames(discordance) = alternatives
colnames(discordance) = alternatives
dd <- combs[which(discord_sum<mean(discord_sum)),] # compile signifcant discordance values

for(k in 1:nrow(dd)){
  q = which(rownames(discordance) == dd[k,1])
  p = which(colnames(discordance) == dd[k,2])
  discordance[q,p] <- "TRUE"
}
# ----------------------------------------
# Merged
a <- rep(NA,20)
true_concord <- a
true_discord <- a
true_concord[which(concord_sum>mean(concord_sum))] <- 'TRUE'
true_discord[which(discord_sum<mean(discord_sum))] <- 'TRUE'
aca_electre3 <- data.frame(combs, concord_sum, discord_sum, true_concord, true_discord)
# ----------------------------------------
# Credibility Index
credibility <-rep(NA,20)
for (i in 1:20) {
  if(aca_electre3$discord_sum[i]<aca_electre3$concord_sum[i]){credibility[i] <- aca_electre3$concord_sum[i]}
}


credibility2 <- (credibility-median(credibility))/diff(range(credibility))
aca_electre3 <- data.frame(combs, concord_sum, discord_sum, true_concord, true_discord, credibility,credibility2)

credibility3 <-data.frame(matrix(nrow = 5, ncol = 5)) # create empty discordance matrix
rownames(credibility3) = alternatives
colnames(credibility3) = alternatives

for(k in 1:20){
  q = which(rownames(credibility3) ==aca_electre3[k,1])
  p = which(colnames(credibility3) == aca_electre3[k,2])
  credibility3[q,p] <- aca_electre3$credibility[k]
}

aca_electre3_value <- (rowSums(credibility3,na.rm = TRUE)-median(rowSums(credibility3,na.rm = TRUE)))/diff(range(rowSums(credibility3,na.rm = TRUE)))

