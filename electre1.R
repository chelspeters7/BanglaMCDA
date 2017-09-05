#ELECTRE I, Chelsea Peters, 6/9/2017

# Electre_1(performanceMatrix,
#           alternatives,
#           criteria,
#           weights$ngo,
#           minmaxcriteria,
#           concordance_threshold=5.13,discordance_threshold=95)
# ----------------------------------------
# Needed for permutations
combs <- permutations(n=5,r=2,v=alternatives)
# ----------------------------------------
# Concordance
concord = data.frame(matrix(nrow = length(combs[,1]), ncol = 18))
colnames(concord) = colnames(performanceMatrix)
for (j in 1:20){ # for each permutation
  for (i in 1:18){ # for each criteria
    ifelse(performanceMatrix[combs[j,1],i]>performanceMatrix[combs[j,2],i], concord[j,i]<-weights$local[i], concord[j,i] <- 0.0)
  }} # fill with weight
concord_sum <- rowSums(concord) # sum the weights for each permutation
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
for (j in 1:20){
  for (i in 1:18){
    ifelse(performanceMatrix[combs[j,1],i]<performanceMatrix[combs[j,2],i], discord[j,i]<-(performanceMatrix[combs[j,2],i]-performanceMatrix[combs[j,1],i])/100.0, discord[j,i] <- 0.0)
  }} # if not better peforming, find the difference and divide by overall difference
discord_sum <- rowSums(discord) # find the maximum difference in each permutation
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
local_electre1 <- data.frame(combs, concord_sum, discord_sum, true_concord, true_discord)
# ----------------------------------------
# Credibility Index
credibility <-rep(NA,20)
for (i in 1:20) {
  if(local_electre1$discord_sum[i]<local_electre1$concord_sum[i]){credibility[i] <- local_electre1$concord_sum[i]}
}
local_electre1 <- data.frame(combs, concord_sum, discord_sum, true_concord, true_discord, credibility)

credibility3 <-data.frame(matrix(nrow = 5, ncol = 5)) # create empty discordance matrix
rownames(credibility3) = alternatives
colnames(credibility3) = alternatives
for(k in 1:20){
  q = which(rownames(credibility3) ==local_electre1[k,1])
  p = which(colnames(credibility3) == local_electre1[k,2])
  credibility3[q,p] <- local_electre1$credibility[k]
}
local_electre1_value <- (rowSums(credibility3,na.rm = TRUE)-median(rowSums(credibility3,na.rm = TRUE)))/diff(range(rowSums(credibility3,na.rm = TRUE)))


# ----------------------------------------
# ----------------------------------------
# ----------------------------------------
# ----------------------------------------
# Concordance
concord = data.frame(matrix(nrow = length(combs[,1]), ncol = 18))
for (j in 1:20){ # for each permutation
  for (i in 1:18){ # for each criteria
    ifelse(performanceMatrix[combs[j,1],i]>performanceMatrix[combs[j,2],i], concord[j,i]<-weights$ngo[i], concord[j,i] <- 0.0)
  }} # fill with weight
concord_sum <- rowSums(concord) # sum the weights for each permutation
concordance <-data.frame(matrix(nrow = 5, ncol = 5)) # create empty concordance matrix
cc <- combs[which(concord_sum>mean(concord_sum)),] # determine signifcant values compared to mean of all columns

for(k in 1:nrow(cc)){ # compile significant concordance values
  q = which(rownames(concordance) == cc[k,1])
  p = which(colnames(concordance) == cc[k,2])
  concordance[q,p] <- "TRUE"
}

# ----------------------------------------
# Merged
a <- rep(NA,20)
true_concord <- a
true_discord <- a
true_concord[which(concord_sum>mean(concord_sum))] <- 'TRUE'
true_discord[which(discord_sum<mean(discord_sum))] <- 'TRUE'
ngo_electre1 <- data.frame(combs, concord_sum, discord_sum, true_concord, true_discord)
# ----------------------------------------
# Credibility Index
credibility <-rep(NA,20)
for (i in 1:20) {
  if(ngo_electre1$discord_sum[i]<ngo_electre1$concord_sum[i]){credibility[i] <- ngo_electre1$concord_sum[i]}
}
ngo_electre1 <- data.frame(combs, concord_sum, discord_sum, true_concord, true_discord, credibility)

credibility3 <-data.frame(matrix(nrow = 5, ncol = 5)) # create empty discordance matrix
rownames(credibility3) = alternatives
colnames(credibility3) = alternatives
for(k in 1:20){
  q = which(rownames(credibility3) ==ngo_electre1[k,1])
  p = which(colnames(credibility3) == ngo_electre1[k,2])
  credibility3[q,p] <- ngo_electre1$credibility[k]
}
ngo_electre1_value <- (rowSums(credibility3,na.rm = TRUE)-median(rowSums(credibility3,na.rm = TRUE)))/diff(range(rowSums(credibility3,na.rm = TRUE)))


# ----------------------------------------
# ----------------------------------------
# ----------------------------------------
# ----------------------------------------
# Concordance
concord = data.frame(matrix(nrow = length(combs[,1]), ncol = 18))
for (j in 1:20){ # for each permutation
  for (i in 1:18){ # for each criteria
    ifelse(performanceMatrix[combs[j,1],i]>performanceMatrix[combs[j,2],i], concord[j,i]<-weights$aca[i], concord[j,i] <- 0.0)
  }} # fill with weight
concord_sum <- rowSums(concord) # sum the weights for each permutation
concordance <-data.frame(matrix(nrow = 5, ncol = 5)) # create empty concordance matrix
cc <- combs[which(concord_sum>mean(concord_sum)),] # determine signifcant values compared to mean of all columns

for(k in 1:nrow(cc)){ # compile significant concordance values
  q = which(rownames(concordance) == cc[k,1])
  p = which(colnames(concordance) == cc[k,2])
  concordance[q,p] <- "TRUE"
}

# ----------------------------------------
# Merged
a <- rep(NA,20)
true_concord <- a
true_discord <- a
true_concord[which(concord_sum>mean(concord_sum))] <- 'TRUE'
true_discord[which(discord_sum<mean(discord_sum))] <- 'TRUE'
aca_electre1 <- data.frame(combs, concord_sum, discord_sum, true_concord, true_discord)
# ----------------------------------------
# Credibility Index
credibility <-rep(NA,20)
for (i in 1:20) {
  if(aca_electre1$discord_sum[i]<aca_electre1$concord_sum[i]){credibility[i] <- aca_electre1$concord_sum[i]}
}
aca_electre1 <- data.frame(combs, concord_sum, discord_sum, true_concord, true_discord, credibility)

credibility3 <-data.frame(matrix(nrow = 5, ncol = 5)) # create empty discordance matrix
rownames(credibility3) = alternatives
colnames(credibility3) = alternatives
for(k in 1:20){
  q = which(rownames(credibility3) == aca_electre1[k,1])
  p = which(colnames(credibility3) == aca_electre1[k,2])
  credibility3[q,p] <- aca_electre1$credibility[k]
}
aca_electre1_value <- (rowSums(credibility3,na.rm = TRUE)-median(rowSums(credibility3,na.rm = TRUE)))/diff(range(rowSums(credibility3,na.rm = TRUE)))

