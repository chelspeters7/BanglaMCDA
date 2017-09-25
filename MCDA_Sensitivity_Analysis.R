# Name: MCDA_Bangladesh.r
# Author: Chelsea Peters, PhD Candidate, Vanderbilt University
# Contact: chelsea.n.peters@vanderbilt.edu, chelspeters7@gmail.com
# Last updated: 9/6/17
# Purpose: MCDA Sensititivy Analysis

# ===========================================================================================================
# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# ===========================================================================================================
# Clear directory
rm(list = ls())

# ===========================================================================================================
# Set Original Data Set
performanceMatrix <- cbind( 
  c(43.9, 73.3, 86.9, 41.7, 94.7),
  c(100.0, 25.0, 75.0, 75.0, 75.0),
  c(97.3, 40.3, 86.4, 64.0, 35.9),
  c(75.0, 100.0, 0.0, 0.0, 75.0),
  c(100.0, 75.0, 50.0, 50.0, 75.0),
  c(96.1, 59.2, 95.4, 40.2, 79.8),
  c(100.0, 25.0, 100.0, 100.0, 25.0),
  c(100.0, 100.0, 97.5, 18.8, 100.0),
  c(100.0, 50.0, 100.0, 100.0, 100.0),
  c(100.0, 75.0, 50.0, 25.0, 75.0),
  c(50.0, 100.0, 75.0, 50.0, 75.0),
  c(75.0, 75.0, 50.0, 0.0, 50.0),
  c(0.6, 70.8, 71.2, 30.0, 8.3),
  c(0.0, 25.0, 25.0, 25.0, 25.0),
  c(93.5, 55.0, 35.0, 3.5, 61.0),
  c(82.9, 74.5, 74.2, 73.3, 25.5),
  c(100.0, 0.0, 25.0, 50.0, 100.0),
  c(100.0, 25.0, 25.0, 50.0, 100.0))
performanceMatrix2 <- t(cbind(performanceMatrix[4,],performanceMatrix[2,],performanceMatrix[5,], performanceMatrix[3,], performanceMatrix[1,]))
row.names(performanceMatrix2) <- c("RWH", "PSF", "TW", "Pond", "MAR")
alternatives <- c("RWH", "PSF", "TW", "Pond", "MAR")

# Uncertainty analysis (parameter importance) (see Triantaphyllou and Sanchez, 1997)
criteria <-c("Variability_in_Supply", "Variability_in_Quality", "Water_Quality", 
             "Maintenance_Requirements", "Failure_Rate", "Construction_Cost", 
             "Potential_for_NGO/Governmental_Help", "Maintenance_Cost", 
             "Transportation_Costs", "Sense_of_Ownership", "Discrimination", 
             "Misinformation_", "Persons_served", "Job_Creation", "Prevelence_of_Source", 
             "Distance_to_Source", "Hazard_Impact", "Resilience")
criteriaWeights_local<- c(0.7, 0.6, 0.7, 0.8, 0.6, 0.9, 1, 1, 0.8, 0.7, 0.6, 0.5, 0.6, 0.4, 0.8, 0.8, 0.5, 0.5)
MAVT_matrix_local <- t(performanceMatrix2)*criteriaWeights_local
MAVT_local <- colSums(MAVT_matrix_local)
Preferences <- rank(MAVT_local)
#================================================
c <- combinations(5,2)
alt1_name <- array(data = NA, dim = 10) 
alt2_name <- alt1_name
Sval <- matrix(data = NA, nrow = 10, ncol = 18)
SvalPer <- matrix(data = NA, nrow = 10, ncol = 18)
W <- matrix(data = NA, nrow = 10, ncol = 18)

for (pairalt in 1:10) {
  alt1_name[pairalt] <- alternatives[which(Preferences == c[pairalt,1])]
  alt2_name[pairalt] <- alternatives[which(Preferences == c[pairalt,2])]
  for (critnum in 1:18) {

    alt1 <- which(row.names(performanceMatrix2)==alt1_name[pairalt])
    alt2 <- which(row.names(performanceMatrix2)==alt2_name[pairalt])

    Sval[pairalt,critnum] <- (MAVT_local[alt2]-MAVT_local[alt1])/(performanceMatrix2[alt2,critnum]-performanceMatrix2[alt1,critnum])
    if(Sval[pairalt,critnum] < criteriaWeights_local[critnum]){W[pairalt,critnum] <- criteriaWeights_local[critnum]-Sval[pairalt,critnum]}
    SvalPer[pairalt,critnum] <- Sval[pairalt,critnum]*100/criteriaWeights_local[critnum]
    }
}
Sval[which(is.na(W))] <- NA
SvalPer[which(is.na(W))] <- NA
colnames(Sval)<- criteria
colnames(SvalPer)<- criteria

#=================================================
mincritchange <- which(abs(SvalPer) == min(abs(SvalPer), na.rm=T),arr.ind=TRUE)
colnames(SvalPer)[mincritchange[2]]
alt1_name[mincritchange[1]]
alt2_name[mincritchange[1]]
paste("The smallest percentage change in criterion weight corresponds to a", min(abs(SvalPer), na.rm=T), "% change in", 
      colnames(SvalPer)[mincritchange[2]], ". This would make", alt2_name[mincritchange[1]], "a higher preference than",
      alt1_name[mincritchange[1]], ".")
mincritchange2 <- which(abs(Sval) == min(abs(Sval), na.rm=T),arr.ind=TRUE)
colnames(Sval)[mincritchange2[2]]
paste("The smallest percentage change in criterion weight corresponds to a", min(abs(Sval), na.rm=T), "change in", 
      colnames(Sval)[mincritchange2[2]], ". This would make", alt2_name[mincritchange2[1]], "a higher preference than",
      alt1_name[mincritchange2[1]], ".")
#================================================

df <- data.frame(alt1_name, alt2_name, SvalPer)

D <- apply(abs(SvalPer),2,min,na.rm =T)
sensC <- 1/D
sensitivitydecisioncritiera <- sensC[order(sensC, decreasing = TRUE)]

write.csv(df, file = "df.csv" )
#================================================
# Sensitivity analysis (parameter sensitivity) 
c <- permutations(5,2)

R <- matrix(data = NA, nrow = 20, ncol = 18)
alt1_name <- array(data = NA, dim = 20) 
alt2_name <- alt1_name

for (pairalt in 1:20) {
alt1_name[pairalt] <- alternatives[which(Preferences == c[pairalt,1])]
alt2_name[pairalt] <- alternatives[which(Preferences == c[pairalt,2])]
for (critnum in 1:18) {
  
  alt1 <- which(row.names(performanceMatrix2)==alt1_name[pairalt])
  alt2 <- which(row.names(performanceMatrix2)==alt2_name[pairalt])
R[pairalt,critnum] <- (MAVT_local[alt1]-MAVT_local[alt2])/criteriaWeights_local[critnum]*100/performanceMatrix2[alt1,critnum]
if(R[pairalt,critnum] > 100) {R[pairalt,critnum] <- NA}
}}
colnames(R)<- criteria
df2 <- data.frame(alt1_name,alt2_name,R)
Table11 <- array(data = NA, dim = c(5,18))
colnames(Table11)<- criteria
rownames(Table11)<- alternatives

Table11[1,]<-apply(abs(R[1:4,]),2,min,na.rm =T)
Table11[2,]<-apply(abs(R[5:8,]),2,min,na.rm =T)
Table11[3,]<-apply(abs(R[9:12,]),2,min,na.rm =T)
Table11[4,]<-apply(abs(R[13:16,]),2,min,na.rm =T)
Table11[5,]<-apply(abs(R[17:20,]),2,min,na.rm =T)

write.csv(df2, file = "df2.csv" )
