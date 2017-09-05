#AHP, Chelsea Peters

technical.local <- t(matrix(c(1, 3, 3, 5, 7, 
                        1/3, 1, 1/5, 5, 7,
                        1/3, 5, 1, 5, 7, 
                        1/5, 1/5, 1/5, 1, 3, 
                        1/7, 1/7, 1/7, 1/3, 1 ), 
                      nrow=5,ncol=5))
colnames(technical.local) = criteria[1:5]
rownames(technical.local) = criteria[1:5]

technical.ngo <- t(matrix(c(1,5,1/3,1/3,1/7,
                            1/5,1,1/7,1/7,3,    
                            3,7,1,1,9,    
                            3,7,1,1,9,    
                            1/7,1/3,1/9,1/9,1), 
                            nrow=5,ncol=5))
colnames(technical.ngo) = criteria[1:5]
rownames(technical.ngo) = criteria[1:5]

technical.aca <- t(matrix(c(1,5,1/5,	1,    	5,    
                            1/5,	1,    	 1/9,	 1/5,	1,    
                            5,    	9,    	1,    	3,    	9,    
                            1,    	5,    	 1/3,	1,    	5,    
                            1/5,	1,    	 1/9,	 1/5,	1),
                            nrow=5,ncol=5))
colnames(technical.aca) = criteria[1:5]
rownames(technical.aca) = criteria[1:5]

# technical2 <- t(t(technical)/colSums(technical))
# total_tech <- rowSums(technical2)
# ave_tech <- rowMeans(technical2)
# cw <- (technical2%*%ave_tech)/ave_tech
# lambda <- sum(cw)
# CI <- (lambda-5)/(5-1)
# Cr_tech <- CI/1.12

priority_technical.local <- rowSums(technical.local)/sum(rowSums(technical.local))
priority_technical.ngo <- rowSums(technical.ngo)/sum(rowSums(technical.ngo))
priority_technical.aca <- rowSums(technical.aca)/sum(rowSums(technical.aca))
# ----------------------------------------
cost.local <- t(matrix(c(1, 1/3, 1, 3, 
                   3, 1, 3, 5, 
                   1, 1/3, 1, 5, 
                   1/3, 1/5, 1/5, 1), 
                 nrow=4,ncol=4))
colnames(cost.local) = criteria[6:9]
rownames(cost.local) = criteria[6:9]

cost.ngo <- t(matrix(c(1,    	 1/3,	 1/5,	3,    
                       3,    	1,    	 1/3,	7,    
                       5,    	3,    	1,    	9,    
                       1/3,	 1/7,	 1/9,	1 ), 
                       nrow=4,ncol=4))
colnames(cost.ngo) = criteria[6:9]
rownames(cost.ngo) = criteria[6:9]

cost.aca <- t(matrix(c(1,    	1,    	 1/7,	3,    
                       1,    	1,    	 1/5,	3,    
                       7,    	5,    	1,    	9,    
                       1/3,	 1/3,	 1/9,	1), 
                       nrow=4,ncol=4))
colnames(cost.aca) = criteria[6:9]
rownames(cost.aca) = criteria[6:9]
# cost2 <- t(t(cost)/colSums(cost))
# total_cost <- rowSums(cost2)
# ave_cost <- rowMeans(cost2)
# cw <- (cost2%*%ave_cost)/ave_cost
# lambda <- sum(cw)
# CI <- (lambda-4)/(4-1)
# Cr_cost <- CI/0.9

priority_cost.local <- rowSums(cost.local)/sum(rowSums(cost.local))
priority_cost.ngo <- rowSums(cost.ngo)/sum(rowSums(cost.ngo))
priority_cost.aca <- rowSums(cost.aca)/sum(rowSums(cost.aca))
# ----------------------------------------
social.local <- t(matrix(c(1, 3, 3, 3, 7, 
                     1/3, 1, 1, 1/3, 5, 
                     1/3, 1, 1, 1, 5, 
                     1/3, 3, 1, 1, 5, 
                     1/7, 1/5, 1/5, 1/5, 1), nrow=5,ncol=5))
colnames(social.local) = criteria[10:14]
rownames(social.local) = criteria[10:14]

social.ngo <- t(matrix(c(1,    	5,    	7,    	1,    	9,    
                         1/5,	1,    	3,    	 1/5,	5,    
                         1/7,	 1/3,	1,    	 1/7,	3,    
                         1,    	5,    	7,    	1,    	9,   
                         1/9,	 1/5,	 1/3,	 1/9,	1), 
                       nrow=5,ncol=5))
colnames(social.ngo) = criteria[10:14]
rownames(social.ngo) = criteria[10:14]

social.aca <- t(matrix(c(1,    	 1/3,	1,    	 1/3,	7,    
                         3,    	1,    	3,    	1,    	9,    
                         1,    	 1/3,	1,    	 1/3,	7,    
                         3,    	1,    	3,    	1,    	9,    
                         1/7,	 1/9,	 1/7,	 1/9,	1), 
                       nrow=5,ncol=5))
colnames(social.aca) = criteria[10:14]
rownames(social.aca) = criteria[10:14]
# social2 <- t(t(social)/colSums(social))
# total_social <- rowSums(social2)
# ave_social <- rowMeans(social2)
# cw <- (social2%*%ave_social)/ave_social
# lambda <- sum(cw)
# CI <- (lambda-5)/(5-1)
# Cr_social <- CI/1.12

priority_social.local <- rowSums(social.local)/sum(rowSums(social.local))
priority_social.ngo <- rowSums(social.ngo)/sum(rowSums(social.ngo))
priority_social.aca <- rowSums(social.aca)/sum(rowSums(social.aca))
# ----------------------------------------
environmental.local <- t(matrix(c(1, 3, 3, 3, 
                            1/3, 1, 3, 3, 
                            1/3, 1/3, 1, 1, 
                            1/3, 1/3, 1, 1), 
                          nrow=4,ncol=4))
colnames(environmental.local) = criteria[15:18]
rownames(environmental.local) = criteria[15:18]

environmental.ngo <- t(matrix(c(1,    	1,    	5,    	9,    
                                1,    	1,    	5,    	9,    
                                1/5,	 1/5,	1,    	5,    
                                1/9,	 1/9,	 1/5,	1), 
                          nrow=4,ncol=4))
colnames(environmental.ngo) = criteria[15:18]
rownames(environmental.ngo) = criteria[15:18]

environmental.aca <- t(matrix(c(1,    	1,    	 1/9,	 1/3,
                                1,    	1,    	 1/9,	 1/3,
                                9,    	9,    	1,    	6,    
                                3,    	3,    	 1/6,	1    
), 
                          nrow=4,ncol=4))
colnames(environmental.aca) = criteria[15:18]
rownames(environmental.aca) = criteria[15:18]
# environmental2 <- t(t(environmental)/colSums(environmental))
# total_enviro <- rowSums(environmental2)
# ave_enviro <- rowMeans(environmental2)
# cw <- (environmental2%*%ave_enviro)/ave_enviro
# lambda <- sum(cw)
# CI <- (lambda-4)/(4-1)
# Cr_enviro <- CI/0.9

priority_environmental.local <- rowSums(environmental.local)/sum(rowSums(environmental.local))
priority_environmental.ngo <- rowSums(environmental.ngo)/sum(rowSums(environmental.ngo))
priority_environmental.aca <- rowSums(environmental.aca)/sum(rowSums(environmental.aca))

# ----------------------------------------
#alternativesPairwiseComparisonsList <- list(technical = technical, cost = cost, social = social, environmental = environmental)
criteriaWeightsPairwiseComparisons.local <- t(matrix(c(1, 3, 3, 3, 
                                                 1/3, 1, 3, 3, 
                                                 1/3, 1/3, 1, 1, 
                                                 1/3, 1/3, 1, 1), 
                                               nrow=4,ncol=4))
colnames(criteriaWeightsPairwiseComparisons.local) = c("technical", "cost", "social", "environmental")
rownames(criteriaWeightsPairwiseComparisons.local) = c("technical", "cost", "social", "environmental")

criteriaWeightsPairwiseComparisons.ngo <- t(matrix(c(1, 1/6, 1/9, 1,
                                                     6, 1, 1/3, 6,
                                                     9, 3, 1, 9,
                                                     1,6,1/9,1), 
                                                     nrow=4,ncol=4))
colnames(criteriaWeightsPairwiseComparisons.ngo) = c("technical", "cost", "social", "environmental")
rownames(criteriaWeightsPairwiseComparisons.ngo) = c("technical", "cost", "social", "environmental")

criteriaWeightsPairwiseComparisons.aca <- t(matrix(c(1,3,9,3,
                                                     1/3,1,7,3,
                                                     1/9,1/7,1,1/5,
                                                     1/3,1/3,5,1), 
                                                     nrow=4,ncol=4))
colnames(criteriaWeightsPairwiseComparisons.aca) = c("technical", "cost", "social", "environmental")
rownames(criteriaWeightsPairwiseComparisons.aca) = c("technical", "cost", "social", "environmental")

# ----------------------------------------
priority_weights.local <- rowSums(criteriaWeightsPairwiseComparisons.local)/sum(rowSums(criteriaWeightsPairwiseComparisons.local))
priority_weights.ngo <- rowSums(criteriaWeightsPairwiseComparisons.ngo)/sum(rowSums(criteriaWeightsPairwiseComparisons.ngo))
priority_weights.aca <- rowSums(criteriaWeightsPairwiseComparisons.aca)/sum(rowSums(criteriaWeightsPairwiseComparisons.aca))

final_weight.local <- c(priority_technical.local*priority_weights.local[1],
                  priority_cost.local*priority_weights.local[2],
                  priority_social.local*priority_weights.local[3],
                  priority_environmental.local*priority_weights.local[4])
final_weight.ngo <- c(priority_technical.ngo*priority_weights.ngo[1],
                        priority_cost.ngo*priority_weights.ngo[2],
                        priority_social.ngo*priority_weights.ngo[3],
                        priority_environmental.ngo*priority_weights.ngo[4])
final_weight.aca <- c(priority_technical.aca*priority_weights.aca[1],
                        priority_cost.aca*priority_weights.aca[2],
                        priority_social.aca*priority_weights.aca[3],
                        priority_environmental.aca*priority_weights.aca[4])
#overall1 <- AHP(criteriaWeightsPairwiseComparisons,
#                alternativesPairwiseComparisonsList)
score.local <- (colSums(t(performanceMatrix)*final_weight.local)-median(colSums(t(performanceMatrix)*final_weight.local)))/diff(range(colSums(t(performanceMatrix)*final_weight.local)))
score.ngo <- (colSums(t(performanceMatrix)*final_weight.ngo)-median(colSums(t(performanceMatrix)*final_weight.ngo)))/diff(range(colSums(t(performanceMatrix)*final_weight.ngo)))
score.aca <- (colSums(t(performanceMatrix)*final_weight.aca)-median(colSums(t(performanceMatrix)*final_weight.aca)))/diff(range(colSums(t(performanceMatrix)*final_weight.aca)))


AHP_local = data.frame(alternatives, score.local)
AHP_ngo = data.frame(alternatives, score.ngo)
AHP_aca = data.frame(alternatives, score.aca)
