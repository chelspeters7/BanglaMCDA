# ===========================================================================================================
# Clear directory
rm(list = ls())

# ===========================================================================================================
# Standardized randomize 
set.seed(1)

# ===========================================================================================================
# Install and load all packages
# install.packages("easypackages")
# library(easypackages)
# my_packages <- c("dplyr", "xlsx","easypackages","tidyr", "reshape2", "ggplot2", "extrafont", 
#                  "OutrankingTools", "gtools", "scales")
# packages(my_packages)
# libraries(my_packages)

# ===========================================================================================================
# Read in data
source("MCDA_data_read.r")

# ===========================================================================================================
# Number of monte carlo simulations
n <- 1000

source("InitializeMonteCarlo.r")

#===========================================================================================
# For every simulation run the model
for (kjl in 0:n+1){
  # Complete the MCDA Method
  performanceMatrix <- my_array[,,kjl]
  
  weights$local <- my_weights_local[,kjl]
  weights$ngo <- my_weights_ngo[,kjl]
  weights$aca <- my_weights_aca[,kjl]
  source("AHP.r")
  rank.local[,kjl] <- AHP_local$score.local
  rank.ngo[,kjl] <- AHP_ngo$score.ngo
  rank.aca[,kjl] <- AHP_aca$score.aca
}

#===========================================================================================
# Save as dataframe
df.local <- data.frame("id" = 0:n+1, t(rank.local), "Stakeholder" = rep("Local", n+1))
df.ngo<- data.frame("id" = 0:n+1, t(rank.ngo), "Stakeholder" = rep("NGO", n+1))
df.aca <- data.frame("id" = 0:n+1, t(rank.aca), "Stakeholder" = rep("Academic", n+1))
df <- rbind(df.local, df.ngo, df.aca)
colnames(df) <- c("id", alternatives, "Stakeholder")
df2 <- melt(df, id.vars = c("id", "Stakeholder"))


# ===========================================================================================================
# Plotting of Results
png(file = "MonteCarloRankingAHP.png", width = 1152, height = 640)
source("MonteCarloRankingPlot.r")
dev.off()

# ===========================================================================================================
source("SplitBehavior.r")

# ===========================================================================================================
scorevalue <- 0.05 #0.01
weightvalue <- 0.05

#LOCAL
# ===========================================================================================================
# Weighting distributions
png(file = "AHP_Local_Weights.png", width = 1152, height = 640)
beh.x <- beh.local.wei
nbeh.x <- nbeh.local.wei
source("DistributionPlot.r")
mtext("AHP, Local Perception, Top Two Ranks, Weights", outer = TRUE, cex = 1.5)
dev.off()
# ===========================================================================================================
# Testing weighting distributions
lsig.wei.3 <- criteria[is.na(p) == FALSE]

which.sig.local.w <- which.sig.w
local.w <- beh.x
local.n.w <- nbeh.x
# ===========================================================================================================
# Scoring distributions
png(file = "AHP_Local_Scoring.png", width = 1152, height = 640)
beh.x <- beh.local.sco
nbeh.x <-nbeh.local.sco
source("DistributionPlotScores.r")
mtext("AHP, Local Perception, Top Two Ranks, Scoring", outer = TRUE, cex = 1.5)
dev.off()
# ===========================================================================================================
# Testing scoring distributions
which.sig.local <- which.sig
local <- beh.x
local.n <- nbeh.x
# ===========================================================================================================
#NGO
# ===========================================================================================================
# Weighting distributions NGO
png(file = "AHP_NGO_Weights.png", width = 1152, height = 640)
beh.x <- beh.ngo.wei
nbeh.x <- nbeh.ngo.wei
source("DistributionPlot.r")
mtext("AHP, NGO Perception, Top Two Ranks, Weights", outer = TRUE, cex = 1.5)
dev.off()
# ===========================================================================================================
# Testing weighting distributions NGO
osig.wei.3 <- criteria[is.na(p) == FALSE]

which.sig.ngo.w <- which.sig.w
ngo.w <- beh.x
ngo.n.w <- nbeh.x
# ===========================================================================================================
# Scoring distributions NGO
png(file = "AHP_NGO_Scoring.png", width = 1152, height = 640)
beh.x <- beh.ngo.sco
nbeh.x <-nbeh.ngo.sco
source("DistributionPlotScores.r")
mtext("AHP, NGO Perception, Top Two Ranks, Scoring", outer = TRUE, cex = 1.5)
dev.off()
# ===========================================================================================================
# Testing scoring distributions NGO
which.sig.ngo <- which.sig
ngo <- beh.x
ngo.n <- nbeh.x

# ===========================================================================================================
#ACADEMIC
# ===========================================================================================================
# Weighting distributions ACA
png(file = "AHP_ACA_Weights.png", width = 1152, height = 640)
beh.x <- beh.aca.wei
nbeh.x <- nbeh.aca.wei
source("DistributionPlot.r")
mtext("AHP, Academic Perception, Top Two Ranks, Weights", outer = TRUE, cex = 1.5)
dev.off()
# ===========================================================================================================
# Testing weighting distributions
asig.wei.3 <- criteria[is.na(p) == FALSE]

which.sig.aca.w <- which.sig.w
aca.w <- beh.x
aca.n.w <- nbeh.x
# ===========================================================================================================
# Scoring distributions
png(file = "AHP_ACA_Scoring.png", width = 1152, height = 640)
beh.x <- beh.aca.sco
nbeh.x <-nbeh.aca.sco
source("DistributionPlotScores.r")
mtext("AHP, Academic Perception, Top Two Ranks, Scoring", outer = TRUE, cex = 1.5)
dev.off()
# ===========================================================================================================
# Testing scoring distributions
which.sig.aca <- which.sig
aca <- beh.x
aca.n <- nbeh.x
# ===========================================================================================================
#signficant <- performanceMatrixa3*performanceMatrixl3*performanceMatrixo3

data <-data.frame(NULL)

par(mfrow=c(1,1),  oma = c(2, 2, 2, 2), mar=c(1.7,1.7,1.7,1.7))
for (abc in 1:3) {#(abc in 1:nrow(which.sig.local)){
  x <- local[which.sig.local[abc,1],which.sig.local[abc,2],]#local[a, b,]
  nx <- local.n[which.sig.local[abc,1],which.sig.local[abc,2],]#local.n[a, b,]
  Local.df <- data.frame(Value = x, Stakeholder = array("Local",dim = length(x)),
                         Behavior = array("Behavior",dim = length(x)), 
                         Criteria = criteria.text[which.sig.local[abc,2]], 
                         Alternative = alternatives[which.sig.local[abc,1]])
  #a = array(a, dim = length(x)), 
  #b = array(b, dim = length(x)))
  nLocal.df <- data.frame(Value = nx, Stakeholder = array("Local",dim = length(nx)),
                          Behavior = array("Nonbehavior",dim = length(nx)), 
                          Criteria = criteria.text[which.sig.local[abc,2]], 
                          Alternative = alternatives[which.sig.local[abc,1]]) 
  
  y <- ngo[which.sig.ngo[abc,1], which.sig.ngo[abc,2], ]#ngo[a, b,]
  ny <- ngo.n[which.sig.ngo[abc,1], which.sig.ngo[abc,2], ]#ngo.n[a, b,]
  NGO.df <- data.frame(Value = y, Stakeholder = array("NGO",dim = length(y)),
                       Behavior = array("Behavior",dim = length(y)), 
                       Criteria = criteria.text[which.sig.ngo[abc,2]], 
                       Alternative = alternatives[which.sig.ngo[abc,1]])
  #a = array(a, dim = length(y)), 
  #b = array(b, dim = length(y)))
  nNGO.df <- data.frame(Value = ny, Stakeholder = array("NGO",dim = length(ny)),
                        Behavior = array("Nonbehavior",dim = length(ny)), 
                        Criteria = criteria.text[which.sig.ngo[abc,2]], 
                        Alternative = alternatives[which.sig.ngo[abc,1]])
  
  z <- aca[which.sig.aca[abc,1], which.sig.aca[abc,2], ]#aca[a, b,]
  nz <- aca.n[which.sig.aca[abc,1], which.sig.aca[abc,2], ]#aca.n[a, b,]
  ACA.df <- data.frame(Value = z, Stakeholder = array("Academic",dim = length(z)),
                       Behavior = array("Behavior",dim = length(z)), 
                       Criteria = criteria.text[which.sig.aca[abc,2]], 
                       Alternative = alternatives[which.sig.aca[abc,1]])
  
  nACA.df <- data.frame(Value = nz, Stakeholder = array("Academic",dim = length(nz)),
                        Behavior = array("Nonbehavior",dim = length(nz)), 
                        Criteria = criteria.text[which.sig.aca[abc,2]], 
                        Alternative = alternatives[which.sig.aca[abc,1]]) 
  
  data <- rbind(data, Local.df, nLocal.df, NGO.df, nNGO.df, ACA.df, nACA.df)
  data2 <- data %>% 
    mutate(Label = paste(data$Alternative, data$Criteria, sep=", "))
}

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

png(file = "SignificantParameterDistributionsAHP.png", width = 1152, height = 640)
plot <- ggplot(data2, aes(x = Behavior, y = Value, color = Stakeholder)) + 
  facet_wrap(~Label, ncol = 4)+
  geom_boxplot()+
  labs(title ="Significant Parameter Distribution in AHP", x = "", y = "Score") +
  theme_classic() +
  theme_bw()+
  theme(axis.text =  element_text(color = "black", size = 16))+
  theme(text = element_text(size = 30, family = "Garamond", color = "black"))+
  scale_colour_manual(name = "Stakeholders", values = c(cbPalette[1], cbPalette[3], cbPalette[7]))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = 'white', colour = 'white')) +
  theme(strip.text = element_text(size = 14),
        strip.background=element_rect(fill="grey", colour = NULL))
print(plot)
dev.off()

# ===========================================================================================================
# ===========================================================================================================
# ===========================================================================================================
# ===========================================================================================================

dataw <-data.frame(NULL)

par(mfrow=c(1,1),  oma = c(2, 2, 2, 2), mar=c(1.7,1.7,1.7,1.7))
for (abcd in 1:3) { 
  www=which.sig.local.w[abcd]
  xw <- local.w[www, ]
  nxw <- local.n.w[www, ]
  Local.df <- data.frame(Value = xw, Stakeholder = array("Local",dim = length(xw)),
                         Behavior = array("Behavior",dim = length(xw)), 
                         Criteria = criteria.text[which.sig.local.w[abcd]])
  nLocal.df <- data.frame(Value = nxw, Stakeholder = array("Local",dim = length(nxw)),
                          Behavior = array("Nonbehavior",dim = length(nxw)), 
                          Criteria = criteria.text[which.sig.local.w[abcd]])
  www=which.sig.ngo.w[abcd]
  yx <- ngo.w[www, ]
  nyx <- ngo.n.w[www, ]
  NGO.df <- data.frame(Value = yx, Stakeholder = array("NGO",dim = length(yx)),
                       Behavior = array("Behavior",dim = length(yx)), 
                       Criteria = criteria.text[which.sig.ngo.w[abcd]])
  nNGO.df <- data.frame(Value = nyx, Stakeholder = array("NGO",dim = length(nyx)),
                        Behavior = array("Nonbehavior",dim = length(nyx)), 
                        Criteria = criteria.text[which.sig.ngo.w[abcd]])
  www=which.sig.aca.w[abcd]
  zw <- aca.w[www,]
  nzw <- aca.n.w[www,]
  ACA.df <- data.frame(Value = zw, Stakeholder = array("Academic",dim = length(zw)),
                       Behavior = array("Behavior",dim = length(zw)), 
                       Criteria = criteria.text[which.sig.aca.w[abcd]])
  nACA.df <- data.frame(Value = nzw, Stakeholder = array("Academic",dim = length(nzw)),
                        Behavior = array("Nonbehavior",dim = length(nzw)), 
                        Criteria = criteria.text[which.sig.aca.w[abcd]])
  dataw <- rbind(dataw, Local.df, nLocal.df, NGO.df, nNGO.df, ACA.df, nACA.df)
}

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

png(file = "SignificantParameterDistributionsAHPWeights.png", width = 1152, height = 640)
plot <- ggplot(dataw, aes(x = Behavior, y = Value, color = Stakeholder)) + 
  facet_wrap(~Criteria, ncol = 4)+
  geom_boxplot()+
  labs(title ="Significant Parameter Distribution in AHP", x = "", y = "Weight") +
  theme_classic() +
  theme_bw()+
  theme(axis.text =  element_text(color = "black", size = 16))+
  theme(text = element_text(size = 30, family = "Garamond", color = "black"))+
  scale_colour_manual(name = "Stakeholders",
                      values = c(cbPalette[1], cbPalette[3], cbPalette[7]))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = 'white', colour = 'white')) +
  theme(strip.text = element_text(size = 14),
        strip.background=element_rect(fill="grey", colour = NULL))
print(plot)
dev.off()