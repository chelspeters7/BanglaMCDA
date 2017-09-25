# Name: MCDA_Bangladesh.r
# Author: Chelsea Peters, PhD Candidate, Vanderbilt University
# Contact: chelsea.n.peters@vanderbilt.edu, chelspeters7@gmail.com
# Last updated: 6/9/17
# Purpose: Run each MCDA method (MAVT, AHP, ELECTRE I, ELECTRE III) and general plotting of ranking
# of different drinking water sources in coastal Bangladesh. 

# ===========================================================================================================
# Clear directory
rm(list = ls())

# ===========================================================================================================
# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# ===========================================================================================================
# Install and load all packages
install.packages("easypackages")
library(easypackages)
my_packages <- c("RColorBrewer","gridExtra", "xlsx","easypackages","tidyr", "reshape2", "ggplot2", "extrafont", 
"OutrankingTools", "gtools", "scales", "dplyr", "plry")
packages(my_packages)
libraries(my_packages)

# ===========================================================================================================
# Read in data
source("MCDA_data_read.r")

# ===========================================================================================================
# Run MCDA
source("MAVT.r")
source("AHP.r")
source("electre1.r")
source("electre3.r")

# ===========================================================================================================
# Load fonts
font_import()
loadfonts(device="win")       #Register fonts for Windows bitmap output
fonts() 

# ===========================================================================================================
# Set color palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# ===========================================================================================================
# Combine final rankings into one large data frame for plotting
MAVTdata <- data.frame(alternum = c(1:5), Local = local_norm, NGO = ngo_norm, Academic = aca_norm, Method = 
                         c("MAVT", "MAVT", "MAVT", "MAVT", "MAVT"))
electre1data <- data.frame(alternum = c(1:5), Local = local_electre1_value, NGO = ngo_electre1_value, 
                           Academic = aca_electre1_value, Method = 
                             c("ELECTRE I", "ELECTRE I", "ELECTRE I", "ELECTRE I", "ELECTRE I"))
electre3data <- data.frame(alternum = c(1:5), Local = local_electre3_value, NGO = ngo_electre3_value, 
                           Academic = aca_electre3_value, Method = 
                             c("ELECTRE III", "ELECTRE III", "ELECTRE III", "ELECTRE III", "ELECTRE III"))
AHPdata <- data.frame(alternum = c(1:5), Local = AHP_local$score.local, NGO = AHP_ngo$score.ngo, 
                      Academic = AHP_aca$score.aca,  Method = 
                        c("AHP", "AHP", "AHP", "AHP", "AHP"))
rownames(AHPdata) <- c("RWH", "Pond", "PSF", "MAR", "TW")

rank <- melt(rbind(MAVTdata, AHPdata, electre3data, electre1data), id.vars = c("alternum", "Method"), 
             measure.vars = c( "Local", "NGO", "Academic"))
colnames(rank)<- c("Alternative", "Method", "Stakeholder", "Value")

# ===========================================================================================================
# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# ===========================================================================================================
# Plot final ranking for each method
neworder <- c("MAVT", "AHP", "ELECTRE I", "ELECTRE III")
rank <- arrange(transform(rank,
                           Method=factor(Method,levels=neworder)),Method)


