# Name: MCDA data read.r
# Author: Chelsea Peters, PhD Candidate, Vanderbilt University
# Contact: chelsea.n.peters@vanderbilt.edu, chelspeters7@gmail.com
# Last updated: 9/6/17
# Purpose: Import data from XLSX to run all the MCDA scripts.

# ===========================================================================================================
# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# ===========================================================================================================
# Set data
# Matrix including preformace of 5 alternatives on 18 criteria (row = alternative, column = criterion).
performanceMatrix <- read.xlsx("Data Formulas.xlsx", colIndex = c(2:19), header = TRUE, sheetName="Performance Matrix")
# Names of each criteria
colnames(performanceMatrix) = c("Variability in Supply", "Variability in Quality", "Water Quality", 
                                "Maintenance Requirements", "Failure Rate", "Construction Cost", 
                                "Potential for NGO/Governmental Help", "Maintenance Cost", 
                                "Transportation Costs", "Sense of Ownership", "Discrimination", 
                                "Misinformation ", "Persons served", "Job Creation", "Prevelence of Source", 
                                "Distance to Source", "Hazard Impact", "Resilience")
# Names of each alternative
rownames(performanceMatrix) = c("RWH","Pond","PSF","MAR","TW")
performanceMatrix.org <- performanceMatrix
# Vector containing names of alternatives
alternatives <- c("RWH","Pond","PSF","MAR","TW")

# Vector containing names of criteria
criteria <- c("Variability in Supply", "Variability in Quality", "Water Quality", 
             "Maintenance Requirements", "Failure Rate", "Construction Cost", 
             "Potential for NGO/Governmental Help", "Maintenance Cost", 
             "Transportation Costs", "Sense of Ownership", "Discrimination", 
             "Misinformation ", "Persons served", "Job Creation", "Prevelence of Source", 
             "Distance to Source", "Hazard Impact", "Resilience")
criteria.text <- c("Variability in Supply", "Variability in Quality", "Water Quality", 
                   "Maintenance Requirements", "Failure Rate", "Construction Cost", 
                   "NGO/Governmental Help", "Maintenance Cost", 
                   "Transportation Costs", "Sense of Ownership", "Discrimination", 
                   "Misinformation", "Persons served", "Job Creation", "Prevelence of Source", 
                   "Distance to Source", "Hazard Impact", "Resilience")
# Vector indicating the direction of the criteria evaluation (meaning we want to maximize each criteria)
minmaxcriteria <- rep("max", 18) 

# Thresholds vector
IndifferenceThresholds <- c(83.3, 100.0, -10.0, 100.0, 100.0, -0.2, 100.0, 1.3, 25.0, 100.0, 100.0, 75.0, 95.0, 100.0, 75.0, 6.7, 100.0, 100.0)
PreferenceThresholds <- c(58.3, 50.0, -40.0, 75.0, 75.0, 0.0, 75.0, 25.0, 75.0, 50.0, 50.0, 50.0, 80.0, 75.0, 50.0, 20.0, 50.0, 50.0)
VetoThresholds <- c(350.0, 350.0, -240.0, 375.0, 525.0, 0.0, 225.0, 75.0, 375.0, 300.0, 350.0, 500.0, 560.0, 900.0, 250.0, 100.0, 500.0, 500.0)
mode_def <- rep("D", 18)

# Criteria Weights Vector taken from survey results
weights <- read.xlsx("StakeholderWeightsFromSurveys.xlsx", sheetName="Input")
weights$criteria <- as.character(weights$criteria)
weights.org <- weights
