# Run all GSA/monte carlo simulations

# ===========================================================================================================
# Clear directory
rm(list = ls())

# ===========================================================================================================
# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

install.packages("easypackages")
library(easypackages)
my_packages <- c("RColorBrewer","gridExtra", "xlsx","easypackages","tidyr", "reshape2", "ggplot2", "extrafont", 
                 "OutrankingTools", "gtools", "scales", "dplyr")
packages(my_packages)
libraries(my_packages)
# ===========================================================================================================
source("TornadoLocal.r")

# ===========================================================================================================
source("RankingColorMapOAT.R")
bc <- data.frame("id" = 1:5, 
                 "Alterntative" = alternatives, 
                 "Method" = rep("MAVT", 5), 
                 "Stakeholder" = rep("Local", 5), 
                 "% Higher than RWH" = b, 
                 "% Higher than Pond" = f, 
                 "% Higher than PSF" = d,
                 "% Higher than MAR" = c,
                 "% Higher than TW" = e) 

# ===========================================================================================================
source("TornadoNGO.r")

# ===========================================================================================================
source("RankingColorMapOAT.R")
bc2 <- data.frame("id" = 1:5, 
                 "Alterntative" = alternatives, 
                 "Method" = rep("MAVT", 5), 
                 "Stakeholder" = rep("NGO", 5), 
                 "% Higher than RWH" = b, 
                 "% Higher than Pond" = f, 
                 "% Higher than PSF" = d,
                 "% Higher than MAR" = c,
                 "% Higher than TW" = e) 
# ===========================================================================================================
source("TornadoAcademic.r")

# ===========================================================================================================
source("RankingColorMapOAT.R")
bc3 <- data.frame("id" = 1:5, 
                  "Alterntative" = alternatives, 
                  "Method" = rep("MAVT", 5), 
                  "Stakeholder" = rep("Academic", 5), 
                  "% Higher than RWH" = b, 
                  "% Higher than Pond" = f, 
                  "% Higher than PSF" = d,
                  "% Higher than MAR" = c,
                  "% Higher than TW" = e) 

# ===========================================================================================================
source("Tornadoelectre1.R")

# ===========================================================================================================
source("Tornadoelectre3.R")
# ===========================================================================================================
source("TornadoAHP.R")
# ===========================================================================================================
final <- rbind(bc, bc2, bc3, 
               bc4, bc5, bc6,
               bc7, bc8, bc9, 
               bc10, bc11, bc12)
final.melt <- melt(final, id = c("id", "Alterntative", "Method", "Stakeholder"))

hm.palette <- colorRampPalette(brewer.pal(9, 'YlOrRd'), space='Lab')  


png(file = "TornadoHeatMap.png", width = 1000, height = 1000)
ggplot(final.melt, aes(x = id, ordered(variable, 
                                       levels = rev(sort(unique(final.melt$variable))))))+
  facet_grid(Method ~ Stakeholder)+
  geom_tile(aes(fill = value))  +
  scale_x_continuous(breaks = 1:5, labels = alternatives)+
  scale_y_discrete( labels = c("TW", "MAR", "PSF", "Pond", "RWH"))+
  labs(x = "Minimum Value", y = "Maximum Value") +
  coord_equal()+
  #scale_fill_gradientn( colours = hm.palette(100))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(axis.text =  element_text(color = "black", size = 16)) +
  theme(legend.position = "right") +
  theme(text = element_text(size = 30, family = "Garamond", color = "black"))+
  geom_text(aes(label = round(value, 1))) +
  scale_fill_gradient2(low = "white", mid = "#009E73", 
                       high = "#56B4E9", midpoint = 50, na.value = "black", 
                       guide = "colourbar")+ #"#E69F00")+#F0E442
  guides(fill = guide_colorbar((title="Percentage"))) 
dev.off() 
  







# ===========================================================================================================
# ===========================================================================================================
# # ===========================================================================================================
 source("MAVT_monte_carlo.R")
 source("ELECTRE1_monte_carlo.R")
 source("ELECTRE3_monte_carlo.R")
 source("AHP_monte_carlo.R")
