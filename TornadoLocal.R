# Clear directory
#rm(list = ls())

# ===========================================================================================================
# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# ===========================================================================================================
# Read in data
source("MCDA_data_read.r")

# ===========================================================================================================
# Reset working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# ===========================================================================================================
# Local
# ===========================================================================================================
MAVT_low2 <- array(NA, dim = c(5, 18, 5))
MAVT_high2 <- array(NA, dim = c(5, 18, 5))
for (count in 1:5) {
  for (count2 in 1:18) {
    performanceMatrix.low <- performanceMatrix
    performanceMatrix.high <- performanceMatrix
    performanceMatrix.low[count,count2] <- 0.0
    performanceMatrix.high[count,count2] <- 100.0
    # MAVT, low
    MAVT_matrix_low <- t(performanceMatrix.low)*weights$local
    MAVT_low <- colSums(MAVT_matrix_low)
    MAVT_low2[,count2, count] <- (MAVT_low-median(MAVT_low))/diff(range(MAVT_low))
    # MAVT, high
    MAVT_matrix_high <- t(performanceMatrix.high)*weights$local
    MAVT_high <- colSums(MAVT_matrix_high)
    MAVT_high2[,count2, count] <- (MAVT_high-median(MAVT_high))/diff(range(MAVT_high))
  }
}
df.low.rwh <- data.frame("Criteria" = criteria.text,  "Low" = MAVT_low2[1,,1], "Alternative" = alternatives[1])
df.low.pond <- data.frame("Criteria" = criteria.text, "Low" = MAVT_low2[2,,2], "Alternative" = alternatives[2])
df.low.psf <- data.frame("Criteria" = criteria.text,  "Low" = MAVT_low2[3,,3], "Alternative" = alternatives[3])
df.low.mar <- data.frame("Criteria" = criteria.text, "Low" = MAVT_low2[4,,4], "Alternative" = alternatives[4])
df.low.tw <- data.frame("Criteria" = criteria.text,  "Low" = MAVT_low2[5,,5], "Alternative" = alternatives[5])

df.high.rwh <- data.frame("Criteria" = criteria.text,  "High" = MAVT_high2[1,,1], "Alternative" = alternatives[1])
df.high.pond <- data.frame("Criteria" = criteria.text, "High" = MAVT_high2[2,,2], "Alternative" = alternatives[2])
df.high.psf <- data.frame("Criteria" = criteria.text,  "High" = MAVT_high2[3,,3], "Alternative" = alternatives[3])
df.high.mar <- data.frame("Criteria" = criteria.text,  "High" = MAVT_high2[4,,4], "Alternative" = alternatives[4])
df.high.tw <- data.frame("Criteria" = criteria.text, "High" = MAVT_high2[5,,5], "Alternative" = alternatives[5])

start <- data.frame(c(1027.8093,  710.0619,  825.1629,  512.1879,  823.7768 ))
start <- data.frame(c(0.395702077, -0.220539552,  0.002688263, -0.604297923,  0.000000000 ))
#==============================================================================================================================
source("TornadoRestructure.r")
#======================================================================================

png(file = "TornadoLocal.png", width = 661, height = 1332)
source("TornadoPlot.r")
dev.off()

# ============================================================================
# ============================================================================

png(file = "TornadoLocalZoom.png", width = 661, height = 700)
source("TornadoPlotZoom.r")
dev.off()



