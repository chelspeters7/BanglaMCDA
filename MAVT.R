#MAVT, Chelsea Peters, 6/9/2017

# Complete the MAVT weighting
MAVT_matrix_local <- t(performanceMatrix)*weights$local
MAVT_matrix_ngo <- t(performanceMatrix)*weights$ngo
MAVT_matrix_aca <- t(performanceMatrix)*weights$aca
# Sum the alternative scores
MAVT_local <- colSums(MAVT_matrix_local)
MAVT_ngo <- colSums(MAVT_matrix_ngo)
MAVT_aca <- colSums(MAVT_matrix_aca)
# Normalize the results
local_norm <- (MAVT_local-median(MAVT_local))/diff(range(MAVT_local))
ngo_norm <- (MAVT_ngo-median(MAVT_ngo))/diff(range(MAVT_ngo))
aca_norm <- (MAVT_aca-median(MAVT_aca))/diff(range(MAVT_aca))

