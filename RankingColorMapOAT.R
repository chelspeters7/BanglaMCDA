# Use All_Monte_Carlo.R

RWH.low <- df$Low[which(df$Alternative == "RWH")]
PSF.low <- df$Low[which(df$Alternative == "PSF")]
TW.low <- df$Low[which(df$Alternative == "TW")]
Pond.low <- df$Low[which(df$Alternative == "Pond")]
MAR.low <- df$Low[which(df$Alternative == "MAR")]

RWH.high <- df$High[which(df$Alternative == "RWH")]
PSF.high <- df$High[which(df$Alternative == "PSF")]
Pond.high <- df$High[which(df$Alternative == "Pond")]
TW.high <- df$High[which(df$Alternative == "TW")]
MAR.high <- df$High[which(df$Alternative == "MAR")]

bee <- array(NA, dim = c(5, 18))
rownames(bee) <- alternatives
bee[1,] <- RWH.high > min(RWH.low)
bee[1,] <- NA
bee[2,] <- RWH.high > min(Pond.low)
bee[3,] <- RWH.high > min(PSF.low)
bee[4,] <- RWH.high > min(MAR.low)
bee[5,] <- RWH.high > min(TW.low)
numT <- as.numeric(rowSums(bee)) 
b <- numT / c(18,18,18,18,18) *100

fee <- array(NA, dim = c(5, 18))
rownames(fee) <- alternatives
fee[1,] <- Pond.high >  min(RWH.low)
fee[2,] <- Pond.high >  min(Pond.low)
fee[2,] <- NA
fee[3,] <- Pond.high >  min(PSF.low)
fee[4,] <- Pond.high >  min(MAR.low)
fee[5,] <- Pond.high >  min(TW.low)
numT <- as.numeric(rowSums(fee)) 
f <- numT / c(18,18,18,18,18) *100

dee <- array(NA, dim = c(5, 18))
rownames(dee) <- alternatives
dee[1,] <- PSF.high >  min(RWH.low)
dee[2,] <- PSF.high >  min(Pond.low)
#dee[3,] <- PSF.high > PSF.low
dee[4,] <- PSF.high >  min(MAR.low)
dee[5,] <- PSF.high >  min(TW.low)
numT <- as.numeric(rowSums(dee)) 
d <- numT / c(18,18,18,18,18) *100

cee <- array(NA, dim = c(5, 18))
rownames(cee) <- alternatives
cee[1,] <- MAR.high >  min(RWH.low)
cee[2,] <- MAR.high >  min(Pond.low)
cee[3,] <- MAR.high >  min(PSF.low)
#cee[4,] <- MAR.high > MAR.low
cee[5,] <- MAR.high >  min(TW.low)
numT <- as.numeric(rowSums(cee)) 
c <- numT / c(18,18,18,18,18) *100

eee <- array(NA, dim = c(5, 18))
rownames(eee) <- alternatives
eee[1,] <- TW.high >  min(RWH.low)
eee[2,] <- TW.high >  min(Pond.low)
eee[3,] <- TW.high >  min(PSF.low)
eee[4,] <- TW.high >  min(MAR.low)
#eee[5,] <- TW.high > TW.low
numT <- as.numeric(rowSums(eee)) 
e <- numT / c(18,18,18,18,18) *100



