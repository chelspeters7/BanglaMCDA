# Restructure Tornado Data Frame for Plotting Purposes

df.rwh <- merge(df.low.rwh, df.high.rwh, by = c("Criteria", "Alternative"))
df.rwh <- df.rwh %>%
  mutate("Mid" = rep(start[1,1], 18), 
         "Range" = df.rwh$High-df.rwh$Low)
df.rwh2 <- df.rwh[order(abs(df.rwh$Range)),] 
df.rwh2 <- df.rwh2 %>%
  mutate("Order" = 1:18)

df.pond <- merge(df.low.pond, df.high.pond, by = c("Criteria", "Alternative"))
df.pond <- df.pond %>%
  mutate("Mid" = rep(start[2,1], 18), 
         "Range" = df.pond$High-df.pond$Low)
df.pond2 <- df.pond[order(abs(df.pond$Range)),] 
df.pond2 <- df.pond2 %>%
  mutate("Order" = 1:18)

df.psf <- merge(df.low.psf, df.high.psf, by = c("Criteria", "Alternative"))
df.psf <- df.psf %>%
  mutate("Mid" = rep(start[3,1], 18), 
         "Range" = df.psf$High-df.psf$Low)
df.psf2 <- df.psf[order(abs(df.psf$Range)),] 
df.psf2 <- df.psf2 %>%
  mutate("Order" = 1:18)

df.mar <- merge(df.low.mar, df.high.mar, by = c("Criteria", "Alternative"))
df.mar <- df.mar %>%
  mutate("Mid" = rep(start[4,1], 18), 
         "Range" = df.mar$High-df.mar$Low)
df.mar2 <- df.mar[order(abs(df.mar$Range)),] 
df.mar2 <- df.mar2 %>%
  mutate("Order" = 1:18)

df.tw <- merge(df.low.tw, df.high.tw, by = c("Criteria", "Alternative"))
df.tw <- df.tw %>%
  mutate("Mid" = rep(start[5,1], 18), 
         "Range" = df.tw$High-df.tw$Low)
df.tw2 <- df.tw[order(abs(df.tw$Range)),] 
df.tw2 <- df.tw2 %>%
  mutate("Order" = 1:18)

df2 <- rbind(df.rwh2, df.pond2, df.psf2, df.mar2, df.tw2)
# cbPalette2 <- c("#999999", "#E69F00", "#56B4E9", "#009E73", 
#                 "#F0E442", "#0072B2", "#D55E00", "#CC79A7", 
#                 "#999999", "#E69F00", "#56B4E9", "#009E73",
#                 "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
#                 "#999999", "#E69F00")
df.color <- data.frame("Color" = c("#D55E00", "#E69F00", "#56B4E9", "#009E73"), 
                       "Category" = c("Technical", "Economic", "Social", "Environmental"))
df.color$Color <- as.character(df.color$Color)
df.category <- data.frame("Category" = c(rep("Technical", 5),
                                           rep("Economic", 4), 
                                           rep("Social", 5),
                                           rep("Environmental",4)), "Criteria" = criteria.text)
df3 <- merge(df.category, df.color, by = "Category")
df <- merge(df2, df3, by = "Criteria")
