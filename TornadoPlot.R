# Plot tornado diagram
g1 <- ggplot(data = df, aes(x = Mid , y = Order)) 

a <- subset(df, df$Alternative == "RWH")
b <- subset(df, df$Alternative == "Pond")
c <- subset(df, df$Alternative == "PSF")
d <- subset(df, df$Alternative == "MAR")
e <- subset(df, df$Alternative == "TW")



#======================================================================================
g2 <- g1 + 
  geom_errorbarh(data = a, aes(xmax = High, xmin = Low), color = a$Color, size = 3, height = 0.0)+
  # scale_colour_manual(name="Criteria", breaks = c("#D55E00", "#E69F00", "#56B4E9", "#009E73"), 
  #                   labels = c("Technical", "Economic", "Social", "Environmental"), guide = guide_legend(override.aes=aes(fill=NA))) +
  # theme(legend.justification=c(1,0), legend.position=c(1,0))+
  geom_line(data = a, aes(y = Order), color = "grey", size = 1.2) +#fill = e$Color, pch=21, size = 3) +
  labs(title ="RWH", x = "", y = "") +
  theme_classic() +
  theme_bw() +
  theme(axis.text =  element_text(color = "black", size = 10)) +
  theme(text = element_text(size = 10, family = "Garamond", color = "black")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_y_continuous(name = "", breaks = a$Order, labels = a$Criteria)+
  #theme(axis.text.y=element_text(colour = a$Color))+
  scale_x_continuous(limits= c(-1.0, 1.0), breaks = c(-0.8, 0.8), labels = c("Low", "High"))+
  theme(plot.title = element_text(margin = margin(b = -12)))+
  guides(colour=FALSE)
#======================================================================================
g3 <- g1 + 
  geom_errorbarh(data = b, aes(xmax = High, xmin = Low), color = b$Color, size = 3, height = 0.0)+
  geom_line(data = b, aes(y = Order), color = "grey", size = 1.2) +#fill = e$Color, pch=21, size = 3) +
  labs(title ="Pond", x = "", y = "") +
  theme_classic() +
  theme_bw() +
  theme(axis.text =  element_text(color = "black", size = 10)) +
  theme(text = element_text(size = 10, family = "Garamond", color = "black")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_y_continuous(name = "", breaks = b$Order, labels = b$Criteria)+
  #theme(axis.text.y=element_text(colour = b$Color))+
  scale_x_continuous(limits= c(-1.0, 1.0), breaks = c(-0.8, 0.8), labels = c("Low", "High"))+
  theme(plot.title = element_text(margin = margin(b = -12)))+
  guides(colour=FALSE)
#======================================================================================
g4 <- g1 + 
  geom_errorbarh(data = c, aes(xmax = High, xmin = Low), color = c$Color, size = 3, height = 0.0)+
  geom_line(data = c, aes(y = Order), color = "grey", size = 1.2) +#fill = e$Color, pch=21, size = 3) +
  labs(title ="PSF", x = "", y = "") +
  theme_classic() +
  theme_bw() +
  theme(axis.text =  element_text(color = "black", size = 10)) +
  theme(text = element_text(size = 10, family = "Garamond", color = "black")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_y_continuous(name = "", breaks = c$Order, labels = c$Criteria)+
 # theme(axis.text.y=element_text(colour = c$Color))+
  scale_x_continuous(limits= c(-1.0, 1.0), breaks = c(-0.8, 0.8), labels = c("Low", "High"))+
  theme(plot.title = element_text(margin = margin(b = -12)))+
  guides(colour=FALSE)
#======================================================================================
g5 <- g1 + 
  geom_errorbarh(data = d, aes(xmax = High, xmin = Low), color = d$Color, size = 3, height = 0.0)+
  geom_line(data = d, aes(y = Order), color = "grey", size = 1.2) +#fill = e$Color, pch=21, size = 3) +
  labs(title ="MAR", x = "", y = "") +
  theme_classic() +
  theme_bw() +
  theme(axis.text =  element_text(color = "black", size = 10)) +
  theme(text = element_text(size = 10, family = "Garamond", color = "black")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_y_continuous(name = "", breaks = d$Order, labels = d$Criteria)+
 # theme(axis.text.y=element_text(colour = d$Color))+
  scale_x_continuous(limits= c(-1.0, 1.0), breaks = c(-0.8, 0.8), labels = c("Low", "High"))+
  theme(plot.title = element_text(margin = margin(b = -12)))+
  guides(colour=FALSE)
#======================================================================================
g6 <- g1 + 
  geom_errorbarh(data = e, aes(xmax = High, xmin = Low), color = e$Color, size = 3, height = 0.0)+
  geom_line(data = e, aes(y = Order), color = "grey", size = 1.2) +#fill = e$Color, pch=21, size = 3) +
  labs(title ="TW", x = "Ranking", y = "") +
  theme_classic() +
  theme_bw() +
  theme(axis.text =  element_text(color = "black", size = 10)) +
  theme(text = element_text(size = 10, family = "Garamond", color = "black")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_y_continuous(name = "", breaks = e$Order, labels = e$Criteria)+
#  theme(axis.text.y=element_text(colour = e$Color))+
  scale_x_continuous(limits= c(-1.0, 1.0), breaks = c(-0.8, 0.8), labels = c("Low", "High"))+
  theme(plot.title = element_text(margin = margin(b = -12)))+
  guides(colour=FALSE)

 
#======================================================================================

grid.arrange(g2, g3, g4, g5, g6,  nrow = 5, ncol = 1)

