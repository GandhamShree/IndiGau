library(ggplot2)
a <- read.table("iHS_density.txt",header=T)
geom_density()
c <- ggplot(a) + 
  geom_density(aes(Draft), color = "red",linewidth = 1.0) +
  geom_density(aes(Milk), color="blue",linewidth = 1.0) +
  geom_density(aes(Dual), color="Black", linewidth = 1.0) +
  theme_classic()+
  labs(x = "Density", y = "IHS", color = "") +  # Set color argument to an empty string
  theme(legend.position = "top") +
  scale_color_manual(values = c("Draft" = "red", "Milk" = "blue", "Dual" = "black"), guide = "legend")
#gg1<-c  + theme(axis.text.x = element_text(face="bold", size=14,color="black"),axis.text.y = element_text(face="bold",size=14,color="black"),panel.border = element_rect(colour = "black", fill=NA, size=1.0))+theme(legend.title = element_text(face = "bold",size=20))
gg1<-c  + theme(axis.text.x = element_text(face="bold", size=14,color="black"),axis.text.y = element_text(face="bold",size=14,color="black"),panel.border = element_rect(colour = "black", fill=NA, size=1.0))
gg2 <- gg1 + theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))  # Adjust the margins as needed
ggsave("IHS_density.pdf", gg2, width = 4.2, height = 4, units = "in", bg = "white")
