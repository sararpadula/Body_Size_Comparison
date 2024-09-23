#loading packages
library(ggplot2)
install.packages("patchwork")
library(patchwork)

#reading in data
bodySize <- read.csv("Paired_SMI.csv")

moch <- subset(bodySize, Species == "MOCH")
bcch <- subset(bodySize, Species == "BCCH")


#Density plot of wing length difference colored based on species


bcch_density <- ggplot(bcch, aes(x=Wing_Difference))+
geom_density(alpha=0.5, color = "black", fill = "#0072b2")+
  labs (x="Difference Between Male and Female Wing Chord (mm)", y="Density")+
  theme_classic()+
  theme(plot.title = element_text(hjust=0.5))+
  scale_x_continuous(limits=c(-6,12), breaks=seq(-6,12,2))+
  geom_vline(aes(xintercept =0), color = "black", linewidth = 1)+
  geom_vline(aes(xintercept =4), color = "black", linewidth = 1, linetype="dashed")+
  theme(panel.border = element_rect(color = "black", 
                                    fill = NA, 
                                    linewidth = 0.5))+
  theme(axis.text = element_text(size=14, color = "black"))+
  theme(axis.title = element_text(size = 16))+
  theme(axis.title.x = element_text(margin = margin(t = 10)))+
  theme(axis.title.y = element_text(margin = margin(r = 10)))+
  theme(text = element_text(family = "sans"))

moch_density <-ggplot(moch, aes(x=Wing_Difference))+
  geom_density(alpha=0.5, color = "black", fill = "#cc79a7")+
  labs (x="", y="Density")+
  theme_classic()+
  theme(plot.title = element_text(hjust=0.5))+
  scale_x_continuous(limits=c(-6,12), breaks=seq(-6,12,2))+
  geom_vline(aes(xintercept =0), color = "black", linewidth = 1)+
  geom_vline(aes(xintercept =3), color = "black", linewidth = 1, linetype="dashed")+
  theme(panel.border = element_rect(color = "black", 
                                    fill = NA, 
                                    linewidth = 0.5))+
  theme(axis.text = element_text(size=14, color = "black"))+
  theme(axis.title = element_text(size = 16))+
  theme(axis.title.x = element_text(margin = margin(t = 10)))+
  theme(axis.title.y = element_text(margin = margin(r = 10)))+
  theme(text = element_text(family = "sans"))

combined_plot <- moch_density / bcch_density
combined_plot

