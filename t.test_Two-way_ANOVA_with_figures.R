#import data
setwd("~/TakaR/2019zambia_mic_community/")
data <- read.table("16S_copynumber.txt", header = T)

#prepare ggplot2
if (!require(ggplot2))install.packages('ggplot2')
library(ggplot2)

#change column names
#log copy is variable. the others are factors.
colnames(data) <- c("Site", "Landuse", "Log_copy")

#t.test to check the difference between the land-uses (ignoring effects of the sites)
t.test(Log_copy ~ Landuse, data = data, var.equal=T)

#make a figure which shows the differenve between the landuses
#first, change the levels of the land-uses (optional)
data$Landuse <- factor(data$Landuse, levels = c("Natural", "Farm"))

#Then, make a figure
g <- ggplot(data, aes(x=Landuse, y=Log_copy, fill=Landuse)) +
  geom_boxplot(lwd=3) +
  theme(panel.grid = element_blank())
g2 <- g +
  labs(y="log copy number/g・soil", x=NULL) +
  theme(axis.text=element_text(size=37, face = "bold", colour = "black"),
        axis.title=element_text(size=40,face="bold", colour = "black"),
        axis.line = element_line(size = 1.2, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "none") +
  annotate("text", x=1.5, y=8.5, label="16S copy number", vjust=0.5, size=15, fontface="bold")
g3 <- g2 +
  scale_fill_manual(values = c("gray40", "white"))
g3 <- g3 +
  geom_segment(x=1, y=7.5, xend=2, yend=7.5, size=1) +
  annotate("text", x=1.5, y=7.8, label="*", size=15) #to add significance info
#show it
g3

#save the figure as a png
ggsave(filename = "200608zam_16S.png", plot = g3, width = 9, height = 9, dpi = 300)

#Two-way ANOVA
summary(aov(Log_copy ~ Landuse * Site, data = data))

#If there is no significant difference on the interaction, quit here.
#If you get a significant differnce, do lsmeans to compare each group.

#I have no significant difference in the interaction but in the land-uses.
#So make a figure showing the difference on the land-uses in all the sites.
h <- ggplot(data, aes(x=Site, y=Log_copy, fill=Landuse)) +
  geom_boxplot(lwd=3) +
  theme(panel.grid = element_blank())
h2 <- h +
  labs(y="log copy number/g・soil", x="Site") +
  theme(axis.text=element_text(size=37, face = "bold", colour = "black"),
        axis.title=element_text(size=45,face="bold"),
        axis.line = element_line(size = 1.8, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position=c(0.80, 0.80),
        legend.background = element_rect(fill = "white", colour = "black"),
        legend.title = element_text(size = 37, face = "bold"),
        legend.text = element_text(size = 37, face = "bold")) +
  annotate("text", x=2, y=8.5, label="16S copy number", vjust=0, size=12, fontface="bold", col="black")
h3 <- h2 +
  scale_fill_manual(values = c("gray40", "white")) +
  labs(fill="Land-use") #this is for the legend
#show it
h3

#save the figure as a png
ggsave(file="zam_16S_site.png", plot=h3, dpi = 300, width = 9, height = 9)
