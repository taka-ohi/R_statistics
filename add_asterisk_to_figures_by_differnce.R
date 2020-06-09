# want to show a significant differences

# import data
setwd("2019zambia_mic_community/")
data <- read.table("16S_copynumber.txt", header = T)

# change column names
colnames(data) <- c("Site", "Landuse", "Log_copy")

# do t-test to check the difference between the land-uses
ttest <- t.test(Log_copy ~ Landuse,
                data = data,
                var.equal=T)
# take the p.value
tp <- ttest$p.value

# "the most notable part" in this script
# create a function which shows the degree of significant difference
# p > 0.1 "", 0.1 ≧ p > 0.05 ".", 0.05 ≧ p > 0.01 "*", 0.01 ≧ p > 0.001 "**", 0.001 > p "***"
sig <- function(a) {
  if (a > 0.1) {
    return("")
  } else {
    if ((a <= 0.1)&&(a > 0.05)) {
      return(".")
    } else {
      if ((a <= 0.05)&&(a > 0.01)) {
        return("＊")
      } else {
        if ((a <= 0.01)&&(a > 0.001)) {
          return("＊＊")
        } else return("＊＊＊")
      }
    }
  }
}

#Then, make a figure
# prepare ggplot2
if (!require(ggplot2))install.packages('ggplot2')
library(ggplot2)

#make a boxplot in academic style
#g:basic plot
g <- ggplot(data, aes(x=Landuse, y=Log_copy, fill=Landuse)) +
  geom_boxplot(lwd=3) +
  theme(panel.grid = element_blank())
#g2:enhance thickness etc. to paste on a paper
g2 <- g +
  labs(y="log copy number/g・soil", x=NULL) +
  theme(axis.text=element_text(size=37, face = "bold", colour = "black"),
        axis.title=element_text(size=40,face="bold", colour = "black"), 
        axis.line = element_line(size = 1.2, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "none") +
  annotate("text", x=1.5, y=8.5,
           label="16S copy number",
           vjust=0.5,
           size=15,
           fontface="bold")
#fill the boxes by the colours you choose
g3 <- g2 +
  scale_fill_manual(values = c("gray40", "white"))
#this is for showing the significant differences, using "sig" made before
g3 <- g3 +
  geom_segment(x=1, y=7.5, xend=2, yend=7.5, size=1) +
  annotate("text", x=1.5, y=7.8, label=print(sig(tp)), size=15)

#save the figure as a png
ggsave(filename = "zam_16S.png", plot = g3, width = 9, height = 9, dpi = 300)
