
#create example data
group <- rep(c("obasan", "ossan"), times=c(9, 9))
weight <- rnorm(18, mean = 80)
height <- rnorm(18, mean = 165)

#meage the data
data <- data.frame(group, weight, height)

#install the package "dplyr"
if (!require(dplyr))install.packages('dplyr')
library(dplyr)


#calculate mean of "data1" by "group"
weight_mean <- data %>%
  group_by(group) %>%
  summarise(weight_mean=mean(weight))
#calculate mean of nitrogen content
height_mean <- data %>%
  group_by(group) %>%
  summarise(height_mean=mean(height))
#merge them
result <- merge(weight_mean, height_mean, by="group")

#save the result
write.csv(result, file = "body_info.csv")

#by using other packages, you can do these at once
if (!require(qwraps2))install.packages('qwraps2')
if (!require(sjPlot))install.packages('sjPlot')
if (!require(tidyr))install.packages('tidyr')
library(qwraps2)
library(sjPlot)
library(tidyr)

#make a table with mean and sd
table <- data %>%
  group_by(group) %>%
  summarise("Weight" = mean_sd(weight,
                               denote_sd = "pm",
                               markup = getOption("qwraps2_markup", "markdown")),
            "Height" = mean_sd(height,
                               denote_sd = "pm",
                               markup = getOption("qwraps2_markup", "markdown")))
tab_df(table)
tab_df(table,
       file="bodyinfo_table.doc") #save the table as a document table                                             

