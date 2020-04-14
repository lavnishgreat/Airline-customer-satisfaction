#changing satisfaction result attributes
df$`Likelihood to recommend`[which(df$`Likelihood to recommend`>= 7)]="Satisfied"
df$`Likelihood to recommend`[which(df$`Likelihood to recommend`<= 7)]="Not Satisfied"

##high Satissfation
dataHighSatisfaction<-df[df$`Likelihood to recommend`=="Satisfied",]
View(dataHighSatisfaction)
##low satisfaction airline
dataLowSatisfaction<-df[df$`Likelihood to recommend`=="Not Satisfied",]
View(dataLowSatisfaction)
library(dplyr)
which(is.na(dataLowSatisfaction))
dataGroupedByAirlineName<-group_by(dataLowSatisfaction,`Partner Name`)

dataSummarised<-summarise(dataGroupedByAirlineName ,LowSatisfiedEntries=n())

View(dataSummarised)