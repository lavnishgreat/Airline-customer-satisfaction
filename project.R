################################################
#IST 687
# Student name: Lavnish Talreja	
# Homework number:Project
# Date due: 09/30/19
#I did this work by myself and with the help of book "An introduction to data science by jeffrey saltz,jeffrey stanton and with the help of professor Yatish Hegde
# I did this work with the help of following internet urls:
#https://www.rdocumentation.org/

#dev.off() # Clear the graph window
dev.off() # Clear the graph window
cat('\014')  # Clear the console
rm(list=ls()) # Clear all user objects from the environment!!!
getwd()
path='C:/Homework/IST 687'
setwd(path)
dir()
library(RCurl)
library(jsonlite)
library(rjson)
library(httr)
library(imputeTS)
df<- jsonlite::fromJSON("project.json")
View(df)

missingcol <- apply(df,2, function(x)all(any(is.na(x)))) 

##Data cleaning

which(is.na(df$Departure.Delay.in.Minutes))
which(is.na(df$Arrival.Delay.in.Minutes))
which(is.na(df$Flight.time.in.minutes))
which(is.na(df$Likelihood.to.recommend))
which(!is.na(df$freeText))
df$Departure.Delay.in.Minutes
df$Departure.Delay.in.Minutes<-na_interpolation(df$Departure.Delay.in.Minutes)
df$Arrival.Delay.in.Minutes<-na_interpolation(df$Arrival.Delay.in.Minutes)
df$Flight.time.in.minutes<-na_interpolation(df$Flight.time.in.minutes)
df$Likelihood.to.recommend<-na_interpolation(df$Likelihood.to.recommend)

#Cleaning of colnames
names(df)<-gsub(".","_",names(df), fixed = T)
which(is.na(df))
str(df)
##do it later
df$Destination_City<-trimws(df$Destination_City,which = "both")
df$Airline_Status<-trimws(df$Airline_Status,which = "both")
df$Gender<-trimws(df$Gender, which = "both")
df$Type_of_Travel<- trimws(df$Type_of_Travel,which = "both")
df$Class<-trimws(df$Class, which = "both")
df$Partner_Code<- trimws(df$Partner_Code, which = "both")
df$Partner_Name<-trimws(df$Partner_Name, which="both")
df$Origin_City<-trimws(df$Origin_City, which="both")
df$Origin_State<-trimws(df$Origin_State, which = "both")

## updating the date format
df$Flight_date=gsub("-", "/", df$Flight_date, fixed=T )
df$Flight_date=as.Date(df$Flight_date, "%m/%d/%Y")
df$Flight_date= format(df$Flight_date, "%m-%d-%y")

## apply text mining to df$freeText
library(tm)
free_text<-df$freeText[10001:10282]
words.vec<-VectorSource(free_text)
words.corpus<-Corpus(words.vec)
words.corpus<-tm_map(words.corpus,
                     content_transformer(tolower))
words.corpus<-tm_map(words.corpus,
                     removePunctuation)
words.corpus<-tm_map(words.corpus,
                     removeNumbers)
words.corpus<-tm_map(words.corpus,removeWords, stopwords("English"))

free_text<- data.frame(text = sapply(words.corpus, as.character), stringsAsFactors = FALSE)

#word cloud for free text
library(wordcloud)
textdis<-VCorpus(VectorSource(free_text))
inspect(textdis)    
head(textdis)


tdm<-TermDocumentMatrix(textdis)
m<-as.matrix(tdm)
wordCounts<-rowSums(m)
wordCounts<-sort(wordCounts,decreasing = TRUE)
cloudFrame<-data.frame(word=names(wordCounts),freq=wordCounts)
wordcloud(cloudFrame$word,cloudFrame$freq,res=300,colors=brewer.pal(8, "Dark2"))


#Rounding off the floating values
df$Departure_Delay_in_Minutes<-round(df$Departure_Delay_in_Minutes)
df$Arrival_Delay_in_Minutes<-round(df$Arrival_Delay_in_Minutes)
df$Flight_time_in_minutes<-round(df$Flight_time_in_minutes)

#changing satisfaction result attributes
df$Likelihood_to_recommend[which(df$Likelihood_to_recommend>= 7)]="Satisfied"
df$Likelihood_to_recommend[which(df$Likelihood_to_recommend<= 7)]="Not Satisfied"

##low satisfaction airline
df1<-df[,1:31]
dataLowSatisfaction<-df1[df$Likelihood_to_recommend=="Not Satisfied",]
View(dataLowSatisfaction)
library(dplyr)
which(is.na(dataLowSatisfaction))
dataGroupedByAirlineName<-group_by(dataLowSatisfaction,Partner_Name)

dataSummarised<-summarise(dataGroupedByAirlineName ,LowSatisfiedEntries=n())

View(dataSummarised)

##Creating wordcloud of low satisfaction airlines
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
library("SnowballC")
library("wordcloud")
library("RColorBrewer")


View(df1)
#text mining
text<-gsub("Airlines Inc.","",dataLowSatisfaction$Partner_Name)
text<-gsub("Air Services","",text)
text<-gsub("Inc.","",text)
text<-gsub("Business","",text)
text<-gsub("Co.","",text)
text<-gsub("Airlines","",text)
text<-gsub("Airways","",text)

textdis<-VCorpus(VectorSource(text))
inspect(textdis)
head(textdis)

#this is wordcloud of airlines..need to create for low satisfaction
tdm<-TermDocumentMatrix(textdis)

m<-as.matrix(tdm)
wordCounts<-rowSums(m)
wordCounts<-sort(wordCounts,decreasing = TRUE)
cloudFrame<-data.frame(word=names(wordCounts),freq=wordCounts)
wordcloud(cloudFrame$word,cloudFrame$freq,res=300,colors=brewer.pal(8, "Dark2"))

##data cleaning is done

which(is.na(df1))

##plotting starts from here

##partner name
library(ggplot2)
plotting_partner_name <- ggplot(dataSummarised,aes(dataSummarised$Partner_Name  ,dataSummarised$LowSatisfiedEntries))
plotting_partner_name <- plotting_partner_name + geom_bar(stat = "identity")
plotting_partner_name <- plotting_partner_name + theme(axis.text.x = element_text(angle = 90, hjust = 1))
plotting_partner_name

##Destination City
plotting_Destination_City <- ggplot(dataLowSatisfaction) + aes(x = dataLowSatisfaction$Destination_City, frequency(100))
plotting_Destination_City <- plotting_Destination_City + geom_bar(stat = "identity")
plotting_Destination_City <- plotting_Destination_City + theme(axis.text.x = element_text(angle = 90, hjust = 1))
plotting_Destination_City

## Origin city 
plotting_origin_City <- ggplot(dataLowSatisfaction) + aes(x = dataLowSatisfaction$Origin_City, frequency(100))
plotting_origin_City <- plotting_origin_City + geom_bar(stat = "identity")
plotting_origin_City <- plotting_origin_City + theme(axis.text.x = element_text(angle = 90, hjust = 1))
plotting_origin_City


## Gender
plotting_gender <- ggplot(dataLowSatisfaction) + aes(x = dataLowSatisfaction$Gender, frequency(100))
plotting_gender <- plotting_gender + geom_bar(stat = "identity")
plotting_gender <- plotting_gender+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
plotting_gender

## class
plotting_class <- ggplot(dataLowSatisfaction) + aes(x = dataLowSatisfaction$Class, frequency(100))
plotting_class <- plotting_class + geom_bar(stat = "identity")
plotting_class <- plotting_class + theme(axis.text.x = element_text(angle = 90, hjust = 1))
plotting_class

subsettingoddata <- df1[,c(4,6,7,8,9,13,15,21,22,23,25,26,27,28,29,30,31)]
View(subsettingoddata)
which(is.na(subsettingoddata))

##Modelling starts now
library(caTools) 
set.seed(5) 
split = sample.split(df1, SplitRatio = 0.75) 

training_set= subset(df1, split == TRUE) 
test_set = subset(df1, split == FALSE)

## linear regression
df1$Destination_City<-as.numeric((df1$Destination_City))

View(training_set)
colnames(training_set)
which(is.na(training_set))
dim(training_set)
m1 <- lm(formula=Likelihood_to_recommend ~ .,data= subsettingoddata)
summary(m1)

model<-model.matrix(m1)

# Significant columns

# Airline_Status
# Age
# Price_Sensitivity
#Type_of_Travel
# No_of_Flights_p_a_
# Eating and drinking at airport

m2 <- lm(formula = Likelihood_to_recommend ~ Type_of_Travel, data = df1)
summary(m2)

# Mileage and Personal

m3<-lm(formula=Likelihood_to_recommend~Age, data=training_set)
summary(m3)
plot(training_set$Likelihood_to_recommend,training_set$Age)
abline(m3)

# Nothing significant

m4<-lm(formula=Likelihood_to_recommend~Price_Sensitivity,training_set)
summary(m4)
plot(dataCleaned$Satisfaction,dataCleaned$Price_Sensitivity)
abline(m4)

# a line is drawn but didn't get much value

m5<-lm(formula = Likelihood_to_recommend~ Gender,data = training_set)
summary(m5)
plot(dataCleaned$Satisfaction,dataCleaned$No_of_Flights_p_a_)
abline(m5)

# Nothing significant

m6<-lm(formula = Satisfaction~Class,dataCleaned)
summary(m6)