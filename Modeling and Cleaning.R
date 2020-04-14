dev.off() # Clear the graph window
cat('\014')  # Clear the console
rm(list=ls()) # Clear all user objects from the environment!!!
getwd()
path='C:/Homework/IST 687'
setwd(path)

## Data Cleaning
library(imputeTS)
df<- jsonlite::fromJSON("project.json")
View(df)
missingcol <- apply(df,2, function(x)all(any(is.na(x)))) 

which(is.na(df$Departure.Delay.in.Minutes))
which(is.na(df$Arrival.Delay.in.Minutes))
which(is.na(df$Flight.time.in.minutes))
which(is.na(df$Likelihood.to.recommend))
which(!is.na(df$freeText))

names(df)<-gsub(".","_",names(df), fixed = T)
df$Departure_Delay_in_Minutes<-na_interpolation(df$Departure_Delay_in_Minutes)
df$Arrival_Delay_in_Minutes<-na_interpolation(df$Arrival_Delay_in_Minutes)
df$Flight_time_in_minutes<-na_interpolation(df$Flight_time_in_minutes)
df$Likelihood_to_recommend<-na_interpolation(df$Likelihood_to_recommend)




View(df)

str(df)
df$Destination_City<-trimws(df$Destination_City,which = "both")
df$Origin_City<-trimws(df$Origin_City,which = "both")
df$Airline_Status<-trimws(df$Airline_Status,which = "both")
df$Gender<-trimws(df$Gender, which = "both")
df$Type_of_Travel<- trimws(df$Type_of_Travel,which = "both")
df$Class<-trimws(df$Class, which = "both")
df$Partner_Code<- trimws(df$Partner_Code, which = "both")
df$Partner_Name<-trimws(df$Partner_Name, which="both")
df$Origin_State<-trimws(df$Origin_State, which="both")
View(df)
## updating the date format
df$Flight_date=gsub("-", "/", df$Flight_date, fixed=T )
df$Flight_date=as.Date(df$Flight_date, "%m/%d/%Y")
df$Flight_date= format(df$Flight_date, "%m-%d-%y")

df1 <- df[,-32]
View(df1)
which(is.na(df1))
modeling <- lm(formula =df1$Likelihood_to_recommend~ .,data= df1)
summary(modeling)

#Significant columns
#airline status
#age
#class
#price sensitivity
#type of travel
#no of flights
#eating and drinking

#Linear modelling
## Modeling with likelihood to recommend and Price sensitvity
library(ggplot2)
modeling1 <- lm(formula = df1$Likelihood_to_recommend~ df1$Price_Sensitivity, data= df1)
summary(modeling1)
ggplot(df1, aes(x=Price_Sensitivity, y= Likelihood_to_recommend)) +geom_count() + stat_smooth(method = "lm", col = "red") + labs(x = "Price Sensitivity", y = "Satisfaction")

# Modeling with likelihood to recommend and Class
modeling2 <- lm(formula = df1$Likelihood_to_recommend~ df1$Class,data = df1)
summary(modeling2)
ggplot(df1, aes(x= Class, y= Likelihood_to_recommend)) +geom_count() + stat_summary(aes(y= df1$Likelihood_to_recommend, group=1),fun.y=mean, color = "red", geom = "point", group = 1) + labs(x = "Class", y = "Satisfaction")

# Modeling with likelihood to recommend and Airline Status
modeling3 <- lm(formula = df1$Likelihood_to_recommend~ df1$Airline_Status, data = df1)
summary(modeling3)
ggplot(df1, aes(x=Airline_Status, y= Likelihood_to_recommend)) +geom_count() + stat_summary(aes(y=df1$Likelihood_to_recommend, group=1),fun.y=mean, color = "red", geom = "point", group = 1) + labs(x = "Airline Status", y = "Satisfaction")

##  Modeling with likelihood to recommend and Age
Modeling4 <- lm(formula = df1$Likelihood_to_recommend~ df1$Age, data = df1)
summary(Modeling4)
ggplot(df1, aes(x=Age, y= Likelihood_to_recommend)) +geom_count() + stat_summary(aes(y=df1$Likelihood_to_recommend, group=1),fun.y=mean, color = "red", geom = "point", group = 1) + labs(x = "Age", y = "Satisfaction")

## Modeling with Likelihood to recommend and Type of travel
Modeling5 <- lm(formula = df1$Likelihood_to_recommend~ df1$Type_of_Travel, data = df1)
summary(Modeling5)
ggplot(df1, aes(x=df1$Type_of_Travel , y= Likelihood_to_recommend)) +geom_count() + stat_summary(aes(y= df1$Likelihood_to_recommend, group=1),fun.y=mean, color = "red", geom = "point", group = 1) + labs(x = "Type of travel", y = "Satisfaction")


# Modelling with Likelihood to recommend and flights per year
Modeling6=lm(formula = Likelihood_to_recommend ~ df1$Flights_Per_Year, data= df1)
summary(Modeling6)
ggplot( df1, aes(x= df1$Flights_Per_Year, y= Likelihood_to_recommend)) +geom_count() + stat_smooth(method = "lm", col = "red") + labs(x = "Flights per year", y = "Satisfaction")

# Modelling with Likelihood to recommend and eating and drinking at airport
Modeling7=lm(formula = Likelihood_to_recommend ~ df1$Eating_and_Drinking_at_Airport, data= df1)
summary(Modeling7)
ggplot( df1, aes(x= df1$Eating_and_Drinking_at_Airport, y= Likelihood_to_recommend)) +geom_count() + stat_smooth(method = "lm", col = "red") + labs(x = "Eating and drinking at airport", y = "Satisfaction")


## SVM
library(kernlab)
randIndex=sample(1:dim(df1)[1])
randIndex

cutPoint2_3=floor(2 * dim(df1)[1]/3)
cutPoint2_3


trainData=df1[randIndex[1:cutPoint2_3],]
trainData
View(trainData)

testData= df1[randIndex[(cutPoint2_3+1):dim(df1)[1]],]
testData
str(testData)
View(testData)

dim(trainData)
dim(testData)

newtrainData=trainData[,c(4,6,7,8,9,13,15,21,22,23,25,26,27,28,29,30,31)]
View(newtrainData)
newtestData=testData[,c(4,6,7,8,9,13,15,21,22,23,25,26,27,28,29,30,31)]
str(newtestData)
str(newtrainData)
svmop=ksvm(newtrainData$Likelihood_to_recommend ~., data=newtrainData, kernel = "vanilladot",kpar="automatic", C=5,cross=3, prob.model=TRUE)
svmop
 
svmoppredict <- predict(svmop, newtestData, type = "votes")
svmoppredict
View(svmoppredict)
#comtable for 
comTable=data.frame(newtestData[,10], svmoppredict[1,])
comTable[comTable== '0']= "Not Satisfied"
comTable[comTable== '1'] = "Satisfied"
table(comTable)
library(ggplot2)


## Association rule
#airline status
#age
#class
#price sensitivity
#type of travel
#no of flights
#eating and drinking

library(arules)
library(arulesViz)

change=function(v)
{
  q=quantile(v, c(0.4, 0.6))
  vBuckets=replicate(length(v), "Average")
  vBuckets[v <= q[1]]="Low"
  vBuckets[v > q[2]]="High"
  return(vBuckets)
}

Likelihood_to_recommend_1<-change(newtestData$Likelihood_to_recommend)
unique(Likelihood_to_recommend_1)

Age_1<-change(newtestData$Age)
unique(Age_1)

Price_sensitivity_1<-change(newtestData$Price_Sensitivity)
unique(Price_sensitivity_1)

Flights_per_year_1<-change(newtestData$Flights_Per_Year)
unique(Flights_per_year_1)

loyalty_1<- change(newtestData$Loyalty)
unique(loyalty_1)

Eating_1<-change(newtestData$Eating_and_Drinking_at_Airport)
unique(Eating_1)

ProjectSurvey=data.frame(Likelihood_to_recommend_1,  Age_1,Price_sensitivity_1,Flights_per_year_1,loyalty_1,Eating_1)
View(ProjectSurvey1)
ProjectSurvey1=ProjectSurvey[ProjectSurvey$Likelihood_to_recommend_1=="High",]
ruleset=apriori(ProjectSurvey1, parameter = list(support=0.05, confidence =0.05))

inspect(ruleset[1:10])
summary(ruleset)

View(ruleset)

ruleset_filter=sort(ruleset, decreasing=T, by="count")
inspect(ruleset_filter)
plot(ruleset_filter, method="graph")








