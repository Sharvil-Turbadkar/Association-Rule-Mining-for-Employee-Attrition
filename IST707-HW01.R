###Title:IST 707 Assignment-01
getwd()
setwd("C:/Users/Sharvil T/Desktop/IST 707/IST-707 Association Rule Mining")

employee_attrition<-read.table("employee_attrition.csv",header=TRUE,sep=",",stringsAsFactors = F,na.strings = c("","NA"))
# Using na.strings to treat  blank spaces as NA values

install.packages("rmarkdown")
library(rmarkdown)
install.packages("knitr")
library(knitr)

# Storing in a dummy variable
employee_attrition->df.employee
knitr::kable(employee_attrition)
# Columns with missing values 
colnames(df.employee)[colSums((is.na(df.employee)))>0]
# Columns with NA values
employee_attrition[(is.na(employee_attrition$DistanceFromHome))==T,]

table(employee_attrition$JobLevel)

library(plyr)
library(dplyr)

str(employee_attrition)

# Check for missing values
sum(!complete.cases(df.employee))
# Removing NA values

#Find Index of Missing value 
which(is.na(df.employee$DistanceFromHome))

# Distance From Home
mean(df.employee$DistanceFromHome,na.rm=TRUE)->df.employee$DistanceFromHome[is.na(df.employee$DistanceFromHome)]

# PercentSalaryHike
df.employee$PercentSalaryHike[is.na(df.employee$PercentSalaryHike)]<-0
# Performance Rating
table(df.employee$PerformanceRating)
df.employee[is.na(df.employee$PerformanceRating),]
# Assigning Median  as it 5 times more common than the second highest occuring which is 4
# 3 is the Median
df.employee$PerformanceRating[is.na(df.employee$PerformanceRating)]<-median(df.employee$PerformanceRating,na.rm=T)
# Years Since Last Promotion
df.employee$YearsSinceLastPromotion[is.na(df.employee$YearsSinceLastPromotion)]<-0
# Total Working Years
df.employee$TotalWorkingYears[is.na(df.employee$TotalWorkingYears)]<-0
# RelationshipSatisfaction
table(df.employee$RelationshipSatisfaction)
#Job Level
df.employee$JobLevel[is.na(df.employee$JobLevel)]<-median(df.employee$JobLevel,na.rm=T)

# RelationShip_Satisfaction
df.employee$RelationshipSatisfaction
df.employee$RelationshipSatisfaction[is.na(df.employee$RelationshipSatisfaction)]<-4

# Finding index of row that contains NA value for OverTime
which(is.na(df.employee$OverTime))
table(df.employee$OverTime)
# Replacing NA value with the mode for OverTime
df.employee$OverTime[is.na(df.employee$OverTime)]<-rownames(sort((table(df.employee$OverTime)),decreasing = T))[1]

#Gender
df.employee$Gender[is.na(df.employee$Gender)]<-rownames(sort((table(df.employee$Gender)),decreasing = T))[1]

# Columns with missing values
colnames(df.employee)[colSums(is.na(df.employee))>0]
######################################### All NA Values are now Removed######################
# COnvert Character to factors
sapply(df.employee, is.character)->char_var
lapply(df.employee[,char_var],as.factor)-> df.employee[,char_var]
str(employee_attrition)

# Building a correlation Matrix
library(corrplot)
corr_matrix<-cor(df.employee[complete.cases(df.employee),c("AttritionNum","Age","Education","TotalWorkingYears","JobLevel","NumCompaniesWorked","MonthlyIncome","YearsWithCurrManager","YearsInCurrentRole")],method = "pearson")
corrplot<-corrplot(corr_matrix,type = "lower")

str(df.employee$AttritionNum)

# Building an age dot plot 
agedotplot<-ggplot(df.employee,aes(x=df.employee$Age,y=df.employee$JobLevel))+geom_point(aes(color=as.factor(df.employee$JobLevel),size=df.employee$Age))
agedotplot

table(df.employee$Attrition_num)
# Removing Columns with little to no Variance
nzv<-caret::nearZeroVar(df.employee,saveMetrics = T)
colnames(df.employee)[nzv$nzv]

rmv<-c("EmployeeCount","Over18","StandardHours")
df.employee<-df.employee[,!names(df.employee) %in% rmv]

# Removing ouliers
boxplot(df.employee$DistanceFromHome)
summary(df.employee$DistanceFromHome)
df.employee$DistanceFromHome[df.employee$DistanceFromHome %in% boxplot.stats(df.employee$DistanceFromHome)$out]<-median(df.employee$DistanceFromHome)

boxplot(employee_attrition$DistanceFromHome,main="Distance from home before removing  outlier ")
boxplot(df.employee$YearsWithCurrManager)
df.employee$YearsWithCurrManager[df.employee$YearsWithCurrManager%in% boxplot.stats(df.employee$YearsWithCurrManager)$out]<-median(df.employee$YearsWithCurrManager)

summary(df.employee$MonthlyIncome)
str(df.employee)
#Checking for Duplicate Data
nrow(df.employee)
nrow(df.employee[!duplicated(df.employee),])
# No duplicate data found

# Removing Employee Number as it is a redundant column
rmv<-c("EmployeeNumber")
df.employee<-df.employee[,!names(df.employee) %in% rmv]

str(df.employee)

# Converting attrition from categorical to numerical variable and storing it in a new variable
library(dplyr)
library(plyr)
df.employee$AttritionNum<-ifelse(df.employee$Attrition=="Yes",1,0)

df.employee%>%group_by(Gender)%>%summarise(med=mean(HourlyRate))%>%arrange(desc(med))
# Ageplot to view attrition by age_bucket
library(ggplot2)
incomeplot<-ggplot(df.employee,aes(x=as.factor(df.employee$JobRole),y=df.employee$MonthlyIncome,fill=JobRole))+geom_bar(stat = "identity")
incomeplot<-incomeplot+ggtitle("Income by Job Role")+xlab("Job Role")+ylab("Income")
incomeplot

unique(df.employee$DistanceFromHome)

# Income by age
ageplot<- df.employee%>%group_by(Age)%>%summarise(mean=mean(MonthlyIncome),count=n())

incomeageplot<-ggplot(ageplot,aes(x=ageplot$Age,y=ageplot$mean,fill=ageplot$Age))+geom_bar(stat = "identity")
incomeageplot<-incomeageplot+ggtitle("Income distribution by Age")+xlab("Age")+ylab("Income")
incomeageplot
# Younger people get paid less thus they have higher Attrition

# Histogram depicting Distribution of Income
hist(df.employee$MonthlyIncome,col = "grey",border="black",xlab="Monthly_Income",ylab = "Frequency",main="Distribution of Monthly income")
abline(v=mean(df.employee$MonthlyIncome),col="red",lwd=3,lty=2)
abline(v=median(df.employee$MonthlyIncome),col="blue",lwd=3,lty=2)
legend(x = "topright", # location of legend within plot area
      c( "Mean", "Median"),
      col = c( "red","blue"),
      lwd = c(2, 2),lty=c(2,2))
# This shows that monthly income is right skewed which means more employees have lesser salaries 

travel_plot<-ggplot(df.employee,aes(x=(df.employee$BusinessTravel)))+geom_histogram(fill="black",bins=25,stat = "count")
travel_plot<-travel_plot+ggtitle("Type of Travel ")+xlab("Frequency")+ylab("Count")
travel_plot
# Employees prefer to travel Rarely on their jobs  


# Histogram depicting Distribution of total number of Working years
hist(df.employee$TotalWorkingYears,col="grey",border="black",xlab="Total Working Years",ylab="Frequency",main="Distribution of Total Working Years")
abline(v=mean(df.employee$TotalWorkingYears),col="red",lty=2,lwd=3)
abline(v=median(df.employee$TotalWorkingYears),col="blue",lty=2,lwd=3)
legend(x="topright",
       col=c("red","blue"),legend = c("Mean","Median"),lwd=c(2,2),lty=c(3,3))
# This shows that employees leave jobs more often than staying with one job


# Histogram depicting Distribution of distance from home
hist(df.employee$DistanceFromHome,col="grey",border="black",xlab="Distance from Home",ylab="Frequency",main="Distribution of Distance from Home")
abline(v=mean(df.employee$DistanceFromHome),col="red",lty=2,lwd=3)
abline(v=median(df.employee$DistanceFromHome),col="blue",lty=2,lwd=3)
legend(x="topright",
       col=c("red","blue"),legend = c("Mean","Median"),lwd=c(2,2),lty=c(3,3))
# This shows that mostly  employees travel lesser distances to get to work


# Distribution of employees on the basis of years spent with the manager
hist(df.employee$YearsWithCurrManager,col="grey",xlab="Years with Manager",ylab="Frequency",main = "Distribution of employees on the basis of years spent with the manager")
abline(v=mean(df.employee$YearsWithCurrManager),col="red",lty=3,lwd=3)
abline(v=median(df.employee$YearsWithCurrManager),col="blue",lty=3,lwd=3)
legend("topright",col=c("red","blue"),legend=c("Mean","Median"),lty=c(3,3),lwd=c(3,3))
# Employees tend to be less with a manager concluding that they tend to leave the company more often

library(dplyr)
library(ggplot2)
marital_df<-df.employee%>%group_by(df.employee$MaritalStatus,df.employee$Attrition)%>%summarise(count=n())
marital_plot<-ggplot(marital_df,aes(x=marital_df$`df.employee$MaritalStatus`,y=marital_df$`df.employee$Attrition`,fill=marital_df$count))+geom_bar(stat = "identity")
marital_plot<-marital_plot+ggtitle("Attrition by Marital Status")+xlab("Attrition")+ylab("Marital Status")
marital_plot
# Thus we can conclude that Married people are more likely to leave 

# Density Plot of work Life balance preference
hist(df.employee$WorkLifeBalance,prob=T,col="grey",xlab="Work Life Balance",ylab="Frequency",main="Distribution of Work Life Balance")
lines(density(df.employee$WorkLifeBalance),col="black",lwd=3)
# This shows that employees prefer an evenly good work life balance but will not exquisitely 

str(df.employee)
# Converting int to numeric 
sapply(df.employee,is.integer)->int_var
df.employee[,int_var]<-lapply(df.employee[,int_var], as.numeric)

# Discretizing variables
library(arules)

df.employee$Age<-arules::discretize(df.employee$Age,breaks = 3,labels = c("Young","Adult","Old"),method = "frequency")
df.employee$Age<-as.factor(df.employee$Age)
View( head(df.employee))


df.employee$DistanceFromHome<-arules::discretize(df.employee$DistanceFromHome,breaks = 3,labels = c("Near","Normal Distance","Distant"),method = "frequency")
df.employee$DistanceFromHome<-as.factor(df.employee$DistanceFromHome)


df.employee$MonthlyIncome<-arules::discretize(df.employee$MonthlyIncome,breaks = 3,labels = c("Low","Medium","High"),method = "frequency")
df.employee$MonthlyIncome<-as.factor(df.employee$MonthlyIncome)

df.employee$MonthlyRate<-arules::discretize(df.employee$MonthlyRate,breaks = 3,labels = c("Low","Medium","High"),method = "frequency")
df.employee$MonthlyRate<-as.factor(df.employee$MonthlyRate)

df.employee$HourlyRate<-arules::discretize(df.employee$HourlyRate,breaks = 3,labels = c("Low","Medium","High"),method = "frequency")
df.employee$HourlyRate<-as.factor(df.employee$HourlyRate)

df.employee$DailyRate<-arules::discretize(df.employee$DailyRate,breaks = 3,labels = c("Low","Medium","High"),method = "frequency")
df.employee$DailyRate<-as.factor(df.employee$DailyRate)

str(df.employee)
df.employee$PercentSalaryHike<-arules::discretize(df.employee$PercentSalaryHike,breaks = 3,labels = c("Low","Medium","High"),method = "frequency")
df.employee$PercentSalaryHike<-as.factor(df.employee$PercentSalaryHike)

df.employee$YearsAtCompany<-arules::discretize(df.employee$YearsAtCompany,breaks=2,label=c("Low","High"),method = "frequency")
df.employee$YearsAtCompany<-as.factor(df.employee$YearsAtCompany)

df.employee$TotalWorkingYears<-arules::discretize(df.employee$TotalWorkingYears,breaks=2,label=c("Low","High"),method = "frequency")
df.employee$TotalWorkingYears<-as.factor(df.employee$TotalWorkingYears)
str(df.employee)

df.employee$YearsInCurrentRole<-arules::discretize(df.employee$YearsInCurrentRole,breaks=2,label=c("Low","High"),method = "frequency")
df.employee$YearsInCurrentRole<-as.factor(df.employee$YearsInCurrentRole)

df.employee$YearsSinceLastPromotion<-arules::discretize(df.employee$YearsSinceLastPromotion,breaks=2,label=c("Low","High"),method = "frequency")
df.employee$YearsSinceLastPromotion<-as.factor(df.employee$YearsSinceLastPromotion)

df.employee$YearsWithCurrManager<-arules::discretize(df.employee$YearsWithCurrManager,breaks=2,label=c("Low","High"),method = "frequency")
df.employee$YearsWithCurrManager<-as.factor(df.employee$YearsWithCurrManager)

df.employee$NumCompaniesWorked<-arules::discretize(df.employee$NumCompaniesWorked,breaks=2,label=c("Low","High"),method = "frequency")
df.employee$NumCompaniesWorked<-as.factor(df.employee$NumCompaniesWorked)

str(df.employee)
df.employee$TrainingTimesLastYear<-arules::discretize(df.employee$TrainingTimesLastYear,breaks=2,label=c("Low","High"),method = "frequency")
df.employee$TrainingTimesLastYear<-as.factor(df.employee$TrainingTimesLastYear)

df.employee$WorkLifeBalance<-arules::discretize(df.employee$WorkLifeBalance,breaks=2,label=c("Low","High"),method = "frequency")
df.employee$WorkLifeBalance<-as.factor(df.employee$WorkLifeBalance)

df.employee$EnvironmentSatisfaction<-arules::discretize(df.employee$EnvironmentSatisfaction,breaks=2,label=c("Low","High"),method = "frequency")
df.employee$EnvironmentSatisfaction<-as.factor(df.employee$EnvironmentSatisfaction)

df.employee$JobSatisfaction<-arules::discretize(df.employee$JobSatisfaction,breaks=2,label=c("Low","High"),method = "frequency")
df.employee$JobSatisfaction<-as.factor(df.employee$JobSatisfaction)

table(df.employee$PerformanceRating)

df.employee$StockOptionLevel<-arules::discretize(df.employee$StockOptionLevel,breaks=2,label=c("Low","High"),method = "frequency")
df.employee$StockOptionLevel<-as.factor(df.employee$StockOptionLevel)

df.employee$PerformanceRating<-arules::discretize(df.employee$PerformanceRating,breaks=2,label=c("Low","High"),method = "interval")
df.employee$PerformanceRating<-as.factor(df.employee$PerformanceRating)
table(df.employee$PerformanceRating)

df.employee$RelationshipSatisfaction<-arules::discretize(df.employee$RelationshipSatisfaction,breaks=2,label=c("Low","High"),method = "frequency")
df.employee$RelationshipSatisfaction<-as.factor(df.employee$RelationshipSatisfaction)

df.employee$Education<-arules::discretize(df.employee$Education,breaks=4,label=c("Uneducated","Basic Education","Highly Educated","Scholar"),method = "frequency")
df.employee$Education<-as.factor(df.employee$Education)

df.employee$JobLevel<-arules::discretize(df.employee$JobLevel,breaks=5,label=c("Junior Level","Upper Juinor Level","General Employee","Senior Level" ,"Managerial Level"),method = "interval")
df.employee$JobLevel<-as.factor(df.employee$JobLevel)

str(df.employee)

# Applying apriori 
rules<-apriori(df.employee[,sapply(df.employee, is.factor)])
head(rules)
rules
inspect(head(rules))

# Finding columns that are Factors
fac_var<-sapply(df.employee,is.factor)

# Converting Factor to Transaction
trans<-as(df.employee[,fac_var],"transactions")

# Rules with the most Lift
inspect(rules(sort(trans,by="lift",decreasing = T),10))        

disc_df.employee<-sapply(df.employee,is.factor)
rm(disc_df.employee)

library(arules)
# Making Rules where there is Attrition
Yes_attrition<-apriori(data=trans,parameter = list(support=0.01,conf=0.3),appearance = list(default="lhs",rhs=c("Attrition=Yes")))
Yes_attrition<-head(sort(Yes_attrition,decreasing = T,by="lift"),10)
inspect(head(sort(Yes_attrition,decreasing = T,by="lift"),10))

No_attrition<-apriori(data=trans,parameter = list(support=0.01,conf=0.3),appearance = list(default="lhs",rhs=c("Attrition=No")))
No_attrition<-head(sort(No_attrition,decreasing = T,by="lift"),10)

Yes_attrition
inspect(head(sort(No_attrition,decreasing = T,by="lift"),10))

install.packages("arulesViz")
library(arulesViz)
plot(Yes_attrition,method="graph")
plot(No_attrition,method="graph")

str(df.employee)
# ItemFrequency 
# Items with the most frequency
itm<-eclat(trans,parameter = list(support=0.5,minlen=2))
itemFrequencyPlot(trans,topN=15,type="absolute",main="Frequency of most occuring items")
itemFrequencyPlot(Yes_attrition,topN=10,type="absolute",main="Frequency ")
itm

inspect(head(sort(itm, decreasing = T),10))

# Shiny Apps
install.packages('rsconnect')
library(rsconnect)

rsconnect::setAccountInfo(name='sharvilt',
                          token='D7704AEBC2F1895FB39575766FD1837F',
                          secret='MvQJ7+FBWu74R/HbdrjGOrrDWNDo189qg39l30iR')


# Running the shiny App
library(shiny)
setwd("C:/Users/Sharvil T/Desktop/IST 707")
runApp()

