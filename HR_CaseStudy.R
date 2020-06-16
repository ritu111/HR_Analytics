############################ HR CASE STUDY ###################
################################################################
#Business Understanding
#Data Understanding
#Data Preparation & EDA
#Model Building 
#Model Evaluation
################################################################

### Business Understanding:

# There is high level of attrition in the company because of which the compamy is affected in various ways.
# Based on the past and current employee information,
# the company has maintained a database containing personal/work information,

## AIM:

# The aim is to understand what factors they should focus on, in order to curb attrition. 
#In other words, they want to know what changes they should make to their workplace, 
#in order to get most of their employees to stay

################################################################

### Data Understanding

library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(gridExtra)
library(corrplot)
library(caTools)
library(MASS)
library(car)
library(caret)
library(cowplot)
library(e1071)

#Loading the 5 files

emp_survey<-read.csv("employee_survey_data.csv", stringsAsFactors = FALSE)
emp_info<-read.csv("general_data.csv", stringsAsFactors = FALSE)
in_time<-read.csv("in_time.csv", stringsAsFactors = FALSE)
out_time<-read.csv("out_time.csv",stringsAsFactors = FALSE)
mang_survey<- read.csv("manager_survey_data.csv",stringsAsFactors = FALSE)

View(emp_survey)
View(emp_info)
View(in_time)
View(out_time)
View(mang_survey)

str(emp_info)  #4410 obs of 24 variables including the target variable
str(in_time)   #4410 obs of 262 variables
str(out_time)    #4410 obs of 262 variables
str(emp_survey)  #4410 obs of 4 variables
str(mang_survey)  #4410 obs of 3 variables
summary(emp_info)

#confirming unique number of employee ID's
length(unique(tolower(emp_info$EmployeeID)))    # 4410, confirming EmployeeID is key 
length(unique(tolower(in_time$X)))              # 4410, confirming EmployeeID is key
length(unique(tolower(out_time$X)))              # 4410
length(unique(tolower(emp_survey$EmployeeID)))   # 4410
length(unique(tolower(mang_survey$EmployeeID)))  # 4410

#Data Cleaning, Preparation and EDA
#In_time and out_time
colnames(in_time)[1] <-"EmployeeID"                            #Renaming column
colnames(out_time)[1]<-"EmployeeID"                            #Renaming column

colnames(in_time)    <-gsub(pattern="X","",colnames(in_time))  #Replacing X with blank in columnnames
colnames(out_time)   <-gsub(pattern="X","",colnames(out_time)) #Replacing X with blank in columnnames

#removing public holidays
in_time<-in_time[,(colSums(is.na(in_time))!=nrow(in_time))]    #Removing columns which have all NA values
out_time<-out_time[,(colSums(is.na(out_time))!=nrow(out_time))]#Removing columns which have all NA values

#cross checcking data across both the files
which(!colnames(in_time)==colnames(out_time))                  #Column names in in_time and out_time are matching

nrow(in_time) == length(unique(in_time$EmployeeID))             #unique employees
nrow(out_time) == length(unique(out_time$EmployeeID))           #TRUE

#converting dates to date format from both the files.
in_time[,2:250]<-lapply(in_time[,2:250], function(x) as_datetime(x))
out_time[,2:250]<-lapply(out_time[,2:250], function(x) as_datetime(x))
Hoursspent<-out_time[,2:250]-in_time[,2:250]            #calculating the hours spent by employee on each day
Hoursspent[,1:249] <- lapply(Hoursspent[,1:249], function(x) as.numeric(x)) #Coverting hours into numeric data type
in_time$Avg_Hoursspent<-rowMeans(Hoursspent, na.rm = TRUE)                  #Average time per employee
#Calculating the no of leaves based on NA
for(i in 1:4410)
{
  in_time$no_leaves[i]<-sum(is.na(Hoursspent[i,]))
}
View(in_time) #we can see the derived metrics avg hours and no of leaves added to the data

which(duplicated(emp_survey))                                  #No duplicates
which(duplicated(emp_info))                                    #No Duplicates
which(duplicated(mang_survey))                                 #No Duplicates

#checking for NA's
sum(is.na(mang_survey))  #0
sum(is.na(emp_survey))  #83
sum(is.na(emp_info))    #28
#we will deal with them after merging the datasets

#merging the data
employee_data1<-merge(emp_info,emp_survey,by="EmployeeID",all=T)         #Merging emp_surey and emp_info
employee_data2<-merge(employee_data1,mang_survey,by="EmployeeID",all=T)  #Merging mang_survey with employee_data1
employee_data3<-merge(employee_data2,in_time[,c(1,251,252)],by="EmployeeID",all=T)  #Merging avg hours,no of leaves
View(employee_data3)

sum(is.na(employee_data3)) #111
111/4410  #0.02  
#We are having around 2% of NA values in our dataset.
#We shall remove them and proceed.
employee_data4 <- na.omit(employee_data3)
sum(is.na(employee_data4))  # NA - 0 

# Barcharts for categorical variables
bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")

plot_grid(ggplot(employee_data4, aes(x=factor(EnvironmentSatisfaction),fill=Attrition))+ geom_bar(), 
          ggplot(employee_data4, aes(x=factor(JobSatisfaction),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee_data4, aes(x=factor(WorkLifeBalance),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee_data4, aes(x=factor(JobInvolvement),fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")   
plot_grid(ggplot(employee_data4, aes(x=factor(PerformanceRating),fill=Attrition))+ geom_bar(), 
          ggplot(employee_data4, aes(x=factor(BusinessTravel),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee_data4, aes(x=factor(Department),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee_data4, aes(x=factor(Education),fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")  
plot_grid(ggplot(employee_data4, aes(x=factor(EducationField),fill=Attrition))+ geom_bar(), 
          ggplot(employee_data4, aes(x=factor(Gender),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee_data4, aes(x=factor(JobRole),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee_data4, aes(x=factor(MaritalStatus),fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")  

#numeric variables
ggplot(employee_data4, aes(Age))+ geom_histogram(binwidth = 10) + facet_wrap(~employee_data4$Attrition)

ggplot(employee_data4, aes(DistanceFromHome))+ geom_histogram(binwidth = 10)

ggplot(employee_data4, aes(MonthlyIncome))+ geom_histogram(binwidth = 10)

ggplot(employee_data4, aes(PercentSalaryHike))+ geom_histogram(binwidth = 1)

ggplot(employee_data4, aes(YearsAtCompany))+ geom_histogram(binwidth = 1)+ facet_wrap(~employee_data4$Attrition)

ggplot(employee_data4, aes(no_leaves))+ geom_histogram(binwidth = 5)

ggplot(employee_data4, aes(NumCompaniesWorked))+ geom_histogram(binwidth = 1)

ggplot(employee_data4, aes(TrainingTimesLastYear))+ geom_histogram(binwidth = 1)

ggplot(employee_data4, aes(YearsSinceLastPromotion))+ geom_histogram(binwidth = 1)+ facet_wrap(~employee_data4$Attrition)

ggplot(employee_data4, aes(YearsWithCurrManager))+ geom_histogram(binwidth = 1)+ facet_wrap(~employee_data4$Attrition)

#determining single valued columns and eliminating them
colnames(employee_data4)[which(sapply(employee_data4,function(x) length(unique(x)) == 1))]
employee_data4 <- employee_data4[, -which(colnames(employee_data4) %in% c('EmployeeCount', 'Over18', 'StandardHours'))]

str(employee_data4)

#outier treatment
boxplot(employee_data4$Age)

boxplot(employee_data4$DistanceFromHome)

boxplot(employee_data4$MonthlyIncome)
quantile(employee_data4$MonthlyIncome,seq(0,1,0.01))
employee_data4[which(employee_data4$MonthlyIncome > 163280.0),'MonthlyIncome'] <- 163280.0

boxplot(employee_data4$NumCompaniesWorked)
quantile(employee_data4$NumCompaniesWorked,seq(0,1,0.01))
employee_data4[which(employee_data4$NumCompaniesWorked > 8),'NumCompaniesWorked'] <- 8

boxplot(employee_data4$PercentSalaryHike)

boxplot(employee_data4$StockOptionLevel)
quantile(employee_data4$StockOptionLevel,seq(0,1,0.01))
employee_data4[which(employee_data4$StockOptionLevel > 2),'StockOptionLevel'] <- 2

boxplot(employee_data4$TotalWorkingYears)
quantile(employee_data4$TotalWorkingYears,seq(0,1,0.01))
employee_data4[which(employee_data4$TotalWorkingYears > 28),'TotalWorkingYears'] <- 28

boxplot(employee_data4$TrainingTimesLastYear)
quantile(employee_data4$TrainingTimesLastYear,seq(0,1,0.01))
employee_data4[which(employee_data4$TrainingTimesLastYear > 4),'TrainingTimesLastYear'] <- 4
employee_data4[which(employee_data4$TrainingTimesLastYear < 1),'TrainingTimesLastYear'] <- 1

boxplot(employee_data4$YearsAtCompany)
quantile(employee_data4$YearsAtCompany,seq(0,1,0.01))
employee_data4[which(employee_data4$YearsAtCompany > 19),'YearsAtCompany'] <- 19

boxplot(employee_data4$YearsSinceLastPromotion)
quantile(employee_data4$YearsSinceLastPromotion,seq(0,1,0.01))
employee_data4[which(employee_data4$YearsSinceLastPromotion > 7),'YearsSinceLastPromotion'] <- 7

boxplot(employee_data4$YearsWithCurrManager)
quantile(employee_data4$YearsWithCurrManager,seq(0,1,0.01))
employee_data4[which(employee_data4$YearsWithCurrManager > 14),'YearsWithCurrManager'] <- 14

boxplot(employee_data4$Avg_Hoursspent)
quantile(employee_data4$Avg_Hoursspent,seq(0,1,0.01))
employee_data4[which(employee_data4$Avg_Hoursspent > 10.903018),'Avg_Hoursspent'] <- 10.903018

boxplot(employee_data4$no_leaves)

#adding oveertime criteria
employee_data4$extend_hrs <- ifelse(employee_data4$Avg_Hoursspent > 8,1,0)
str(employee_data4)

# Feature standardisation
#scaling continuos variables

employee_data4$Age<- scale(employee_data4$Age) 
employee_data4$DistanceFromHome<- scale(employee_data4$DistanceFromHome) 
employee_data4$MonthlyIncome<- scale(employee_data4$MonthlyIncome) 
employee_data4$NumCompaniesWorked<- scale(employee_data4$NumCompaniesWorked) 
employee_data4$PercentSalaryHike<- scale(employee_data4$PercentSalaryHike) 
employee_data4$StockOptionLevel<- scale(employee_data4$StockOptionLevel) 
employee_data4$TotalWorkingYears<- scale(employee_data4$TotalWorkingYears) 
employee_data4$TrainingTimesLastYear<- scale(employee_data4$TrainingTimesLastYear) 
employee_data4$YearsAtCompany<- scale(employee_data4$YearsAtCompany) 
employee_data4$YearsSinceLastPromotion<- scale(employee_data4$YearsSinceLastPromotion) 
employee_data4$YearsWithCurrManager<- scale(employee_data4$YearsWithCurrManager) 
employee_data4$Avg_Hoursspent<- scale(employee_data4$Avg_Hoursspent)
employee_data4$no_leaves<- scale(employee_data4$no_leaves)

#Converting attributes with 2 levels into numbers(0,1)
employee_data4$Attrition <- ifelse(employee_data4$Attrition == "Yes", 1,0)
employee_data4$Gender <- ifelse(employee_data4$Gender == "Female",1,0)

# Checking attrition rate of employee
attrition_rate <- sum(employee_data4$Attrition)/nrow(employee_data4)
attrition_rate # 16.16% 

str(employee_data4)
# creating a dataframe of categorical features
employee_data4_fact<- employee_data4[,c(4,5,7,8,10,11,12,22,23,24,25,26)]

# converting categorical attributes to factor
employee_data4_fact<- data.frame(sapply(employee_data4_fact, function(x) factor(x)))
str(employee_data4_fact)

# creating dummy variables for factor attributes
dummies<- data.frame(sapply(employee_data4_fact, 
                            function(x) data.frame(model.matrix(~x-1,data =employee_data4_fact))))

# Final dataset
employee_data4_final<- cbind(employee_data4[,c(2,3,6,9,13,14,15,16,17,18,19,20,21,27,28,29)],dummies) 
View(employee_data4_final)
str(employee_data4_final)


########################################################################
# splitting the data between train and test
set.seed(100)

indices = sample.split(employee_data4_final$Attrition, SplitRatio = 0.7)

train = employee_data4_final[indices,]

test = employee_data4_final[!(indices),]

########################################################################
# Logistic Regression: 

#Initial model
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1)  

# Stepwise selection
model_2<- stepAIC(model_1, direction="both")
summary(model_2)

# Removing multicollinearity through VIF check
vif(model_2) #2076.9

#YearsAtCompany is having high vif and is insignificant, so removing it

model_3 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear  + YearsSinceLastPromotion + 
                 YearsWithCurrManager + extend_hrs + BusinessTravel.xNon.Travel + 
                 BusinessTravel.xTravel_Frequently + EducationField.xHuman.Resources + 
                 JobLevel.x2 + JobRole.xHuman.Resources + JobRole.xManager + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried + 
                 EnvironmentSatisfaction.x1 + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + JobSatisfaction.x1 + JobSatisfaction.x2 + 
                 JobSatisfaction.x3 + WorkLifeBalance.x1 + WorkLifeBalance.x3 + 
                 JobInvolvement.x3 + Education.x5, family = "binomial", data = train)

summary(model_3) #2077.4
vif(model_3)

#All the variables are having low vif values, let us remove them based on p value
#removing EnvironmentSatisfaction.x3 as it is insignificant

model_4 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear  + YearsSinceLastPromotion + 
                 YearsWithCurrManager + extend_hrs + BusinessTravel.xNon.Travel + 
                 BusinessTravel.xTravel_Frequently + EducationField.xHuman.Resources + 
                 JobLevel.x2 + JobRole.xHuman.Resources + JobRole.xManager + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried + 
                 EnvironmentSatisfaction.x1 + EnvironmentSatisfaction.x2 + 
                  JobSatisfaction.x1 + JobSatisfaction.x2 + 
                 JobSatisfaction.x3 + WorkLifeBalance.x1 + WorkLifeBalance.x3 + 
                 JobInvolvement.x3 + Education.x5, family = "binomial", data = train)

summary(model_4)
vif(model_4)

#Removing Education.x5 as it insignificant among others
model_5 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear  + YearsSinceLastPromotion + 
                 YearsWithCurrManager + extend_hrs + BusinessTravel.xNon.Travel + 
                 BusinessTravel.xTravel_Frequently + EducationField.xHuman.Resources + 
                 JobLevel.x2 + JobRole.xHuman.Resources + JobRole.xManager + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried + 
                 EnvironmentSatisfaction.x1 + EnvironmentSatisfaction.x2 + 
                 JobSatisfaction.x1 + JobSatisfaction.x2 + 
                 JobSatisfaction.x3 + WorkLifeBalance.x1 + WorkLifeBalance.x3 + 
                 JobInvolvement.x3 , family = "binomial", data = train)

summary(model_5)

#removing EnvironmentSatisfaction.x2 based on least significance
model_6 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear  + YearsSinceLastPromotion + 
                 YearsWithCurrManager + extend_hrs + BusinessTravel.xNon.Travel + 
                 BusinessTravel.xTravel_Frequently + EducationField.xHuman.Resources + 
                 JobLevel.x2 + JobRole.xHuman.Resources + JobRole.xManager + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried + 
                 EnvironmentSatisfaction.x1 + 
                 JobSatisfaction.x1 + JobSatisfaction.x2 + 
                 JobSatisfaction.x3 + WorkLifeBalance.x1 + WorkLifeBalance.x3 + 
                 JobInvolvement.x3 , family = "binomial", data = train)

summary(model_6)

#removing JobRole.xHuman.Resources based on least significance
model_7 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear  + YearsSinceLastPromotion + 
                 YearsWithCurrManager + extend_hrs + BusinessTravel.xNon.Travel + 
                 BusinessTravel.xTravel_Frequently + EducationField.xHuman.Resources + 
                 JobLevel.x2 +  JobRole.xManager + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried + 
                 EnvironmentSatisfaction.x1 + 
                 JobSatisfaction.x1 + JobSatisfaction.x2 + 
                 JobSatisfaction.x3 + WorkLifeBalance.x1 + WorkLifeBalance.x3 + 
                 JobInvolvement.x3 , family = "binomial", data = train)

summary(model_7)

#removing JobRole.xManager based on least significance
model_8 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear  + YearsSinceLastPromotion + 
                 YearsWithCurrManager + extend_hrs + BusinessTravel.xNon.Travel + 
                 BusinessTravel.xTravel_Frequently + EducationField.xHuman.Resources + 
                 JobLevel.x2 +  
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried + 
                 EnvironmentSatisfaction.x1 + 
                 JobSatisfaction.x1 + JobSatisfaction.x2 + 
                 JobSatisfaction.x3 + WorkLifeBalance.x1 + WorkLifeBalance.x3 + 
                 JobInvolvement.x3 , family = "binomial", data = train)

summary(model_8)

#removing JobInvolvement.x3 based on least significance
model_9 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear  + YearsSinceLastPromotion + 
                 YearsWithCurrManager + extend_hrs + BusinessTravel.xNon.Travel + 
                 BusinessTravel.xTravel_Frequently + EducationField.xHuman.Resources + 
                 JobLevel.x2 +  
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried + 
                 EnvironmentSatisfaction.x1 + 
                 JobSatisfaction.x1 + JobSatisfaction.x2 + 
                 JobSatisfaction.x3 + WorkLifeBalance.x1 + WorkLifeBalance.x3  , family = "binomial", data = train)

summary(model_9)

#removing WorkLifeBalance.x3 based on least significance
model_10 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear  + YearsSinceLastPromotion + 
                 YearsWithCurrManager + extend_hrs + BusinessTravel.xNon.Travel + 
                 BusinessTravel.xTravel_Frequently + EducationField.xHuman.Resources + 
                 JobLevel.x2 +  
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried + 
                 EnvironmentSatisfaction.x1 + 
                 JobSatisfaction.x1 + JobSatisfaction.x2 + 
                 JobSatisfaction.x3 + WorkLifeBalance.x1 , family = "binomial", data = train)

summary(model_10)

#removing TrainingTimesLastYear  based on least significance
model_11 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                   YearsSinceLastPromotion + 
                  YearsWithCurrManager + extend_hrs + BusinessTravel.xNon.Travel + 
                  BusinessTravel.xTravel_Frequently + EducationField.xHuman.Resources + 
                  JobLevel.x2 +  
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried + 
                  EnvironmentSatisfaction.x1 + 
                  JobSatisfaction.x1 + JobSatisfaction.x2 + 
                  JobSatisfaction.x3 + WorkLifeBalance.x1 , family = "binomial", data = train)

summary(model_11)

#removing JobRole.xResearch.Director based on significance value
model_12 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  YearsSinceLastPromotion + 
                  YearsWithCurrManager + extend_hrs + BusinessTravel.xNon.Travel + 
                  BusinessTravel.xTravel_Frequently + EducationField.xHuman.Resources + 
                  JobLevel.x2 +  
                  JobRole.xManufacturing.Director +  
                  JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried + 
                  EnvironmentSatisfaction.x1 + 
                  JobSatisfaction.x1 + JobSatisfaction.x2 + 
                  JobSatisfaction.x3 + WorkLifeBalance.x1 , family = "binomial", data = train)

summary(model_12)

#removing JobLevel.x2 based on significance factor
model_13 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  YearsSinceLastPromotion + 
                  YearsWithCurrManager + extend_hrs + BusinessTravel.xNon.Travel + 
                  BusinessTravel.xTravel_Frequently + EducationField.xHuman.Resources + 
                  JobRole.xManufacturing.Director +  
                  JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried + 
                  EnvironmentSatisfaction.x1 + 
                  JobSatisfaction.x1 + JobSatisfaction.x2 + 
                  JobSatisfaction.x3 + WorkLifeBalance.x1 , family = "binomial", data = train)

summary(model_13)

#removing BusinessTravel.xNon.Travel based on least significance values among others
model_14 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  YearsSinceLastPromotion + 
                  YearsWithCurrManager + extend_hrs +  
                  BusinessTravel.xTravel_Frequently + EducationField.xHuman.Resources + 
                  JobRole.xManufacturing.Director +  
                  JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried + 
                  EnvironmentSatisfaction.x1 + 
                  JobSatisfaction.x1 + JobSatisfaction.x2 + 
                  JobSatisfaction.x3 + WorkLifeBalance.x1 , family = "binomial", data = train)

summary(model_14)

#removing JobRole.xSales.Executive based on least significance among others
model_15 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  YearsSinceLastPromotion + 
                  YearsWithCurrManager + extend_hrs +  
                  BusinessTravel.xTravel_Frequently + EducationField.xHuman.Resources + 
                  JobRole.xManufacturing.Director +  
                   MaritalStatus.xDivorced + MaritalStatus.xMarried + 
                  EnvironmentSatisfaction.x1 + 
                  JobSatisfaction.x1 + JobSatisfaction.x2 + 
                  JobSatisfaction.x3 + WorkLifeBalance.x1 , family = "binomial", data = train)

summary(model_15)

#removing TotalWorkingYears based on least significance
model_16 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  YearsSinceLastPromotion + 
                  YearsWithCurrManager + extend_hrs +  
                  BusinessTravel.xTravel_Frequently + EducationField.xHuman.Resources + 
                  JobRole.xManufacturing.Director +  
                  MaritalStatus.xDivorced + MaritalStatus.xMarried + 
                  EnvironmentSatisfaction.x1 + 
                  JobSatisfaction.x1 + JobSatisfaction.x2 + 
                  JobSatisfaction.x3 + WorkLifeBalance.x1 , family = "binomial", data = train)

summary(model_16)

#removing JobSatisfaction.x2 based on least significance
model_17 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  YearsSinceLastPromotion + 
                  YearsWithCurrManager + extend_hrs +  
                  BusinessTravel.xTravel_Frequently + EducationField.xHuman.Resources + 
                  JobRole.xManufacturing.Director +  
                  MaritalStatus.xDivorced + MaritalStatus.xMarried + 
                  EnvironmentSatisfaction.x1 + 
                  JobSatisfaction.x1 + 
                  JobSatisfaction.x3 + WorkLifeBalance.x1 , family = "binomial", data = train)

summary(model_17)

#removing JobSatisfaction.x3  based on least significance
model_18 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  YearsSinceLastPromotion + 
                  YearsWithCurrManager + extend_hrs +  
                  BusinessTravel.xTravel_Frequently + EducationField.xHuman.Resources + 
                  JobRole.xManufacturing.Director +  
                  MaritalStatus.xDivorced + MaritalStatus.xMarried + 
                  EnvironmentSatisfaction.x1 + 
                  JobSatisfaction.x1 + 
                   WorkLifeBalance.x1 , family = "binomial", data = train)

summary(model_18) #AIC: 2128.8
vif(model_18)

#13 highly significant variables
final_model<- model_18
#######################################################################

### Model Evaluation

### Test Data ####
#predicted probabilities of Attrition for test data

test_pred = predict(final_model, type = "response", 
                    newdata = test[,-2])

# Let's see the summary 

summary(test_pred)

test$prob <- test_pred
View(test)

# Let's use the probability cutoff of 50%.

test_pred_attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))

table(test_pred_attrition,test_actual_attrition)
confusionMatrix(test_pred_attrition,test_actual_attrition, positive = "Yes")

#######################################################################
test_pred_attrition <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))

test_conf <- confusionMatrix(test_pred_attrition,test_actual_attrition, positive = "Yes")
test_conf
#######################################################################

# Finding optimal probability cuttoff 

perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Summary of test probability

summary(test_pred)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]
cutoff
# Let's choose a cutoff value of 0.1696 for final model

test_cutoff_attrition<- factor(ifelse(test_pred >=0.1696, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")
conf_final
acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc #0.7527132 

sens #0.7607656 

spec #0.7511563 

View(test)

##################################################################################################
### KS -statistic - Test Data ######


test_cutoff_attrition <- ifelse(test_cutoff_attrition=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)


library(ROCR)
#on testing  data
pred_object_test<- prediction(test_cutoff_attrition, test_actual_attrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test) #0.5119219


####################################################################
# Lift & Gain Chart 


lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

attrition_decile = lift(test_actual_attrition, test_pred, groups = 10)
attrition_decile
#Max are covered in the first 4 deciles with 80%gain

plot_grid(ggplot(attrition_decile,aes(x=attrition_decile$bucket,y=attrition_decile$Gain, color=""))+geom_line()+geom_point(),
          ggplot(attrition_decile,aes(x=attrition_decile$bucket,y=attrition_decile$Cumlift))+geom_line()+geom_point(), 
          align = "h",ncol = 1)


#Correlation of final fields of the model
cor(employee_data4_final$Attrition,employee_data4_final$Age)           # -0.1555844
cor(employee_data4_final$Attrition,employee_data4_final$NumCompaniesWorked)      #0.04254968
cor(employee_data4_final$Attrition,employee_data4_final$YearsSinceLastPromotion) #-0.03348072
cor(employee_data4_final$Attrition,employee_data4_final$YearsWithCurrManager)    #-0.1557193
cor(employee_data4_final$Attrition,employee_data4_final$extend_hrs)              #0.2248596
cor(employee_data4_final$Attrition,employee_data4_final$BusinessTravel.xTravel_Frequently)    #0.1103145
cor(employee_data4_final$Attrition,employee_data4_final$EducationField.xHuman.Resources)      #0.08915949
cor(employee_data4_final$Attrition,employee_data4_final$JobRole.xManufacturing.Director)      #-0.0429106
cor(employee_data4_final$Attrition,employee_data4_final$MaritalStatus.xDivorced)          #-0.09046467
cor(employee_data4_final$Attrition,employee_data4_final$MaritalStatus.xMarried)           #-0.0852691
cor(employee_data4_final$Attrition,employee_data4_final$EnvironmentSatisfaction.x1)   #0.1230147
cor(employee_data4_final$Attrition,employee_data4_final$JobSatisfaction.x1 )          #0.0907044
cor(employee_data4_final$Attrition,employee_data4_final$WorkLifeBalance.x1)           #0.09668649
