setwd("C:/Users/prashanth j/Documents/Data sets/case study")
install.packages("cowplot")
install.packages("plotly")
library(tidyverse) 
library(MASS) 
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)
library(lubridate)
library(ROCR)
library(plotly)

# reading all the cv files
emp_general<-read.csv("general_data.csv",stringsAsFactors = F)
emp_suvey<-read.csv("employee_survey_data.csv",stringsAsFactors = F)
emp_manager_suvey<-read.csv("manager_survey_data.csv",stringsAsFactors = F)
emp_intime<-read.csv("in_time.csv",stringsAsFactors = F)
emp_outime<-read.csv("out_time.csv",stringsAsFactors = F)

str(emp_general)
str(emp_suvey)
str(emp_manager_suvey)
str(emp_intime)
str(emp_outime)

# adding first col header to both dataframes.
colnames(emp_intime)[1]<-"EmployeeID"
colnames(emp_outime)[1]<-"EmployeeID"

#Exploring all the dataframes
length(unique(emp_general$EmployeeID))
length(unique(emp_manager_suvey$EmployeeID))
length(unique(emp_intime$EmployeeID))
length(unique(emp_outime$EmployeeID))
length(unique(emp_suvey$EmployeeID))

#identical employeeid across datasets
setdiff(emp_general$EmployeeID,emp_suvey$EmployeeID)
setdiff(emp_general$EmployeeID,emp_manager_suvey$EmployeeID)
setdiff(emp_general$EmployeeID,emp_intime$EmployeeID)
setdiff(emp_general$EmployeeID,emp_outime$EmployeeID)

#Converting columns into appropriate date & time format
emp_id<-emp_intime[1]
emp_intime <- as.data.frame(lapply( emp_intime[, -1], as.POSIXlt))
emp_outime <- as.data.frame(lapply( emp_outime[, -1], as.POSIXlt))

#Comparing the column headers in both the dataframes
setdiff(colnames(emp_intime), colnames(emp_outime))

#As all the coulms are same so substracting both the dataframes
#calculating difference of times to get actual login hours
emp_login <-  emp_outime-emp_intime

#Converting login hours to 2 decimal points
emp_login <- as.data.frame(lapply(emp_login, round, digits=2))

#To delete all such columns when everybody was NA's.
emp_login <- emp_login[,colSums(is.na(emp_login))<nrow(emp_login)]
emp_login<-cbind(emp_id,emp_login)
typeof(emp_login)

#converting all columns into numeric
emp_login<- as.data.frame(sapply(emp_login, as.numeric))

sapply(emp_login,function(x) sum(is.na(x)))

#To calculate average login hours for the employee
emp_login$emp_avg_login <- apply(emp_login[,-1], 1, mean, na.rm=TRUE)

#Boxplot to explore the average hours and outliers
boxplot(emp_login$emp_avg_login)
emp_login$emp_avg_login

#round of the average working hours
emp_login$emp_avg_login<-round(emp_login$emp_avg_login, 0)

#bucketing 3 categories by consdering avg login hours
emp_login$emp_login_cat<- ifelse(emp_login$emp_avg_login > 8, "overtime",ifelse(emp_login$emp_avg_login > 7 & emp_login$emp_avg_login<=8,"regular","early logout"))

#extrating only 3 columns to merge with other dataset's
emp_login<-emp_login[, c(1,251,252)]

#Merging all the dataset and creating master dataframe as "hrdata"
hrdata<-merge(emp_general,emp_suvey, by="EmployeeID", all = F)
hrdata<- merge(hrdata,emp_manager_suvey, by="EmployeeID", all = F)
hrdata<- merge(hrdata,emp_login, by="EmployeeID", all = F)

str(hrdata)
Attritionrate <- sum(hrdata$Attrition)/nrow(hrdata)
Attritionrate 

sapply(hrdata,function(x) sum(is.na(x)))
sum(is.na(hrdata))/nrow(hrdata)  #it is 2.5% of total so computing these NA Values

#NA value computation

hrdata$NumCompaniesWorked[which(is.na(hrdata$NumCompaniesWorked))]<-median(hrdata$NumCompaniesWorked, na.rm = TRUE)
hrdata$TotalWorkingYears[which(is.na(hrdata$TotalWorkingYears))]<-median(hrdata$TotalWorkingYears, na.rm = TRUE)
hrdata$EnvironmentSatisfaction[which(is.na(hrdata$EnvironmentSatisfaction))]<-median(hrdata$EnvironmentSatisfaction, na.rm = TRUE)
hrdata$JobSatisfaction[which(is.na(hrdata$JobSatisfaction))]<-median(hrdata$JobSatisfaction, na.rm = TRUE)
hrdata$WorkLifeBalance[which(is.na(hrdata$WorkLifeBalance))]<-median(hrdata$WorkLifeBalance, na.rm = TRUE)

#removing columns which are having only single type value
hrdata<-hrdata[, -c(9,16,18)]

# we are converting the numeric values to categorical variables
hrdata$Education[which(hrdata$Education==1)]<-'Below College'
hrdata$Education[which(hrdata$Education==2)]<-'College'
hrdata$Education[which(hrdata$Education==3)]<-'Bachelor'
hrdata$Education[which(hrdata$Education==4)]<-'Master'
hrdata$Education[which(hrdata$Education==5)]<-'Doctor'

hrdata$EnvironmentSatisfaction[which(hrdata$EnvironmentSatisfaction==1)]<-'Low'
hrdata$EnvironmentSatisfaction[which(hrdata$EnvironmentSatisfaction==2)]<-'Medium'
hrdata$EnvironmentSatisfaction[which(hrdata$EnvironmentSatisfaction==3)]<-'High'
hrdata$EnvironmentSatisfaction[which(hrdata$EnvironmentSatisfaction==4)]<-'Very High'

hrdata$JobInvolvement[which(hrdata$JobInvolvement==1)]<-'Low'
hrdata$JobInvolvement[which(hrdata$JobInvolvement==2)]<-'Medium'
hrdata$JobInvolvement[which(hrdata$JobInvolvement==3)]<-'High'
hrdata$JobInvolvement[which(hrdata$JobInvolvement==4)]<-'Very High'

hrdata$JobSatisfaction[which(hrdata$JobSatisfaction==1)]<-'Low'
hrdata$JobSatisfaction[which(hrdata$JobSatisfaction==2)]<-'Medium'
hrdata$JobSatisfaction[which(hrdata$JobSatisfaction==3)]<-'High'
hrdata$JobSatisfaction[which(hrdata$JobSatisfaction==4)]<-'Very High'

hrdata$WorkLifeBalance[which(hrdata$WorkLifeBalance==1)]<-'Bad'
hrdata$WorkLifeBalance[which(hrdata$WorkLifeBalance==2)]<-'Good'
hrdata$WorkLifeBalance[which(hrdata$WorkLifeBalance==3)]<-'Better'
hrdata$WorkLifeBalance[which(hrdata$WorkLifeBalance==4)]<-'Best'

hrdata$PerformanceRating[which(hrdata$PerformanceRating==1)]<-'Low'
hrdata$PerformanceRating[which(hrdata$PerformanceRating==2)]<-'Good'
hrdata$PerformanceRating[which(hrdata$PerformanceRating==3)]<-'Excellent'
hrdata$PerformanceRating[which(hrdata$PerformanceRating==4)]<-'Outstanding'

str(hrdata)
#Explorartory Data Analysis 

# Barcharts for categorical features with stacked Attrition information
plot_grid(ggplot(hrdata, aes(x=factor(BusinessTravel),fill=Attrition))+ geom_bar(position ="fill"))
plot_grid(ggplot(hrdata, aes(x=factor(Department),fill=Attrition))+ geom_bar(position ="fill"))
plot_grid(ggplot(hrdata, aes(x=factor(EducationField),fill=Attrition))+ geom_bar(position ="fill"))
plot_grid(ggplot(hrdata, aes(x=factor(MaritalStatus),fill=Attrition))+ geom_bar(position ="fill"))
plot_grid(ggplot(hrdata, aes(x=factor(Gender),fill=Attrition))+ geom_bar(position ="fill"))
plot_grid(ggplot(hrdata, aes(x=factor(EnvironmentSatisfaction),fill=Attrition))+ geom_bar(position ="fill"))
plot_grid(ggplot(hrdata, aes(x=factor(emp_login_cat),fill=Attrition))+ geom_bar(position ="fill"))
plot_grid(ggplot(hrdata, aes(x=factor(Education),fill=Attrition))+ geom_bar(position ="fill"))
plot_grid(ggplot(hrdata, aes(x=factor(WorkLifeBalance),fill=Attrition))+ geom_bar(position ="fill"))
plot_grid(ggplot(hrdata, aes(x=factor(JobInvolvement),fill=Attrition))+ geom_bar(position ="fill"))
plot_grid(ggplot(hrdata, aes(x=factor(JobSatisfaction),fill=Attrition))+ geom_bar(position ="fill"))
plot_grid(ggplot(hrdata, aes(x=factor(PerformanceRating),fill=Attrition))+ geom_bar(position ="fill"))
plot_grid(ggplot(hrdata, aes(x=factor(JobRole),fill=Attrition))+ geom_bar(position ="fill"))

# Histogram and Boxplots for numeric variables
plot_grid(ggplot(hrdata, aes(Age))+ geom_histogram(binwidth = 10),ggplot(hrdata, aes(x="",y=Age))+ geom_boxplot(width=0.1)+coord_flip(), align = "v",ncol = 1)
#No outliers in Age variable

plot_grid(ggplot(hrdata, aes(DistanceFromHome))+ geom_histogram(binwidth = 10),ggplot(hrdata, aes(x="",y=DistanceFromHome))+ geom_boxplot(width=0.1)+coord_flip(), align = "v",ncol = 1)
#No outliers in "DistanceFromHome" variable

plot_grid(ggplot(hrdata, aes(MonthlyIncome))+ geom_histogram(binwidth = 10),ggplot(hrdata, aes(x="",y=MonthlyIncome))+ geom_boxplot(width=0.1)+coord_flip(), align = "v",ncol = 1)
quantile(hrdata$MonthlyIncome, seq(0,1,0.01))
#Although there is a gradual increase in salary but outlier is available 

plot_grid(ggplot(hrdata, aes(PercentSalaryHike))+ geom_histogram(binwidth = 10),ggplot(hrdata, aes(x="",y=PercentSalaryHike))+ geom_boxplot(width=0.1)+coord_flip(), align = "v",ncol = 1)
#No outliers in "PercentSalaryHike" variable

plot_grid(ggplot(hrdata, aes(TotalWorkingYears))+ geom_histogram(binwidth = 10),ggplot(hrdata, aes(x="",y=TotalWorkingYears))+ geom_boxplot(width=0.1)+coord_flip(), align = "v",ncol = 1)
quantile(hrdata$TotalWorkingYears, seq(0,1,0.01))
#Outliers can be seen in boxplot

plot_grid(ggplot(hrdata, aes(hrdata$YearsSinceLastPromotion))+ geom_histogram(binwidth = 10),ggplot(hrdata, aes(x="",y=hrdata$YearsSinceLastPromotion))+ geom_boxplot(width=0.1)+coord_flip(), align = "v",ncol = 1)
quantile(hrdata$YearsSinceLastPromotion, seq(0,1,0.01))
#Outliers can be seen in boxplot

plot_grid(ggplot(hrdata, aes(hrdata$YearsAtCompany))+ geom_histogram(binwidth = 10),ggplot(hrdata, aes(x="",y=hrdata$YearsAtCompany))+ geom_boxplot(width=0.1)+coord_flip(), align = "v",ncol = 1)
quantile(hrdata$YearsAtCompany, seq(0,1,0.01))
#Outliers can be seen in boxplot

plot_grid(ggplot(hrdata, aes(hrdata$YearsWithCurrManager))+ geom_histogram(binwidth = 10),ggplot(hrdata, aes(x="",y=hrdata$YearsWithCurrManager))+ geom_boxplot(width=0.1)+coord_flip(), align = "v",ncol = 1)
quantile(hrdata$YearsWithCurrManager, seq(0,1,0.01))
#Outliers can be seen in boxplot

plot_grid(ggplot(hrdata, aes(hrdata$emp_avg_login))+ geom_histogram(binwidth = 10),ggplot(hrdata, aes(x="",y=hrdata$emp_avg_login))+ geom_boxplot(width=0.1)+coord_flip(), align = "v",ncol = 1)
quantile(hrdata$emp_avg_login, seq(0,1,0.01))
#Outliers can be seen in boxplot

plot_grid(ggplot(hrdata, aes(hrdata$JobLevel))+ geom_histogram(binwidth = 10),ggplot(hrdata, aes(x="",y=hrdata$JobLevel))+ geom_boxplot(width=0.1)+coord_flip(), align = "v",ncol = 1)
#No Outliers can be seen in boxplot

plot_grid(ggplot(hrdata, aes(hrdata$NumCompaniesWorked))+ geom_histogram(binwidth = 10),ggplot(hrdata, aes(x="",y=hrdata$NumCompaniesWorked))+ geom_boxplot(width=0.1)+coord_flip(), align = "v",ncol = 1)
quantile(hrdata$NumCompaniesWorked, seq(0,1,0.01))
#Outliers can be seen in boxplot

plot_grid(ggplot(hrdata, aes(hrdata$StockOptionLevel))+ geom_histogram(binwidth = 10),ggplot(hrdata, aes(x="",y=hrdata$StockOptionLevel))+ geom_boxplot(width=0.1)+coord_flip(), align = "v",ncol = 1)
quantile(hrdata$StockOptionLevel, seq(0,1,0.01))
#Outliers can be seen in boxplot

plot_grid(ggplot(hrdata, aes(hrdata$TrainingTimesLastYear))+ geom_histogram(binwidth = 10),ggplot(hrdata, aes(x="",y=hrdata$TrainingTimesLastYear))+ geom_boxplot(width=0.1)+coord_flip(), align = "v",ncol = 1)
quantile(hrdata$TrainingTimesLastYear, seq(0,1,0.01))
#Outliers can be seen in boxplot

# subsitting the continues varaibles
hrdatacat <- hrdata[c(2,6,10,12,13,14,15,16,17,18,19,20,21,27)]
head(hrdatacat)

# correlation for continues varaibles
res <- cor(hrdatacat, method= "pearson")
corrplot::corrplot(res, method = "circle")


#  data preparation 

# outliers treatment
box <- boxplot.stats(hrdata$YearsAtCompany)
out <- box$out
ad1 <- hrdata[ !hrdata$YearsAtCompany %in% out, ]
hrdata <- ad1

box <- boxplot.stats(hrdata$emp_avg_login)
out <- box$out
ad1 <- hrdata[ !hrdata$emp_avg_login %in% out, ]
hrdata <- ad1

box <- boxplot.stats(hrdata$NumCompaniesWorked)
out <- box$out
ad1 <- hrdata[ !hrdata$NumCompaniesWorked %in% out, ]
hrdata <- ad1

box <- boxplot.stats(hrdata$StockOptionLevel)
out <- box$out
ad1 <- hrdata[ !hrdata$StockOptionLevel %in% out, ]
hrdata <- ad1

box <- boxplot.stats(hrdata$TotalWorkingYears)
out <- box$out
ad1 <- hrdata[ !hrdata$TotalWorkingYears %in% out, ]
hrdata <- ad1

box <- boxplot.stats(hrdata$TrainingTimesLastYear)
out <- box$out
ad1 <- hrdata[ !hrdata$TrainingTimesLastYear %in% out, ]
hrdata <- ad1

box <- boxplot.stats(hrdata$YearsSinceLastPromotion)
out <- box$out
ad1 <- hrdata[ !hrdata$YearsSinceLastPromotion %in% out, ]
hrdata <- ad1

box <- boxplot.stats(hrdata$YearsWithCurrManager)
out <- box$out
ad1 <- hrdata[ !hrdata$YearsWithCurrManager %in% out, ]
hrdata <- ad1

#Converting specific columns to factors just for templorary purpose
hrdata$EmployeeID<-as.factor(hrdata$EmployeeID)

#Feature scaling
num_var<-sapply(hrdata, is.numeric)
hrdata[num_var]<-lapply(hrdata[num_var], scale)
hrdata$EmployeeID<-as.numeric(hrdata$EmployeeID)

# converting target variable Attrition from No/Yes character to factorwith levels 0/1 
hrdata$Attrition<- ifelse(hrdata$Attrition=="Yes",1,0)

# Checking Attrition rate of prospect employee
Attritionrate <- sum(hrdata$Attrition)/nrow(hrdata)
Attritionrate 

#Creating categorical subset from the dataset
Cat_var<- hrdata[, c("Education","BusinessTravel", "EnvironmentSatisfaction", "Department", "EducationField","Gender","JobRole","MaritalStatus", "JobSatisfaction","WorkLifeBalance", "JobInvolvement", "PerformanceRating", "emp_login_cat")]

# converting categorical attributes to factor
hrdata_factor<- data.frame(sapply(Cat_var, function(x) factor(x)))
str(hrdata_factor)

#creating dummy variables for factor attributes
dummies<- data.frame(sapply(hrdata_factor, function(x) data.frame(model.matrix(~x-1,data =hrdata_factor))))

#combining the final dataset
hrdata<-hrdata[, !(names(hrdata)) %in% c( "Education","BusinessTravel", "EnvironmentSatisfaction", "Department", "EducationField","Gender","JobRole","MaritalStatus",  "JobSatisfaction","WorkLifeBalance", "JobInvolvement", "PerformanceRating", "emp_login_cat","EmployeeID")]
hrdata_final<-cbind(hrdata,dummies)
str(hrdata_final)

# splitting the data between train and test
set.seed(100)
indices = sample.split(hrdata_final$Attrition, SplitRatio = 0.7)
train = hrdata_final[indices,]
test = hrdata_final[!(indices),]

# logistic regression
#initial model
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1) #AIC: 963.3 ....Residual deviance: 857.3

library("MASS")
model_2<- stepAIC(model_1, direction="both")
summary(model_2)  #AIC: 939.52...Residual deviance: 871.52

# Removing multicollinearity through VIF check
library(car)
vif(model_2)

#Remove WorkLifeBalance.xBad as it is not significant p=0.140627    >0.05   
model_3<-glm(formula = Attrition ~ Age + DistanceFromHome + JobLevel + NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xNon.Travel + BusinessTravel.xTravel_Frequently + EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xMedium + Department.xHuman.Resources + EducationField.xLife.Sciences + EducationField.xMedical + Gender.xFemale + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + JobRole.xResearch.Scientist + JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried + JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + WorkLifeBalance.xBetter + JobInvolvement.xHigh + JobInvolvement.xLow + JobInvolvement.xMedium + emp_login_cat.xearly.logout + emp_login_cat.xovertime, family = "binomial", data = train)
summary(model_3) #AIC: 939.64  ....Residual deviance:  873.64  
vif(model_3)

#Remove BusinessTravel.xNon.Travel as it is not significant p=0.118110   >0.05   
model_4<-glm(formula = Attrition ~ Age + DistanceFromHome + JobLevel + NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xMedium + Department.xHuman.Resources + EducationField.xLife.Sciences + EducationField.xMedical + Gender.xFemale + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + JobRole.xResearch.Scientist + JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried + JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + WorkLifeBalance.xBetter + JobInvolvement.xHigh + JobInvolvement.xLow + JobInvolvement.xMedium + emp_login_cat.xearly.logout + emp_login_cat.xovertime, family = "binomial", data = train)
summary(model_4) #AIC: 940.23  ...Residual deviance:  876.23
vif(model_4)

#Remove JobLevel as it is not significant p=0.107124     >0.05 
model_5<-glm(formula = Attrition ~ Age + DistanceFromHome  + NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xMedium + Department.xHuman.Resources + EducationField.xLife.Sciences + EducationField.xMedical + Gender.xFemale + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + JobRole.xResearch.Scientist + JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried + JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + WorkLifeBalance.xBetter + JobInvolvement.xHigh + JobInvolvement.xLow + JobInvolvement.xMedium + emp_login_cat.xearly.logout + emp_login_cat.xovertime, family = "binomial", data = train)
summary(model_5) #AIC: 940.89   ..Residual deviance:  878.89
vif(model_5)

#Remove JobInvolvement.xMedium as it is not significant p=0.085592 . and VIF =2.781500  
model_6<-glm(formula = Attrition ~ Age + DistanceFromHome  + NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xMedium + Department.xHuman.Resources + EducationField.xLife.Sciences + EducationField.xMedical + Gender.xFemale + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + JobRole.xResearch.Scientist + JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried + JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + WorkLifeBalance.xBetter + JobInvolvement.xHigh + JobInvolvement.xLow  + emp_login_cat.xearly.logout + emp_login_cat.xovertime, family = "binomial", data = train)
summary(model_6) #AIC: 941.78
vif(model_6)

#Remove PercentSalaryHike as it is not significant p=0.069206 .
model_7<-glm(formula = Attrition ~ Age + DistanceFromHome  + NumCompaniesWorked +  TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xMedium + Department.xHuman.Resources + EducationField.xLife.Sciences + EducationField.xMedical + Gender.xFemale + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + JobRole.xResearch.Scientist + JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried + JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + WorkLifeBalance.xBetter + JobInvolvement.xHigh + JobInvolvement.xLow  + emp_login_cat.xearly.logout + emp_login_cat.xovertime, family = "binomial", data = train)
summary(model_7) #AIC: 943.06  Residual deviance:  885.06 
vif(model_7)

#Remove YearsSinceLastPromotion as it is not significant p=0.024280 *
model_8<-glm(formula = Attrition ~ Age + DistanceFromHome  + NumCompaniesWorked +  TotalWorkingYears + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xMedium + Department.xHuman.Resources + EducationField.xLife.Sciences + EducationField.xMedical + Gender.xFemale + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + JobRole.xResearch.Scientist + JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried + JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + WorkLifeBalance.xBetter + JobInvolvement.xHigh + JobInvolvement.xLow  + emp_login_cat.xearly.logout + emp_login_cat.xovertime, family = "binomial", data = train)
summary(model_8) #AIC: 946.5  Residual deviance:  890.
vif(model_8)

#Remove Department.xHuman.Resources as it is not significant p=0.016397 * 
model_9<-glm(formula = Attrition ~ Age + DistanceFromHome  + NumCompaniesWorked +  TotalWorkingYears + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xMedium + EducationField.xLife.Sciences + EducationField.xMedical + Gender.xFemale + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + JobRole.xResearch.Scientist + JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried + JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + WorkLifeBalance.xBetter + JobInvolvement.xHigh + JobInvolvement.xLow  + emp_login_cat.xearly.logout + emp_login_cat.xovertime, family = "binomial", data = train)
summary(model_9) #AIC: 949.98   Residual deviance:  895.98
vif(model_9)

#Remove JobInvolvement.xHigh    as it is not significant p=0.013394 * 
model_10<-glm(formula = Attrition ~ Age + DistanceFromHome  + NumCompaniesWorked +  TotalWorkingYears + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xMedium + EducationField.xLife.Sciences + EducationField.xMedical + Gender.xFemale + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + JobRole.xResearch.Scientist + JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried + JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + WorkLifeBalance.xBetter + JobInvolvement.xLow  + emp_login_cat.xearly.logout + emp_login_cat.xovertime, family = "binomial", data = train)
summary(model_10) #AIC: 954.09   Residual deviance:  902.09
vif(model_10)

#Remove JobInvolvement.xLow  as it is not significant p=0.015052 * 
model_11<-glm(formula = Attrition ~ Age + DistanceFromHome  + NumCompaniesWorked +  TotalWorkingYears + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xMedium + EducationField.xLife.Sciences + EducationField.xMedical + Gender.xFemale + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + JobRole.xResearch.Scientist + JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried + JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + WorkLifeBalance.xBetter  + emp_login_cat.xearly.logout + emp_login_cat.xovertime, family = "binomial", data = train)
summary(model_11) #AIC:958.75  Residual deviance:  908.75
vif(model_11)

#Remove WorkLifeBalance.xBetter  as it is not significant p=0.012204 * 
model_12<-glm(formula = Attrition ~ Age + DistanceFromHome  + NumCompaniesWorked +  TotalWorkingYears + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xMedium + EducationField.xLife.Sciences + EducationField.xMedical + Gender.xFemale + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + JobRole.xResearch.Scientist + JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried + JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + emp_login_cat.xearly.logout + emp_login_cat.xovertime, family = "binomial", data = train)
summary(model_12) #AIC:963.01   Residual deviance:  915.01
vif(model_12)

#Remove JobRole.xLaboratory.Technician  as it is not significant p=0.005316 **  
model_13<-glm(formula = Attrition ~ Age + DistanceFromHome  + NumCompaniesWorked +  TotalWorkingYears + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xMedium + EducationField.xLife.Sciences + EducationField.xMedical + Gender.xFemale+ JobRole.xResearch.Director + JobRole.xResearch.Scientist + JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried + JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + emp_login_cat.xearly.logout + emp_login_cat.xovertime, family = "binomial", data = train)
summary(model_13) #AIC:968.66    Residual deviance:  922.66
vif(model_13)

#Remove JobRole.xSales.Executive  as it is not significant p=0.015808 *
model_14<-glm(formula = Attrition ~ Age + DistanceFromHome  + NumCompaniesWorked +  TotalWorkingYears + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xMedium + EducationField.xLife.Sciences + EducationField.xMedical + Gender.xFemale+ JobRole.xResearch.Director + JobRole.xResearch.Scientist + MaritalStatus.xDivorced + MaritalStatus.xMarried + JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + emp_login_cat.xearly.logout + emp_login_cat.xovertime, family = "binomial", data = train)
summary(model_14) #AIC: 972.4   Residual deviance:  928.4 
vif(model_14)

#Remove JobRole.xResearch.Scientist  as it is not significant p=0.024741 *
model_15<-glm(formula = Attrition ~ Age + DistanceFromHome  + NumCompaniesWorked +  TotalWorkingYears + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xMedium + EducationField.xLife.Sciences + EducationField.xMedical + Gender.xFemale+ JobRole.xResearch.Director+ MaritalStatus.xDivorced + MaritalStatus.xMarried + JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + emp_login_cat.xearly.logout + emp_login_cat.xovertime, family = "binomial", data = train)
summary(model_15) #AIC: 975.3  Residual deviance:  933.3
vif(model_15)

#Remove EnvironmentSatisfaction.xMedium  as it is not significant p=0.00988 **
model_16<-glm(formula = Attrition ~ Age + DistanceFromHome  + NumCompaniesWorked +  TotalWorkingYears + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xLow + EducationField.xLife.Sciences + EducationField.xMedical + Gender.xFemale+ JobRole.xResearch.Director+ MaritalStatus.xDivorced + MaritalStatus.xMarried + JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + emp_login_cat.xearly.logout + emp_login_cat.xovertime, family = "binomial", data = train)
summary(model_16) #AIC: 979.97  Residual deviance:  939.97
vif(model_16)

#Remove EnvironmentSatisfaction.xHigh   as it is not significant p=0.05032 .  
model_17<-glm(formula = Attrition ~ Age + DistanceFromHome  + NumCompaniesWorked +  TotalWorkingYears + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + EnvironmentSatisfaction.xLow + EducationField.xLife.Sciences + EducationField.xMedical + Gender.xFemale+ JobRole.xResearch.Director+ MaritalStatus.xDivorced + MaritalStatus.xMarried + JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + emp_login_cat.xearly.logout + emp_login_cat.xovertime, family = "binomial", data = train)
summary(model_17) #AIC: 981.78  Residual deviance:  943.78
vif(model_17)

#Remove EducationField.xMedical  as it is not significant p=0.005906 **
model_18<-glm(formula = Attrition ~ Age + DistanceFromHome  + NumCompaniesWorked +  TotalWorkingYears + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + EnvironmentSatisfaction.xLow + EducationField.xLife.Sciences  + Gender.xFemale+ JobRole.xResearch.Director+ MaritalStatus.xDivorced + MaritalStatus.xMarried + JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + emp_login_cat.xearly.logout + emp_login_cat.xovertime, family = "binomial", data = train)
summary(model_18) #AIC: 987.53  Residual deviance:  951.53
vif(model_18)

#Remove EducationField.xLife.Sciences  as it is less significant p=0.107336
model_19<-glm(formula = Attrition ~ Age + DistanceFromHome  + NumCompaniesWorked +  TotalWorkingYears + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + EnvironmentSatisfaction.xLow + Gender.xFemale+ JobRole.xResearch.Director+ MaritalStatus.xDivorced + MaritalStatus.xMarried + JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + emp_login_cat.xearly.logout + emp_login_cat.xovertime, family = "binomial", data = train)
summary(model_19) #AIC: 988.12   Residual deviance:  954.12
vif(model_19)

#Remove DistanceFromHome  as it is less significant p=0.005053 ** 
model_20<-glm(formula = Attrition ~ Age + NumCompaniesWorked +  TotalWorkingYears + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + EnvironmentSatisfaction.xLow + Gender.xFemale+ JobRole.xResearch.Director+ MaritalStatus.xDivorced + MaritalStatus.xMarried + JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + emp_login_cat.xearly.logout + emp_login_cat.xovertime, family = "binomial", data = train)
summary(model_20) #AIC: 994.35   Residual deviance:  962.35 
vif(model_20)

#Remove Age  as it is less significant with p =0.002584 ** 
model_21<-glm(formula = Attrition ~ NumCompaniesWorked +  TotalWorkingYears + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + EnvironmentSatisfaction.xLow + Gender.xFemale+ JobRole.xResearch.Director+ MaritalStatus.xDivorced + MaritalStatus.xMarried + JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + emp_login_cat.xearly.logout + emp_login_cat.xovertime, family = "binomial", data = train)
summary(model_21) #AIC: 1001.9  Residual deviance:  971.93 
vif(model_21)

#Remove JobRole.xResearch.Director  as it is less significant with p =0.004042 ** 
model_22<-glm(formula = Attrition ~ NumCompaniesWorked +  TotalWorkingYears + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + EnvironmentSatisfaction.xLow + Gender.xFemale+ MaritalStatus.xDivorced + MaritalStatus.xMarried + JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + emp_login_cat.xearly.logout + emp_login_cat.xovertime, family = "binomial", data = train)
summary(model_22) #AIC: 1007.7  Residual deviance:  979.73 
vif(model_22)

#Remove Gender.xFemale  as it is comparatively less significant with p =0.000258 ***
model_23<-glm(formula = Attrition ~  NumCompaniesWorked +  TotalWorkingYears + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + EnvironmentSatisfaction.xLow+ MaritalStatus.xDivorced + MaritalStatus.xMarried + JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + emp_login_cat.xearly.logout + emp_login_cat.xovertime, family = "binomial", data = train)
summary(model_23) #AIC: 1019.2  Residual deviance:  993.17 
vif(model_23)

#Remove JobSatisfaction.xHigh  as it has high VIF = 2.028513
model_24<-glm(formula = Attrition ~ NumCompaniesWorked +  TotalWorkingYears + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + EnvironmentSatisfaction.xLow+ MaritalStatus.xDivorced + MaritalStatus.xMarried + JobSatisfaction.xLow + JobSatisfaction.xMedium + emp_login_cat.xearly.logout + emp_login_cat.xovertime, family = "binomial", data = train)
summary(model_24) #AIC: 1050.5  Residual deviance:  1026.5 
vif(model_24)

#Remove JobSatisfaction.xMedium   as it has high VIF = 0.0277 *
model_25<-glm(formula = Attrition ~ NumCompaniesWorked +  TotalWorkingYears + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + EnvironmentSatisfaction.xLow+ MaritalStatus.xDivorced + MaritalStatus.xMarried + JobSatisfaction.xLow +  emp_login_cat.xearly.logout + emp_login_cat.xovertime, family = "binomial", data = train)
summary(model_25) #AIC: 1053.2  Residual deviance:  1031.2 
vif(model_25)

### Model Evaluation
### Test Data ####

#predicted probabilities of Attrition for test data
test_pred = predict(model_25, type = "response", newdata = test[,-2])
summary(test_pred)
test$prob <- test_pred

# Let's use the probability cutoff of 50%
test_pred_Attrition <- factor(ifelse(test_pred >= 0.50, "Yes","No"))
test_actual_Attrition <- factor(ifelse(test$Attrition==1, "Yes","No"))
table(test_pred_Attrition, test_actual_Attrition)
#Accuracy 85.9%
#Attrition Accuracy(Sensitivity)= 30%
#Non Attrition Accuracy(Specificity)=96.2%
  

#Let's check with different cut-off values , i.e at 40% instead 50%
test_cutoff_Attrition <- factor(ifelse(test_pred >= 0.40 , "Yes", "No"))
test_conf <- confusionMatrix(test_pred_Attrition, test_actual_Attrition, positive = "Yes")
test_conf

caret::confusionMatrix(test_pred_Attrition, test_actual_Attrition, positive = "Yes")
#Accuracy=85%
#Attrition Accuracy(Sensitivity)= 46%
#Non Attrition Accuracy(Specificity)=92%

# In order to Choose the cutoff value. 
# Let's find out the optimal probalility cutoff 
perform_fn <- function(cutoff) { predicted_Attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
conf <- confusionMatrix(predicted_Attrition, test_actual_Attrition, positive = "Yes")
acc <- conf$overall[1]
sens <- conf$byClass[1]
spec <- conf$byClass[2]
out <- t(as.matrix(c(sens, spec, acc))) 
colnames(out) <- c("sensitivity", "specificity", "accuracy")
return(out)
}

#creating a cutoff values and initializing a matrix of 100*3
summary(test_pred)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)

for(i in 1:100){OUT[i,] = perform_fn(s[i])} 

cutoff <- s[which(abs(OUT[,1]- OUT[,2])<0.02)]
cutoff

# Let's choose a cutoff value of 0.1616 for final model

test_cutoff_Attrition <- factor(ifelse(test_pred >=0.1616, "Yes", "No"))
conf_final <- confusionMatrix(test_cutoff_Attrition, test_actual_Attrition, positive = "Yes")
acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]


test_cutoff_Attrition <- ifelse(test_cutoff_Attrition=="Yes",1,0)
test_actual_Attrition <- ifelse(test_actual_Attrition=="Yes",1,0)


pred_object_test<- prediction(test_cutoff_Attrition, test_actual_Attrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

plot(performance_measures_test, colorize=T)



install.packages("InformationValue")

library(InformationValue)
optCutOff <- optimalCutoff(test$Attrition, test_pred)[1]
plotROC(test$Attrition, test_pred)




