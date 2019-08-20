setwd("C:/Users/prashanth j/Documents/Data sets")
install.packages("Amelia") # for visualization
library(Amelia)

train <- read.csv("train.csv",na.strings =c(""," ",NA,"NA"), header=T)
test <- read.csv("test.csv",na.strings =c(""," ",NA,"NA"), header=T)
str(train)
str(test)

#checking for missing values

sapply(train, function(x) sum(is.na(x)))
# we found that 177 age , 687 cabin and 2 embarked missing values in train dataset

sapply(test, function(x) sum(is.na(x)))
# we found that 86 age , 327 cabin and 1 fare missing values in test dataset

# add a column survived in test dataset
test$Survived <- 1

# combine the test and train and will impute the mssing values
final <- rbind(train,test)
str(final)

#changing the datatypes
final$PassengerId <- as.factor(final$PassengerId)
final$Survived <- as.factor(final$Survived)
final$Pclass <- as.factor(final$Pclass)

#visualize the missing value
missmap(final)

#missing value imputation

# for age imputing it with mean
final$Age[is.na(final$Age)] <- mean(final$Age[!is.na(final$Age)])

# for fare we see that most used fare price is 8.05 , we will replace that it with NA
which(is.na(final$Fare))
sort(table(final$Fare[!is.na(final$Fare)]))
final$Fare[is.na(final$Fare)] <- 8.05

# 2 missing values for embarked 
# gives the index wer the na values is present
which(is.na(final$Embarked))

#gives the no of occurances for each level we see "s" have more so replace by s
table(final$Embarked[!is.na(final$Embarked)])

# replac 2 missing values by "s"
final$Embarked[is.na(final$Embarked)] <- 'S'

# cabin has lot of missing values will drop it
final <- final[-11]

#splitting the data

train1 <- final[1:891,]
test1 <- final[892:1309,]
sapply(test1, function(x) sum(is.na(x)))

test1$Survived <- NULL

# to check the accuracy of model we split the train set into subtrain and subtest because(actual testdata doesnt have info abt survived )

#data visualization
library(ggplot2)
ggplot(train1, aes(x = Sex, fill = Survived )) + facet_wrap(~Pclass) + geom_bar() + theme_bw()

#split
library(caTools)
set.seed(101) 
sample = sample.split(train1$Survived, SplitRatio = .80)
sub_train = subset(train1, sample == TRUE)
sub_test  = subset(train1, sample == FALSE)

# logistic regression model

model1 <- glm(Survived ~ Age+Sex+Embarked+Pclass+Fare  , data = sub_train, family = binomial)
summary(model1)

#emabrked and fare doesnt seeems to be significant let's remove
model2 <- glm(Survived ~ Age+Sex+Pclass  , data = sub_train, family = binomial)
summary(model2)

#prediction
prediction <- predict(model2, newdata = sub_test, type = "response")
head(prediction)

# probabbilit of 0.5
prediction = ifelse(prediction > 0.5,1,0)
head(prediction)

#confusion matrix

cm <- table(Actual = sub_test$Survived, Predicted = prediction)
cm

#accuracy = 78.6
print(sum(diag(cm))/sum(cm))

#model with full train data

model <- glm(Survived ~ Age+Sex+Pclass, data = train1, family = "binomial")
summary(model)

prediction <- predict(model, newdata = test1, type = "response")
prediction <- ifelse(prediction > 0.5,1,0)  
test1$Survived <- prediction









