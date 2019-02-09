library("ggplot2", lib.loc="~/R/win-library/3.4")
library("dplyr", lib.loc="~/R/win-library/3.4")
library("C50", lib.loc="~/R/win-library/3.4")
View(test)
#Fetching the data 
train<-read.csv("C:/Users/hp/Downloads/train.csv")
test<-read.csv("C:/Users/hp/Downloads/test.csv")
View(train)
View(test)
summary(train)
summary(test)
str(train)
str(test)
median(train$Age, na.rm= TRUE)
#checking if there are missing values in variables.
sum(is.na(train$SibSp))
sum(is.na(train$Parch))
sum(is.na(train$Embarked))
sum(is.na(train$Sex))
sum(is.na(train$Pclass))
sum(is.na(train$Age))
# replacing the missing values of Age with the median of the age
train$Age<- ifelse(is.na(train$Age),28,train$Age)
brain<- data.frame(Pclass= train$Pclass, Sex= train$Sex, Age= train$Age, Sibsp= train$SibSp,
                   Parch= train$Parch, Embarked= train$Embarked, Survived= train$Survived)
titanic_model<- C5.0(x= brain[-7], y= brain$Survived)
brain$Survived<- as.factor(brain$Survived)
titanic_model<- C5.0(x= brain[-7], y= brain$Survived)
titanic_model
# The tree size comes out to be zero. This means that there was something wrong with the 
# above code.
# So, I removed some of the variables which are insignificant according to me.
qwerty<- data.frame(Pclass= brain$Pclass, Sex= brain$Sex, Age= brain$Age, Survived= brain$Survived)
View(qwerty)
#training the model on the training data
titanic_model<-C5.0(x= qwerty[-4], y= qwerty$Survived)
titanic_model
summary(titanic_model)
best<- data.frame(Pclass= test$Pclass,Sex= test$Sex, Age= test$Age)
View(best)
sum(is.na(best$Pclass))
sum(is.na(best$Sex))
sum(is.na(best$Age))
median(test$Age, na.rm= TRUE)
best$Age<- ifelse(is.na(best$Age),27,best$Age)
View(best)
#predicting the model on the test dataset
prediction1<- predict(titanic_model, best)
prediction1
# Formation of dataframe containing the PassengerId and the Survived column.
zxc<- data.frame(PassengerId= test$PassengerId, Survived= prediction1)
View(zxc)
# writing the dataframe in the csv format.
write.csv(zxc, file="titanic5.csv", row.names = FALSE)
#In this model, I got an accuracy of 76.076%.





