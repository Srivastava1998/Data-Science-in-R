library("ggplot2", lib.loc="~/R/win-library/3.4")
library("dplyr", lib.loc="~/R/win-library/3.4")
library("C50", lib.loc="~/R/win-library/3.4")
#Fetching the data 
train<-read.csv("C:/Users/hp/Downloads/train.csv")
test<-read.csv("C:/Users/hp/Downloads/test.csv")
View(train)
View(test)
#training the model on the training data
model1<-glm(Survived ~ Pclass + Sex + Age, data = train, family= "binomial")
summary(model1)
summary(train$Age)
summary(test$Age)
median(train$Age)
median(test$Age)
# replacing the missing values of Age with the median of the age
train$Age<-ifelse(is.na(train$Age),27,train$Age)
test$Age<- ifelse(is.na(test$Age),27,test$Age)
#checking if there are missing values in the other variables.
sum(is.na(train$Parch))
sum(is.na(train$Fare))
sum(is.na(train$Embarked))
# training the model after removing the missing values
model1<- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare, data = train, family = "binomial")
summary(model1)
# predicting the model using the predict function
t<- predict(model1, test, type= "response")
t
summary(t)
# the values which are greater than 0.5 will be rounded off to 1, otherwise, the value is
# rounded off to 0.
u<-ifelse(t>0.5,1,0)
summary(u)
# There was one missing value. It is replaced with 0.
u<-ifelse(is.na(u),0,u)
summary(u)
# Formation of dataframe containing the PassengerId and the Survived column.
asd<- data.frame(PassengerId= test$PassengerId, Survived= u)
str(asd)
# writing the dataframe in the csv format.
write.csv(asd, file= "Sachit234.csv", row.names = FALSE)
#In this model, I got an accuracy of 73.684%. In the above model, we see that the p- value of 
# Parch and Fare is pretty high compared to the 5% significance level. Therefore, these 2 
#variables can be considered as insignificant. So, we can use another linear model in which
# these two variables can be dropped.
# training the model on the training dataset
model2<-glm(Survived~ Pclass + Sex + Age + SibSp, data = train, family = "binomial")
summary(model2)
#predicting the model on the test dataset
w<- predict(model2, test, type= "response")
w
summary(w)
# the values which are greater than 0.5 will be rounded off to 1, otherwise, the value is
# rounded off to 0.
e<- ifelse(w>0.5,1,0)
e
summary(e)
# Formation of dataframe containing the PassengerId and the Survived column.
jkl<- data.frame(PassengerId= test$PassengerId, Survived =e)
# writing the dataframe in the csv format.
write.csv(jkl, file = "titanic.csv", row.names = FALSE)
#In this model, I got an accuracy of 74.162%. In the above model, we see that the variables we 
#have chosen are significant according to the 5% significance level.
