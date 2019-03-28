library(MASS)
summary(insurance)
str(insurance)
cor(insurance$age,insurance$bmi)
cor(insurance$age,insurance$children)
cor(insurance$age,insurance$charges)
cor(insurance$bmi,insurance$charges)
cor(insurance$children,insurance$charges)
# We can use correlation matrix for finding correlation of each variable with each and every
# variable.

library(caTools)
set.seed(12345)
sample = sample.split(insurance,SplitRatio = 0.8) 
train9<- subset(insurance, sample== TRUE)
test9<- subset(insurance, sample== FALSE)
ins_model<- lm(charges~., data= train9)
summary(ins_model)
ins_step<-stepAIC(ins_model, direction = "both", trace = TRUE)
summary(ins_step)
predict(ins_step,test9)
predict(ins_step, train9)
test9$charges2<- predict(ins_step, test9)
library(Metrics)
rmsle(test9$charges2, test9$charges)
summary(test9$charges2)
test9$charges2<- abs(test9$charges2)
rmsle(test9$charges2, test9$charges)
# We get the final model by using StepAIC function in the MASS package.It uses Akaike
# information criteria. It provides means for model selection.
# In the final model of the multiple regression model, we see that all the variables in the
# dataset are significant according to 5% significance level. The multiple R squared value
# is 0.749. It tells the percentage of variablilty in the response variable explained by the
# explanatory variable. The Adjusted R squared value is 0.748. It increases as the added or
# removed predictors increase the model accuracy. It depends on the adding or removal of 
# predictors. the Adjusted R squared value corrects R-squared by penalizing models with a large
# number of independent variables. It is useful for comparing the performance of models with
# different numbers of explanatory variables.

# The RMSLE value tells the root mean squared logarathmic error value of the actual value from
# the predicted value.




