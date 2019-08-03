
####################################################################
####################################################################
####################################################################


####################################################################

rm(list = ls())
dev.off()

require(titanic)

# Imorting the datasets
train <- titanic_train
test <- titanic_test

require(dplyr)
full <- bind_rows(train, test)

# Let's see the structure of the dataset
str(full)
summary(full)

# Case matching
full <- mutate_if(full, is.character, toupper)

# Let's see the number of distinct values in each row of the dataset 
sapply(full, n_distinct)

# Check duplicates
sum(duplicated(full[,-1]))
colSums(is.na(full))

# NA's
# 418 in Survived
# 263 in Age
# 1 in Fare

# Cheking NA of 'Fare'.
which(is.na(full$Fare))

# Let's see the observation in which there is a NA value in Fare
full[1044,]

# It has 'Pclass' = 3 and 'Embarked' = S
# Let's see if they have impact on price.
summary(full$Fare[which(full$Embarked == 'S' & 
                          full$Pclass == 3)])

# We see '0' Fare which seems ambiguous.
# Let's see in detail.
View(subset(full, Fare == 0))

# We see that for all Fare = 0 : 
#   1. 'Embarked' at Port 'S'; 
#   2. Are all male; 
#   3. Have SibSp and Parch '0' i.e. no family travelling with them.
#   4. Have only one person survived i.e. very low survival.

# It seems to be the working crew of the ship for many reasons

# Replacing NA with median.
full$Fare[1044] <- 8.050

sapply(full, function(x) length(which(x == "")))

# Blanks
# 1014 in Cabin
# Let's check the percentage of blank values
1014 / nrow(full) * 100

# There are a lot of missing values. It's better if we drop it.
full$Cabin <- NULL


# 2 Blanks in Embarked.
table(full$Embarked)

full[which(full$Embarked == ""),]

# We see that the two observations we got have the same fare, Pclass.
require(ggplot2)
ggplot(full, aes(x = Embarked, y = Fare)) + 
  facet_grid(. ~ Pclass) + geom_boxplot()

# Now, let's see if fare= 80 gives some meaningful insight.
ggplot(full, aes(x = Embarked, y = Fare)) + 
  facet_grid(. ~ Pclass) + geom_boxplot() + 
  geom_hline(aes(yintercept = 80), col = "red")
# The fare= 80 is passing from the median of passengers travelling in Pclass 1 and
# embarked the ship from C.
# So we should replace it by 'C'
full$Embarked[which(full$Embarked == "")] <- "C"

full$Embarked <- as.factor(full$Embarked)

sapply(full, function(x) length(which(x == " ")))


#### EDA

# Univariate Analysis
titanic_plot <- ggplot(full[1:891,], 
                aes(fill = factor(Survived)))

# Survived
titanic_plot + geom_bar(aes(x = Survived))

table(full$Survived)

# Pclass
full$Pclass <- as.factor(full$Pclass)
titanic_plot + geom_bar(aes(x = Pclass))
titanic_plot + geom_bar(aes(x = Pclass), position = 'fill')
# We can see that most percentage of the people survived in Pclass 1 and the least
# survived in Pclass 3. It may be that Passenger class 1 must have been given the 
# preferences or they may be closer to the life boats.



# Name
length(unique(full$Name))
# Since names cannot be used for analysis, we can drop the Name column from the dataset.
full$Name <- NULL

#### Visualization ----
titanic_plot <- ggplot(full[1:891,], aes(fill = factor(Survived)))

# Sex
full$Sex <- as.factor(full$Sex)
titanic_plot + geom_bar(aes(x = Sex))
titanic_plot + geom_bar(aes(x = Sex), position = 'fill')

# We can see from the plots that females were given preferences over males.



# Age
titanic_plot + geom_histogram(aes(x = Age), 
               binwidth = 10, col = 'black')

titanic_plot + geom_histogram(aes(x = Age),
               binwidth = 10, col = 'black', 
               position = 'fill')

summary(full$Age)
# There are a lot of NA values in Age

# Checking outliers
plot(quantile(full$Age, seq(0,1,0.01), na.rm = T))
quantile(full$Age, seq(0,1,0.01), na.rm = T)

# At 100th percentile. Cap it.
full$Age[full$Age > 65] <- 67
summary(full$Age)

# NA imputation
require(randomForest)
require(mice)

mice_df <- full[,!colnames(full) %in% 
           c('PassengerId','Survived','Ticket')]

set.seed(1)
mice_model <- mice(mice_df, method = 'rf')

mice_data <- complete(mice_model)


# Check imputation 
par(mfrow = c(1,2))

hist(full$Age, col = 'red', main = 'Original Age')
hist(mice_data$Age, col = 'blue', main = 'Imputed Age')

# Seems good to impute.
par(mfrow = c(1,1))

full$Age <- mice_data$Age


# SibSp and Parch

titanic_plot + geom_histogram(aes(SibSp), binwidth = 1, col = 'black')
titanic_plot + geom_histogram(aes(SibSp), binwidth = 1, col = 'black', position = 'fill')


titanic_plot + geom_histogram(aes(Parch), binwidth = 1, col = 'black')
titanic_plot + geom_histogram(aes(Parch), binwidth = 1, col = 'black', position = 'fill')

# Both are not giving any clear info.
# Lets create a derived metric.
full$relatives <- full$SibSp + full$Parch

titanic_plot <- ggplot(full[1:891,], aes(fill = factor(Survived)))

titanic_plot + geom_histogram(aes(relatives), binwidth = 1, col = 'black')
titanic_plot + geom_histogram(aes(relatives), binwidth = 1, col = 'black', position = 'fill')



# Ticket
length(unique(full$Ticket))
# Too many 
# So, we will remove the variable.

full$Ticket <- NULL

# Fare
titanic_plot + geom_histogram(aes(Fare), bins = 10, col = 'black')
titanic_plot + geom_histogram(aes(Fare), bins = 10, col = 'black', position = 'fill')

# Check outliers
quantile(full$Fare, seq(0,1,0.01))

# We will cap the outliers
full$Fare[full$Fare > 164.866700] <- 180

# Embarked
titanic_plot <- ggplot(full[1:891,], aes(fill = factor(Survived)))
titanic_plot + geom_bar(aes(Embarked))
titanic_plot + geom_bar(aes(Embarked), position = 'fill')

# No meaningful insight can be drawn out from the plots.
str(full)

#### EDA complete


#### Logistic Regression

# Creating dummy df
require(dummies)
ful_dum <- dummy.data.frame(full)

# Split the data back into a train set and a test set
tr_dum <- ful_dum[1:891,]
ts_dum <- ful_dum[892:1309,]

# Create training and validation datasets from train dataset (which Has target variable column)
require(caTools)
set.seed(123)

i <- sample.split(tr_dum$Survived, SplitRatio = 0.8)

trn <- tr_dum[i,]
val <- tr_dum[!i,]

# Build the model 
model_1 <- glm(Survived ~ ., data = trn[,-1], family = 'binomial')
summary(model_1)

# Step reduction as per AIC
require(MASS)
model_2 <- stepAIC(model_1, direction = 'both')
# Check multi-collinearity
require(car)
sort(vif(model_2), decreasing = T)

# All are in limit.

# Lets check p-values
summary(model_2)


# Removing EmbarkedQ
model_3 <- glm(formula = Survived ~ Pclass1 + Pclass2 + SexFEMALE + Age + 
                 SibSp + EmbarkedC , family = "binomial", data = trn[, 
                                                                                -1])


summary(model_3)

# Remove EmbarkedC
model_4 <- glm(formula = Survived ~ Pclass1 + Pclass2 + SexFEMALE + Age + 
                 SibSp, family = "binomial", data = trn[,-1]) 
summary(model_4)


# Now all variables are significant as per p-value
log_model <- model_4


# Prediction on validation set
predicted_probability <- predict(log_model,newdata = val, type = "response")

summary(predicted_probability)

# Setting cutoff value of predicted probability
pred_survived <- ifelse(predicted_probability > 0.5, "1", "0")

# Lets see good our prediction is.
actual_survived <- as.factor(val$Survived)
pred_survived<- as.factor(pred_survived)

require(caret)

con_mat <- confusionMatrix(pred_survived, actual_survived, positive = "1")

con_mat




# Now submit prediction on kaggle
test_pred <- predict(log_model, newdata = ts_dum, type = 'response')

test_pred <- ifelse(test_pred > 0.5, "1", "0")

ts_dum$Survived <- test_pred


submission <- ts_dum[, c(1,2)]


write.csv(submission, "titanic.csv", row.names = FALSE)











#### SVM

require(e1071)

# covert target variable as category
tr_dum$Survived <- as.factor(tr_dum$Survived)

trn <- tr_dum[i, ]
val <- tr_dum[!i, ]

# linear kernel
svm_lnr <- svm(Survived ~ ., data = trn[,-1], 
               kernel = 'linear')

pred_sur <- predict(svm_lnr, val[,-c(1,2)])

confusionMatrix(pred_sur, val$Survived, positive = '1')


# polynomial kernel
svm_pol <- svm(Survived ~ ., data = trn[,-1], 
               kernel = 'polynomial')

pred_sur <- predict(svm_pol, val[,-c(1,2)])

confusionMatrix(pred_sur, val$Survived, positive = '1')


# radial kernel
svm_rad <- svm(Survived ~ ., data = trn[,-1], 
               kernel = 'radial')

pred_sur <- predict(svm_rad, val[,-c(1,2)])

confusionMatrix(pred_sur, val$Survived, positive = '1')

# We see that linear kernel gives best prediction
# Submit prediction with linear svm

ts_dum$Survived <- predict(svm_lnr, newdata = ts_dum)

table(ts_dum$Survived)

submission <- ts_dum[, c(1,2)]

write.csv(submission, "sub1.csv", row.names = T)


#### Decision Tree

require(rpart)
require(rpart.plot)

# Lets take data without dummy variables
full$Survived <- as.factor(full$Survived)

tr.data <- full[1:891, ]
ts.data <- full[892:1309, ]

trn <- tr.data[i, ]
val <- tr.data[!i, ]

tree.model <- rpart(Survived ~ .,                     # formula
                    data = trn[, -1])                   # training data

# display decision tree
prp(tree.model)

pred <- predict(tree.model, newdata = val, type = 'class')

confusionMatrix(pred, val$Survived, positive = "1")
# This is split on GINI index basis

# Try with information gain as basis of split.
tree.model <- rpart(Survived ~ ., data = trn[, -1], parms = list(split = "information")) 

prp(tree.model)

pred <- predict(tree.model, newdata = val, type = 'class')

confusionMatrix(pred, val$Survived, positive = "1")
# We see similar result

# Try with pruned tree
tree.model <- rpart(Survived ~ ., data = trn[, -1], control = rpart.control(minbucket = 100)) 

prp(tree.model)

pred <- predict(tree.model, newdata = val, type = 'class')

confusionMatrix(pred, val$Survived, positive = "1")
# We see better combination of accuracy, sensitivity and specificity
# by pruned tree on unseen data.

# We can submit prediction with pruned tree.


#### RF


# Building the model 
require(randomForest)

rf_model <- randomForest(Survived ~ ., 
                         data = trn[, -1], 
                         ntree = 1000,
                         proximity = F, 
                         do.trace = 100, 
                         importance = T)


pred_sur <- predict(rf_model, val[,-2])

confusionMatrix(pred_sur, val$Survived, positive = "1")
# Result is best so far by Random Forest model.

# But accuracy, sensitivity and specificity are varying.

# Let's improve it by predicting probabilities rather than labels
# ( just like logistic regression)
predicted_probability <- predict(rf_model, val[,-2], type = "prob")

View(predicted_probability)
# now rather than "0" and "1",, We see probabilities of "0" and "1" 

# Our interest is with probabilities of "1".
summary(predicted_probability[,2])

# Set cutoff ... Hit and trial.
cutoff = 0.38

pred_survived <- factor(ifelse(predicted_probability[,2] > cutoff, "1", "0"))

confusionMatrix(pred_survived, val$Survived, positive = '1')
# We get a more stable result at cutoff 0.38 rather than 0.5

pred <- predict(tree.model, newdata = ts.data, type = 'class')
test$Survived<- pred
View(test)
df<- test[,c(1,13)]
df1<-write.csv(df,file = "titanic_model.csv", row.names = FALSE)
