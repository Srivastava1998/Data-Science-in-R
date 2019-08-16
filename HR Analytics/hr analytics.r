train<- read.csv("train2.csv",stringsAsFactors = F)
test<- read.csv("test2.csv", stringsAsFactors = F)
require(dplyr)
full <- bind_rows(train, test)
full<- mutate_if(full,is.character, tolower)
str(full)
summary(full)
table(full$is_promoted)
colSums(is.na(full))
summary(full$previous_year_rating)
full$previous_year_rating<- as.factor(full$previous_year_rating)

full$previous_year_rating<- ifelse(is.na(full$previous_year_rating), "0" ,full$previous_year_rating)
colSums(is.na(full))
colSums(full == "" |full== " ")
# df[which(!duplicated(df))]

df<- filter(full, education == ""| education== " ")
table(full$education)
full$education<- ifelse(full$education==""|full$education==" ","other",full$education)
summary(full)
str(full)
n_distinct(full$region)
n_distinct(full$department)


## Univariate analysis
n_distinct(full$employee_id)

# We see that all the employee identities are unique. So, there is no duplicacy in the
# dataset. Also, since it's a unique id, it cannot be used as a factor for prediction.

n_distinct(full$department)
table(full$department)
full$department<- as.factor(full$department)

n_distinct(full$region)
full$region<- as.factor(full$region)

n_distinct(full$education)
full$education<- as.factor(full$education)

n_distinct(full$gender)
table(full$gender)
full$gender<- as.factor(full$gender)

n_distinct(full$recruitment_channel)
table(full$recruitment_channel)
full$recruitment_channel<- as.factor(full$recruitment_channel)

n_distinct(full$no_of_trainings)
table(full$no_of_trainings)
summary(full$no_of_trainings)

n_distinct(full$age)
summary(full$age)
plot(quantile(full$age, seq(0,1,0.01)))
quantile(full$age, seq(0,1,0.01))
# No significant outlier found in the Ages of the people.

n_distinct(full$previous_year_rating)
summary(full$previous_year_rating)
full$previous_year_rating<- as.factor(full$previous_year_rating)

n_distinct(full$length_of_service)
summary(full$length_of_service)
plot(quantile(full$length_of_service, seq(0,1,0.01)))
quantile(full$length_of_service, seq(0,1,0.01))
# There are some outliers. We must cap them so that they do not alter the analysis.
full$length_of_service[full$length_of_service > 16] <- 18
summary(full$length_of_service)

table(full$KPIs_met..80.)
full$KPIs_met..80.<- as.factor(full$KPIs_met..80.)

table(full$awards_won.)
full$awards_won.<- as.factor(full$awards_won.)

summary(full$avg_training_score)
plot(quantile(full$avg_training_score, seq(0,1,0.01)))
quantile(full$avg_training_score, seq(0,1,0.01))
# We see that there are some outliers. In order to prevent some alteration in the 
# analysis, we must cap the outliers.

full$avg_training_score[full$length_of_service > 91]<- 93

table(full$is_promoted)
summary(full$is_promoted)

summary(full)

# Lets apply logistic regression to our model.

require(dummies)
ful_dum<- dummy.data.frame(full[,-3])
ful_dum$is_promoted<-as.factor(ful_dum$is_promoted) 
tr_dum<- ful_dum[1:54808,]
ts_dum<- ful_dum[54809:78298, ]

require(caTools)
set.seed(100)
i<- sample.split(tr_dum$is_promoted, SplitRatio = 0.8)
trn<- tr_dum[i,]
val<- tr_dum[!i,]

# Building the model
# model<- glm(is_promoted~.,data= trn[,-1], family = 'binomial')
# summary(model)
# #17359
# 
# require(MASS)
# 
# model_1<- stepAIC(model, direction = 'both')
# summary(model_1)
# #17307
# sort(vif(model_1), decreasing = TRUE)
# 
# model_2<- glm(formula = is_promoted ~ departmentanalytics + departmentfinance + 
#                 departmenthr + departmentlegal + departmentoperations + departmentprocurement + 
#                 `departmentr&d` + `educationbachelor's` + 
#                 `educationmaster's & above` + no_of_trainings + age + previous_year_rating0 + 
#                 previous_year_rating1 + previous_year_rating2 + previous_year_rating3 + 
#                 previous_year_rating4 + length_of_service + KPIs_met..80.0 + 
#                 awards_won.0, family = "binomial", data = trn[, 
#                                                                                    -1])
# summary(model_2)
# #22116
# model_3<- glm(formula = is_promoted ~ departmentanalytics + departmentfinance + 
#                 departmenthr + departmentlegal + departmentoperations + 
#                 `departmentr&d` + `educationbachelor's` + `educationmaster's & above` + 
#                 no_of_trainings + age + previous_year_rating0 + previous_year_rating1 + 
#                 previous_year_rating2 + previous_year_rating3 + previous_year_rating4 + 
#                 length_of_service + KPIs_met..80.0 + awards_won.0, family = "binomial", 
#               data = trn[, -1])
# summary(model_3)
# #22115
# model_4<- glm(formula = is_promoted ~ departmentanalytics + departmentfinance + 
#                 departmenthr + departmentlegal + departmentoperations  + 
#                 `departmentr&d` + `educationbachelor's` + `educationmaster's & above` + 
#                 no_of_trainings + age + previous_year_rating0 + previous_year_rating1 + 
#                 previous_year_rating2 + previous_year_rating3 + previous_year_rating4 + 
#                 length_of_service + KPIs_met..80.0 + awards_won.0, family = "binomial", 
#               data = trn[, -1])
# summary(model_4)
# #22115
# 
# model_5<- glm(formula = is_promoted ~ departmentfinance + 
#       departmenthr + departmentlegal + departmentoperations + `departmentr&d` + 
#       `educationbachelor's` + `educationmaster's & above` + no_of_trainings + 
#       age + previous_year_rating0 + previous_year_rating1 + previous_year_rating2 + 
#       previous_year_rating3 + previous_year_rating4 + length_of_service + 
#       KPIs_met..80.0 + awards_won.0, family = "binomial", data = trn[, 
#                                                                      -1])
# summary(model_5)
# #22117
# predicted_probability<- predict(model_5,val, type= "response")
# summary(predicted_probability)
# pred_promoted <- ifelse(predicted_probability > 0.10, "1", "0")
# actual_promoted<- as.factor(val$is_promoted)
# pred_promoted<- as.factor(pred_promoted)
# summary(pred_promoted)
# summary(actual_promoted)
# 
# require(caret)
# confusionMatrix(pred_promoted,actual_promoted, positive = "1")



require(rpart)
require(rpart.plot)
full$is_promoted <- as.factor(full$is_promoted)

tr.data <- full[1:54808, ]
ts.data <- full[54809:78298, ]


trn <- tr.data[i, ]
val <- tr.data[!i, ]

tree.model <- rpart(is_promoted ~ .,                     # formula
                    data = trn[, -c(1,3)])                   # training data
prp(tree.model)

pred <- predict(tree.model, newdata = val, type = 'class')

confusionMatrix(pred, val$is_promoted, positive = "1")

summary(pred)
summary(val$is_promoted)


tree.model <- rpart(is_promoted ~ .,                     # formula
                    data = trn[, -c(1,3)],
                    parms=list(split="information", loss=matrix(c(0,1,6.3,0), byrow=TRUE, nrow=2)))  
summary(tree.model)
prp(tree.model)

pred <- predict(tree.model, newdata = val, type = 'class')
confusionMatrix(pred, val$is_promoted, positive = "1")

tree.model1 <- rpart(is_promoted ~ .,                     # formula
                    data = trn[, -c(1,3)],
                    parms=list(split="information", loss=matrix(c(0,1,6.4,0), byrow=TRUE, nrow=2)))


summary(tree.model1)
prp(tree.model1)

pred <- predict(tree.model1, newdata = val, type = 'class')
confusionMatrix(pred, val$is_promoted, positive = "1")



require(randomForest)

rf_model <- randomForest(is_promoted ~ ., 
                         data = trn[, -c(1,3)], 
                         ntree = 1000,
                         proximity = F, 
                         do.trace = 100, 
                         importance = T, cutoff = c(0.92,0.08))


pred_sur <- predict(rf_model, val[,-14])

confusionMatrix(pred_sur, val$is_promoted, positive = "1")

# senstivity= 0.76
# specificity= 0.80
# accuracy= 0.801

# This is a relatively better model than the other models as it is giving a good combination
# of accuracy, senstivity and specificity.












