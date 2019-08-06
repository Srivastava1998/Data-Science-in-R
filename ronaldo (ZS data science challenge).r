#data <- read.csv("C:/Users/hp/Desktop/ZS Associates/data.csv", stringsAsFactors=FALSE)
data<- read.csv("data.csv", check.names = T, stringsAsFactors = FALSE)

# Checking the structure and the summary of the dataset
str(data)
summary(data)
colnames(data)

# Checking the head and tail of the data
head(data)
tail(data)

# Changing all the column names to same case so that mismatch does not occur in the analysis
require(dplyr)
ronaldo <- mutate_if(data, is.character, toupper)

# Let's see the number of distinct values in each column of the dataset 
sapply(ronaldo, n_distinct)


# After looking at the distinct values of the dataset, we get some idea. The variable names "X"
# which was initially blank in the csv file, is a unique id for every observation.

# Match event id must be unique id for every match in the dataset.

sum(duplicated(ronaldo[,-1]))
# We see that there are no duplicate observation in the dataset.

ronaldo[ronaldo == ""]<- NA
colSums(ronaldo== "", na.rm=T)

colSums(is.na(ronaldo))
sapply(ronaldo, n_distinct)

# We see that there are some missing values in the dataset. So, we will treat the missing 
# values so that it does not alter the analysis.
a<-ronaldo[which(is.na(ronaldo$match_event_id)),]
1563/30697
# since, they some missing values in the match_event_id which comprise 5% of the total,
# we can drop this variable as match_event_id won't be used in the analysis
 a<-ronaldo[,c("remaining_min","remaining_min.1")]

ronaldo$remaining_min<- ifelse(is.na(ronaldo$remaining_min), ronaldo$remaining_min.1,
                               ronaldo$remaining_min)

ronaldo$remaining_min.1<- NULL
summary(ronaldo$remaining_min)

plot(quantile(ronaldo$remaining_min, seq(0,1,0.01), na.rm = T))
quantile(ronaldo$remaining_min, seq(0,1,0.01), na.rm=T)


# we see there are some of outliers in the dataset.
# so, we will cap them.

ronaldo$remaining_min[ronaldo$remaining_min > 11] <- 12
summary(ronaldo$remaining_min)

# since the missing values have reduced considerably and the ouliers have been removed, we
# can replace the remaining missing values by median

ronaldo$remaining_min<- ifelse(is.na(ronaldo$remaining_min), 5,
                               ronaldo$remaining_min)
summary(ronaldo$remaining_min)

a<-ronaldo[,c("remaining_sec","remaining_sec.1")]

ronaldo$remaining_sec <- ifelse(is.na(ronaldo$remaining_sec), ronaldo$remaining_sec.1,
                                ronaldo$remaining_sec)
summary(ronaldo$remaining_sec)
ronaldo$remaining_sec.1<- NULL

plot(quantile(ronaldo$remaining_sec, seq(0,1,0.01), na.rm = T))
quantile(ronaldo$remaining_sec, seq(0,1,0.01), na.rm=T)

# We will cap the outliers.
ronaldo$remaining_sec[ronaldo$remaining_sec > 59] <- 59
summary(ronaldo$remaining_sec)

# Since, most of the outliers are removed, we will replace it by median.
ronaldo$remaining_sec<- ifelse(is.na(ronaldo$remaining_sec), 29,
                               ronaldo$remaining_sec)
summary(ronaldo$remaining_sec)

a<-ronaldo[,c("distance_of_shot","distance_of_shot.1")]

summary(ronaldo$distance_of_shot)
summary(ronaldo$distance_of_shot.1)
var(ronaldo$distance_of_shot,na.rm= T)
var(ronaldo$distance_of_shot.1, na.rm= T)

ronaldo$distance_of_shot<- ifelse(is.na(ronaldo$distance_of_shot), ronaldo$distance_of_shot.1,
                                  ronaldo$distance_of_shot)
summary(ronaldo$distance_of_shot)

plot(quantile(ronaldo$distance_of_shot, seq(0,1,0.01), na.rm = T))
quantile(ronaldo$distance_of_shot, seq(0,1,0.01), na.rm=T)

ronaldo$distance_of_shot[ronaldo$distance_of_shot > 55] <- 60
summary(ronaldo$distance_of_shot)

ronaldo$distance_of_shot<- ifelse(is.na(ronaldo$distance_of_shot), 35,
                                  ronaldo$distance_of_shot)

summary(ronaldo$distance_of_shot)

ronaldo$distance_of_shot.1 <- NULL

a<-ronaldo[,c("power_of_shot","power_of_shot.1")]

summary(ronaldo$power_of_shot)
summary(ronaldo$power_of_shot.1)
# We see that power_of_shot.1 has some ambigious values. So, we will replace the missing
# values of power_of_shot with power of shot.1.
ronaldo$power_of_shot<- ifelse(is.na(ronaldo$power_of_shot), ronaldo$power_of_shot.1,
                               ronaldo$power_of_shot)

summary(ronaldo$power_of_shot)

plot(quantile(ronaldo$power_of_shot, seq(0,1,0.01), na.rm = T))
quantile(ronaldo$power_of_shot, seq(0,1,0.01), na.rm = T)

ronaldo$power_of_shot[ronaldo$power_of_shot > 7] <- 8
ronaldo$power_of_shot<- ifelse(is.na(ronaldo$power_of_shot), 3,
                               ronaldo$power_of_shot)
summary(ronaldo$power_of_shot)
table(ronaldo$power_of_shot)

require(ggplot2)
ggplot(ronaldo[!is.na(ronaldo$is_goal), ], aes(x = power_of_shot, fill = factor(is_goal))) +
  geom_bar()

ggplot(ronaldo[!is.na(ronaldo$is_goal), ], aes(x = power_of_shot, fill = factor(is_goal))) +
  geom_bar(position = 'fill')

ronaldo$power_of_shot.1<- NULL

a<-ronaldo[,c("knockout_match","knockout_match.1")]
table(ronaldo$knockout_match)
table(ronaldo$knockout_match.1)

summary(ronaldo$knockout_match)
summary(ronaldo$knockout_match.1)

# The variables give the value of 0 and 1 in which "0" is one category and "1" is another.
# So, we will replace the missing values of "knockout_match" with the missing values of
# "knockout_match.1" and then we will proceed with the treatmnent.
ronaldo$knockout_match <- ifelse(is.na(ronaldo$knockout_match) & 
                                   between(ronaldo$knockout_match.1,0,1),
                                 ronaldo$knockout_match.1,
                                 ronaldo$knockout_match)

summary(ronaldo$knockout_match)

ronaldo$knockout_match <- ifelse(is.na(ronaldo$knockout_match), "MISSING", ronaldo$knockout_match)
summary(factor( ronaldo$knockout_match))

ggplot(ronaldo[!is.na(ronaldo$is_goal),], aes(knockout_match, fill = factor(is_goal))) + geom_bar()
ggplot(ronaldo[!is.na(ronaldo$is_goal),], aes(knockout_match, fill = factor(is_goal))) + geom_bar(position = 'fill')

table(ronaldo$knockout_match)
ronaldo$knockout_match<-as.factor(ronaldo$knockout_match)

summary(ronaldo$knockout_match)

ronaldo$knockout_match.1<- NULL


# feature engineering. We can find the total time by combining minutes and seconds.

summary(ronaldo$remaining_sec)

ronaldo$time_remaining <- ronaldo$remaining_min + (ronaldo$remaining_sec/60)

ronaldo$remaining_min<- NULL
ronaldo$remaining_sec<- NULL

summary(ronaldo$time_remaining)

colSums(is.na(ronaldo))

# Now, let's see the variable of game season.
class(ronaldo$game_season)
table(ronaldo$game_season)
sum(is.na(ronaldo$game_season))
n_distinct(ronaldo$game_season)
# As there are 21 categories in the game season, to avoid overfitting, we will drop
# the variable.
ronaldo$game_season<- NULL

# Let's see about the area of shot.
summary(ronaldo$area_of_shot)
table(ronaldo$area_of_shot)
sum(is.na(ronaldo$area_of_shot))


library(stringr)

ggplot(ronaldo[!is.na(ronaldo$is_goal), ], aes(area_of_shot, fill =factor(is_goal)))+geom_bar()
ggplot(ronaldo[!is.na(ronaldo$is_goal), ], aes(area_of_shot, fill =factor(is_goal)))+geom_bar(position = 'fill')
area <- str_split(ronaldo$area_of_shot, pattern = " ", simplify = T)

ronaldo$area_shot <- area[,1]
table(ronaldo$area_shot)
# We can say that there are now four categories in the variables.
summary(ronaldo$area_shot)



ronaldo$area_shot<- ifelse(is.na(ronaldo$area_shot), "MISSING", ronaldo$area_shot)
summary(ronaldo$area_shot)
table(ronaldo$area_shot)
ronaldo$area_shot<- as.factor(ronaldo$area_shot)
summary(ronaldo$area_shot)

# Dropping area_of_shot variable
ronaldo$area_of_shot<- NULL

colSums(is.na(ronaldo))


# Let's look at the variables "type_of_shot" and "type_of_combined_shot"
a<-ronaldo[,c("type_of_shot","type_of_combined_shot")]
colSums(is.na(e))
table(ronaldo$type_of_shot)
table(ronaldo$type_of_combined_shot)

# "type_of_shot" variable has more than 50% of missing values and also a lot of categories.
# So, we will drop this variable for the analysis.

# "type_of_combined_shot" variable has less categories but it also has more than 50% of 
# missing values. The NA imputation will result in ambiguous result. Therefore, we will
# drop this variable as well.

ronaldo$type_of_shot<- NULL
ronaldo$type_of_combined_shot<- NULL

colSums(is.na(ronaldo))

# Now, we look at the "shot_id_number" and "shot_basics".

c<- ronaldo[,c("shot_id_number", "shot_basics")]
n_distinct(c$shot_id_number)
# We see that these are distict values of shots made by Ronaldo. There are # 29135 distinct
# values. So, we will drop the variable in the analysis. 
ronaldo$shot_id_number<- 1:30697
# Note: The submsissions have to be made by using shot_id_number. So, we won't drop the code
# from the dataset.

table(ronaldo$shot_basics)
sum(is.na(ronaldo$shot_basics))

#Same approach as "area_shot".

ggplot(ronaldo[!is.na(ronaldo$is_goal), ], aes(shot_basics, fill =factor(is_goal)))+geom_bar()
ggplot(ronaldo[!is.na(ronaldo$is_goal), ], aes(shot_basics, fill =factor(is_goal)))+geom_bar(position = 'fill')


ronaldo$shot_basics<- ifelse(is.na(ronaldo$shot_basics),"MISSING", ronaldo$shot_basics)
sum(is.na(ronaldo$shot_basics))
summary(ronaldo$shot_basics)
ronaldo$shot_basics<- as.factor(ronaldo$shot_basics)
summary(ronaldo$shot_basics)

colSums(is.na(ronaldo))

# Let's see the "range_of_shot" variable
summary(ronaldo$range_of_shot)
table(ronaldo$range_of_shot)

ronaldo$range_of_shot<- ifelse(is.na(ronaldo$range_of_shot),"MISSING", ronaldo$range_of_shot)
summary(ronaldo$range_of_shot)
table(ronaldo$range_of_shot)

ronaldo$range_of_shot<- as.factor(ronaldo$range_of_shot)
summary(ronaldo$range_of_shot)


colSums(is.na(ronaldo))
# Let's see the "team_name" column.
table(ronaldo$team_name)
sum(is.na(ronaldo$team_name))
# This variable has only one category with some missing values. Therefore, we will drop
# the variable.

ronaldo$team_name<- NULL

colSums(is.na(ronaldo))
# Let's look at "date_of_game" variable


summary(ronaldo$date_of_game)
ronaldo$date_of_game <- as.Date(ronaldo$date_of_game)
ronaldo$date_of_game <- format(ronaldo$date_of_game, "%Y")
ronaldo$date_of_game <- as.numeric(ronaldo$date_of_game)

n_distinct(ronaldo$date_of_game)

ggplot(ronaldo[!is.na(ronaldo$is_goal), ], aes(date_of_game, fill =factor(is_goal)))+geom_bar()
ggplot(ronaldo[!is.na(ronaldo$is_goal), ], aes(date_of_game, fill =factor(is_goal)))+geom_bar(position = 'fill')

ronaldo$date_of_game <- ifelse(ronaldo$date_of_game < 2000, "BEFORE_2000",
                               ifelse(ronaldo$date_of_game > 2010, "AFTER 2010", "2000-2010"))

ronaldo$date_of_game<- ifelse(is.na(ronaldo$date_of_game),"MISSING",ronaldo$date_of_game)


ronaldo$date_of_game<- as.factor(ronaldo$date_of_game)



colSums(is.na(ronaldo))

# let's look at the "match_event_id" variable

n_distinct(ronaldo$match_event_id)
# It has lot of distinct values and also id won't be used in further analysis. So, we drop the
# variable

ronaldo$match_event_id<- NULL
colSums(is.na(ronaldo))






# Let's see what can be done in location X and location Y.

summary(ronaldo$location_x)
summary(ronaldo$location_y)


ggplot(ronaldo[!is.na(ronaldo$is_goal), ],
       aes(location_x, fill =factor(is_goal))) +
  geom_histogram(bins = 10)

ggplot(ronaldo[!is.na(ronaldo$is_goal), ],
       aes(location_x, fill =factor(is_goal))) +
  geom_histogram(position = 'fill', bins = 10)


ggplot(ronaldo[!is.na(ronaldo$is_goal), ],
       aes(location_y, fill =factor(is_goal))) +
  geom_histogram(bins = 10)

ggplot(ronaldo[!is.na(ronaldo$is_goal), ],
       aes(location_y, fill =factor(is_goal))) +
  geom_histogram(position = 'fill', bins = 10)

ggplot(ronaldo[!is.na(ronaldo$is_goal), ], aes(date_of_game, fill =factor(is_goal)))+geom_bar(position = 'fill')


# We will do the NA imputation with the help of mice package in location X and location Y.



str(ronaldo)

# Let's look at the "home.away" column
n_distinct(ronaldo$home.away)
a<-ronaldo[,c("home.away","lat.lng")]
c<- ronaldo[,"lat.lng"]
sum(is.na(ronaldo$home.away))
c<- as.data.frame(c)

b<- filter(a, lat.lng== "42.982923, -71.446094")
sum(is.na(b))
# We see that in the "home.away" column, the teams are seperated with either "@" or "VS".
# After having a close look on "home.away" and "lat.lng" column, we see that the observations
# in which contains "VS" is actually a home game since the latitude and longitude points to
# only one value i.e "42.982923, -71.446094".

away <- str_split(ronaldo$home.away, pattern = " ", simplify = T)
away<- as.data.frame(away, stringsAsFactors = FALSE)
homeaway<- cbind(away,c)

sum(is.na(homeaway$V2))

missing<- filter(homeaway, V2== "")
missing$V2<- ifelse(missing$c == "42.982923, -71.446094", "VS.", "@")

sum(is.na(missing$V2))
homeaway$V2<-ifelse(homeaway$V2== "", missing$V2, homeaway$V2)
sum(is.na(homeaway$V2))

ronaldo$home.away<- homeaway$V2

colSums(is.na(ronaldo))

v<-filter(ronaldo, is.na(home.away))

v$home.away<- ifelse(v$lat.lng=="42.982923, -71.446094", "VS.","@")
sum(is.na(v$home.away))

ronaldo$home.away <- ifelse(is.na(ronaldo$home.away), v$home.away, ronaldo$home.away)
sum(is.na(ronaldo$home.away))

table(ronaldo$home.away)
# After treating this variable, we see that there is one missing value still left.

b<- filter(ronaldo,is.na(home.away))
# After checking the value, we see that it should be given "@" value.

ronaldo$home.away<- ifelse(is.na(ronaldo$home.away),"@",ronaldo$home.away)

sum(is.na(ronaldo$home.away))

colSums(is.na(ronaldo))
ronaldo$lat.lng<- NULL

str(ronaldo)

summary(ronaldo$match_id)
n_distinct(ronaldo$match_id)
# These are the match id which won't be used in the analysis. So, we can drop it.
ronaldo$match_id<- NULL
n_distinct(ronaldo$team_id)
ronaldo$team_id<- NULL

summary(ronaldo)
str(ronaldo)
table(ronaldo$home.away)
ronaldo$home.away<- as.factor(ronaldo$home.away)
str(ronaldo)

require(mice)
require(randomForest)
mice_df<-  ronaldo[,!colnames(ronaldo) %in% 
                     c('X','is_goal','shot_id_number')]
colSums(is.na(ronaldo))

set.seed(1)
mice_model <- mice(mice_df, method = 'rf')
mice_data <- complete(mice_model)
colSums(is.na(mice_data))

ronaldo$location_x<- mice_data$location_x
ronaldo$location_y<- mice_data$location_y
train <- ronaldo[!is.na(ronaldo$is_goal),]

test <- ronaldo[is.na(ronaldo$is_goal),]

colSums(is.na(train))
colSums(is.na(test))




require(randomForest)
require(caTools)
set.seed(123)

ronaldo$is_goal<-as.factor(ronaldo$is_goal)
ts.data<- filter(ronaldo, is.na(is_goal))
tr.data<- filter(ronaldo, !is.na(is_goal))
i <- sample.split(tr.data$is_goal, SplitRatio = 0.8)
trn <- tr.data[i, ]
val <- tr.data[!i, ]


rf_model <- randomForest(is_goal ~ .,
                         data = trn[, -c(1,12)],
                         ntree = 1000,
                         proximity = F,
                         do.trace = 100,
                         importance = T
                         )


pred_goal <- predict(rf_model, val[,-7], type = "prob")
pred_goal1<- predict(rf_model, ts.data[,-7], type= "prob")
# confusionMatrix(pred_goal, val$is_goal, positive = "1")
# table(pred_goal)
# table(val$is_goal)
pred_goal1<- as.data.frame(pred_goal1)
ts.data$is_goal <- pred_goal1$`1`
submission <- ts.data[, c(12,7)]
write.csv(submission, "sachit_srivastava_230698_prediction_1.csv", row.names = FALSE)
importance(rf_model)
varImpPlot(rf_model)

