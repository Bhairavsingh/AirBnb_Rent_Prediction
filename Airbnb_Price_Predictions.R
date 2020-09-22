

#Installing and recalling packages

install.packages("e1071")
install.packages("pls")
install.packages("lattice")
install.packages("mice")
install.packages("ggplot2")
install.packages("s20x")
install.packages("glmnet")
install.packages("corrplot")
install.packages("MASS")
library(MASS)
library(lattice)
library(mice)
library(ggplot2)
library(caret)
library(s20x)
library(glmnet)
library(pls)
library(e1071)
library(corrplot)
library(MASS)
library(Metrics)
library(caret)

#Reading Data
data<-read.csv("A:/Master_of_Science_Data/3rd_Semester_Fall_2018/Intelligent Data Analytics (DSA_5103)/Project/Data set/Original/train.csv")
head(data)
View(data)

#Listing out attributes names.
attributes <- attributes(data)

#Listing out attributes names and missing values.
for (i in c(1:29))
{
  data[, i][which(data[, i] == "")] <- NA
  print(paste(attributes$names[i], sum(is.na(data[, i]))))
}

newData[is.na(newData[,18]),18] <- mean(newData[,18],na.rm = T)
newData$host_response_rate <- as.numeric((sub("%","",newData$host_response_rate, fixed = TRUE)))
newData[is.na(newData[,12]),12] <- mean(newData[,12],na.rm = T)

airbnb<- na.omit(newData)

write.csv(newData, file = 'newData.csv')

as.vector(newData$accommodates)
boxcox(as.vector(newData$accommodates))

library(caTools)
splittingratio <- sample.split(airbnb, SplitRatio = 0.7)
trainData<- subset(airbnb, splittingratio==TRUE)
testData<- subset(airbnb, splittingratio==FALSE)

reglm <- lm(data = trainData, log_price~ .)
summary(reglm)
#Adjusted R-squared:  0.6769 
testinglm <- predict(reglm, trainData)
#Calculating RMSE value
rmse(trainData$log_price, testinglm)
# 0.3981701
AIC(reglm)
# 46451.08
BIC(reglm)
# 57362.66
regglm.mod <- glm(data = trainData, log_price~ property_type + room_type + accommodates + bathrooms + bed_type + cancellation_policy + cleaning_fee + city + host_has_profile_pic + host_identity_verified + host_identity_verified + host_response_rate + instant_bookable + latitude + longitude + neighbourhood + number_of_reviews + review_scores_rating + zipcode + bedrooms + beds)
?poly()
cv.error = rep(0, 5)
for (i in 1:5)
{
  regglm.mod <- glm(data = trainData, log_price~ property_type + room_type + accommodates + bathrooms + bed_type + cancellation_policy + cleaning_fee + city + host_has_profile_pic + host_identity_verified + host_identity_verified + host_response_rate + instant_bookable + latitude + longitude + neighbourhood + number_of_reviews + review_scores_rating + zipcode + poly(bedrooms, 5) + beds)
  cv.error[i] = cv.glm(data = trainData, regglm.mod, k = 10)$delta[1]
}

cv.error



#Performing Ridge regression.
#Calculating lambda sequence
l <- 10^seq(0.00001, 1, length = 10)
#set.seed(2707)
regridge <- train(data = trainData, log_price ~ ., method = "glmnet", trControl = trainControl("cv", number = 10), tuneGrid = expand.grid(alpha = 0, lambda = l))
#Predicting the ViolentCrimesPerPop using train data.
testingridge <- predict(regridge, trainData)
#Calculating RMSE value
rmse(trainData$log_price, testingridge)

# 0.4659843

#Performing Lasso regression.
reglasso <- train(data = trainData, log_price ~ ., method = "glmnet", trControl = trainControl("cv", number = 10), tuneGrid = expand.grid(alpha = 1, lambda = l))
#Predicting the ViolentCrimesPerPop using train data.
testinglasso <- predict(reglasso, trainData)
#Calculating RMSE value
rmse(trainData$log_price, testinglasso)

# 0.711494

#Performing Elastic net regression.
regelastic <- train(data = trainData, log_price ~ ., method = "glmnet", trControl = trainControl("cv", number = 10), tuneGrid = expand.grid(alpha = seq(0, 1, 10), lambda = l))
#Predicting the ViolentCrimesPerPop using train data.
testingelastic <- predict(regelastic, trainData)
#Calculating RMSE value
rmse(trainData$log_price, testingelastic)

# 0.4659843

