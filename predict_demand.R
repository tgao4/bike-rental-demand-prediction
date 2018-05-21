library(dplyr)
library(tidyr)
setwd("F:/R/Bike-Sharing-demand-prediction")

#Read training file
dat = read.csv("train.csv")
#Read test file
bike_test <- read.csv("test.csv")
all <- read.csv("all.csv")

#Test file actual bike rentals
all$hr1 <- paste0(all$hr, ":00")
all$datetime <- paste(all$dteday, all$hr1)
all$datetime <- strftime(all$datetime, "%Y-%m-%d %H:%M:%S")
library("dplyr")
all_new <- inner_join(all, bike_test, by = "datetime", type = "inner")
actual <- all_new$cnt

#Split time stamp into Year, Month, Date and Hour
bike <- separate(dat, col = datetime, into = c("Date", "Hour"), sep = " ")
bike_test <- separate(bike_test, col = datetime, into = c("Date", "Hour"), sep = " ")

bike <- separate(bike, col = Date, into = c("Year", "Month", "Date"), sep = "-")
bike_test <- separate(bike_test, col = Date, into = c("Year", "Month", "Date"), sep = "-")

bike$hour_simple <- substr(bike$Hour, 1, 2)
bike_test$hour_simple <- substr(bike_test$Hour, 1, 2)

bike$Year <- as.factor(bike$Year)
bike_test$Year <- as.factor(bike_test$Year)

bike$Month <- as.factor(bike$Month)
bike_test$Month <- as.factor(bike_test$Month)

bike$hour_simple <- as.factor(bike$hour_simple)
bike_test$hour_simple <- as.factor(bike_test$hour_simple)

#Make categorical variables as nominal variables
bike$season <- factor(bike$season, levels = c(1, 2, 3, 4), labels = c("spring", "summer", "fall", "winter"))
bike_test$season <- factor(bike_test$season, levels = c(1, 2, 3, 4), labels = c("spring", "summer", "fall", "winter"))

bike$holiday <- factor(bike$holiday, levels = c(0, 1), labels = c("no", "yes"))
bike_test$holiday <- factor(bike_test$holiday, levels = c(0, 1), labels = c("no", "yes"))

bike$workingday <- factor(bike$workingday, levels = c(0, 1), labels = c("no", "yes"))
bike_test$workingday <- factor(bike_test$workingday, levels = c(0, 1), labels = c("no", "yes"))

bike$weather <- factor(bike$weather, levels = c(1, 2, 3, 4), labels = c("clear", "mist", "light rainsnow", "heavy rainsnow"))
bike_test$weather <- factor(bike_test$weather, levels = c(1, 2, 3, 4), labels = c("clear", "mist", "light rainsnow", "heavy rainsnow"))


#build models
# Create the formula string for bike rentals as a function of the inputs
outcome <- "count"
vars <- c("hour_simple", "holiday", "workingday", "weather", "temp", "humidity", "windspeed", "season")
(fmla <- paste(outcome, "~", paste(vars, collapse = " + ")))


(mean_bikes <- mean(bike$count))
(var_bikes <- var(bike$count))

library("vtreat")
nRows <- nrow(bike)
# Implement the 3-fold cross-fold plan with vtreat
splitPlan <- kWayCrossValidation(nRows, 3, NULL, NULL)


k <- 3 
bike$pred.poisson.cv <- 0 
for(i in 1:k) {
  split <- splitPlan[[i]]
  model <- glm(fmla,family = quasipoisson, data = bike[split$train,])
  bike$pred.poisson.cv[split$app] <- predict(model, data=bike[split$app,], type = "response")
}

bike$pred.rf.cv <- 0 
seed<-423563
for(i in 1:k) {
  split <- splitPlan[[i]]
  model <- ranger(fmla, 
                  bike, 
                  num.trees = 500, 
                  respect.unordered.factors = "order", 
                  seed = seed)
  bike$pred.rf.cv[split$app] <- predict(model, data=bike[split$app,])$predictions
}

rmse<-function(predcol, ycol) {
  res <- predcol - ycol
  sqrt(mean(res^2))
}

rmse(bike$pred.poisson.cv, bike$count)
rmse(bike$pred.rf.cv, bike$count)


library("ranger")
seed<-423563
(bike_model_rf <- ranger(fmla, 
                         bike, 
                         num.trees = 500, 
                         respect.unordered.factors = "order", 
                         seed = seed))
bike_test_predict = predict(bike_model_rf, bike_test)$predictions

#Test accuracy
rmse(bike_test_predict, actual)


