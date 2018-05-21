rm(list = ls())
setwd("F:/Bike Sharing")

#Import data
bike <- read.csv("train.csv")


#Data Preparation
bike$season <- as.factor(bike$season)
bike$holiday <- as.factor(bike$holiday)
bike$workingday <- as.factor(bike$workingday)
bike$weather <- as.factor(bike$weather)
levels(bike$season) <- c("spring", "summer", "fall", "winter")
levels(bike$holiday) <- c("not holiday", "holiday")
levels(bike$workingday) <- c("not workingday", "workingday")
levels(bike$weather) <- c("clear", "mist", "light rainsnow", "heavy rainsnow")

#Check missing values
str(bike)
summary(bike)

#Check outliers
boxplot(bike$temp, data = bike)
boxplot(bike$atemp)
boxplot(bike$humidity)
boxplot(bike$windspeed)
humidity.outlier <- which(bike$humidity < 20)
bike$humidity[humidity.outlier]

#Data Explorayary
#Explore the relationship between season and demand

#Summer and fall have higher demands than spring and winter
library(ggplot2)
ggplot(data = bike, aes(x = season, y = count)) + geom_boxplot()
library(dplyr)
total.rentals.byseason <- bike %>%
  group_by(season) %>%
  summarise(total.rentals = sum(count)) 
ggplot(data = total.rentals.byseason, aes(x = season, y = total.rentals)) + geom_bar(stat = "identity", fill = "dodgerblue3")+
  geom_text(aes(label = total.rentals), colour = "white", vjust = 1)
ggplot(data = bike, aes(x = season, y = count)) + geom_bar(stat = "identity", fill = "dodgerblue3") +
  geom_text(aes(label = count), colour = "white", vjust = 1)

#Explore the relationship between holiday and demand
ggplot(data = bike, aes(x = holiday, y = count)) + geom_boxplot()

#Explore the relationhsip between workingday and not workingday
ggplot(data = bike, aes(x = workingday, y = count)) + geom_boxplot()

#Explore the relationship between weather and demand
ggplot(data = bike, aes(x = weather, y = count)) + geom_boxplot()
ggplot(data = bike, aes(x = weather, y = count)) + geom_bar(stat = "identity", fill = "dodgerblue3")
ggplot(bike, aes(count, fill = weather)) + geom_histogram(alpha = 0.5)

#Explore the relationship between temperature and demand
ggplot(bike, aes(x = temp, y = count)) + geom_point()
ggplot(bike, aes(x = cut(temp, breaks = 5), y = count)) + geom_boxplot()
cor(bike$temp, bike$count)

#Explore the relationship between "feels like" temperature and demand
cor(bike$temp, bike$atemp)
ggplot(bike, aes(x = cut(atemp, breaks = 5), y = count)) + geom_boxplot()
cor(bike$atemp, bike$count)

#Explore the relationship between humidty and demand
cor(bike$humidity, bike$count)
ggplot(bike, aes(x = cut(humidity, breaks = 5), y = count)) + geom_boxplot()

#Explore the relationship between windspeed and demand
cor(bike$windspeed, bike$count)
ggplot(bike, aes(x = cut(windspeed, breaks = 5), y = count)) + geom_boxplot()

#Explore the relationship between non-registered users and demand
cor(bike$casual, bike$count)
ggplot(bike, aes(x = casual, y = count)) + geom_point()

#Explore the relatioship between registered users and demand
cor(bike$registered, bike$count)
ggplot(bike, aes(x = registered, y = count)) + geom_point() + geom_smooth(method = "lm", se = FALSE)

#Explore the time and the demand
library(tidyr)
#Split the timestamp into Year, Month, Date and Hour
bike <- separate(bike, col = datetime, into = c("Date", "Hour"), sep = " ")
bike <- separate(bike, col = Date, into = c("Year", "Month", "Date"), sep = "-")
bike$hour_simple <- substr(bike$Hour, 1, 2)

#Plot mean hourly rentals
#08:00, 17:00-19:00 has high average demand
hour_count <- bike %>%
  group_by(hour_simple) %>%
  summarise(mean.register = mean(count))
ggplot(data = hour_count, aes(x = hour_simple, y = mean.register)) + geom_bar(stat = "identity")


#Plot mean monthly rentals, non-registered user rentals and registered user rentals
library(xts)
bike$datetime <- as.Date(bike$datetime)
bike_xts <- as.xts(bike[, -1], order.by = bike$datetime)
head(bike_xts)
class(bike_xts)
periodicity(bike_xts)
nmonths(bike_xts)

monthly_split_count <- split(bike_xts$count , f = "months")
mean_of_means_count <- lapply(monthly_split_count, FUN = mean)
month_index <- to.period(bike_xts, period = "months", OHLC = FALSE, indexAt = "firstof")
monthly_count <- as.xts(as.numeric(mean_of_means_count), order.by = index(month_index))
head(temps_monthly_count)
plot.xts(temps_monthly_count)

monthly_split_casual <- split(bike_xts$casual , f = "months")
mean_of_means_casual <- lapply(monthly_split_casual, FUN = mean)
monthly_casual <- as.xts(as.numeric(mean_of_means_casual), order.by = index(month_index))

monthly_split_registered <- split(bike_xts$registered , f = "months")
mean_of_means_registered <- lapply(monthly_split_registered, FUN = mean)
monthly_registered <- as.xts(as.numeric(mean_of_means_registered), order.by = index(month_index))

mean_month<-merge(monthly_count, monthly_casual, monthly_registered)

plot.zoo(mean_month, plot.type = "single", lty = c(1,2,3))
legend("topright", lty=c(1,2,3), legend = c("total count","casual","registered"), bg = "white")
axis(1, at = index(mean_month), format(index(mean_month), "%b %Y"), cex.axis = .7)



