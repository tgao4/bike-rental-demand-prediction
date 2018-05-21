library('ggplot2')
library('forecast')
library('tseries')

#Read data
daily_data = read.csv('day.csv', header=TRUE, stringsAsFactors = FALSE)

#Plot the time series
daily_data$Date = as.Date(daily_data$dteday)
ggplot(daily_data, aes(Date, cnt)) + geom_line() + scale_x_date('month') + ylab("Daily Bike rentals") +
  xlab("")

#Remove time series outliers
count_ts = ts(daily_data[, c('cnt')])

daily_data$clean_cnt = tsclean(count_ts)

ggplot() +
  geom_line(data = daily_data, aes(x = Date, y = clean_cnt)) + ylab('Cleaned Bicycle Count')

#Smooth the data
daily_data$cnt_ma = ma(daily_data$clean_cnt, order = 7) # using the clean count with no outliers
daily_data$cnt_ma30 = ma(daily_data$clean_cnt, order = 30)


ggplot() +
  geom_line(data = daily_data, aes(x = Date, y = clean_cnt, colour = "Counts")) +
  geom_line(data = daily_data, aes(x = Date, y = cnt_ma,   colour = "Weekly Moving Average"))  +
  geom_line(data = daily_data, aes(x = Date, y = cnt_ma30, colour = "Monthly Moving Average"))  +
  ylab('Bicycle Count')

#Decompose the time series
count_ma = ts(na.omit(daily_data$cnt_ma), frequency = 30)
decomp = stl(count_ma, s.window = "periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp)

#Check acf and pacf
library('astsa')
d_count_ma <- diff(count_ma)
plot(d_count_ma)
acf2(d_count_ma)

#Fit the model
sarima(count_ma, p = 1, d = 1, q = 7)
sarima(count_ma, p = 1, d = 1, q = 1, P = 0, D = 1, Q = 1, S = 7)

#predict
sarima.for(count_ma, n.ahead = 30, p = 1, d = 1, q = 7)