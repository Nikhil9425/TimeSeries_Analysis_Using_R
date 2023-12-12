library(MASS)
library(tseries)
library(forecast)

data<-read.csv('C:\\Users\\HP\\Documents\\TS project\\NFLX.csv')
head(data)

stock_ts<-ts(data$Adj.Close,start =c(2017,17),frequency =365)
plot(stock_ts,type='l')

acf(stock_ts)
pacf(stock_ts)

stock_diff<-diff(log(stock_ts))
stock_log<-log(stock_ts)

model.arima<-Arima(stock_diff,order = c(5,1,5))
summary(model.arima)

model1<-Arima(stock_log[1:1270],order = c(5,1,5))

forecast1<-forecast(model.arima,h=30)
plot(forecast1)

forecast_ori <- forecast(model.arima, h = 100)
a <- ts(stock_diff)
forecast_ori %>% autoplot() + autolayer(a)

