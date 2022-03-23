library(forecast)
library(mltools)
library(data.table)

setwd("~/workspace/time_series_analysis/assignment_3/")

variance_over_time = function(data_series) {
  result_data = c()
  for (i in 1:length(data_series)) {
    result_data = c(result_data, var(data_series[1:i]))
  }
  return(result_data)
}


data = read.csv("DataAssignment3.csv")

uniqe_parties = unique(data$Government)
parties_colors = unique(data$Color)

use_frame = FALSE
par(mfrow=c(2,1))
plot(Y_diff)
lines(Y_diff + sqrt(variance_over_time(Y_diff)), col="red")
lines(Y_diff - sqrt(variance_over_time(Y_diff)), col="red")


par(mfrow=c(2,2))
plot(data$Date, data$NumberWords, col=data$Color, frame=use_frame, main="Raw data", xlab = "Year", ylab = "Total words")
legend(1920, 3*10^7, legend=uniqe_parties, col=parties_colors, lty=5)
plot(data$Date[2:102], diff(data$NumberWords), col=data$Color, frame=use_frame, main="Word difference 1 year", xlab = "Year of change", ylab = "Difference in total words (1 Year)")
plot(data$Date, log(data$NumberWords), col=data$Color, frame=use_frame, main="Log Y", xlab = "Year", ylab = "Log(Total words)")
plot(data$Date[2:102], diff(log(data$NumberWords)), col=data$Color, frame=use_frame, main="Log Y + Word difference 1 year", xlab = "Year of change", ylab = "Difference of Log(Total words)")


par(mfrow=c(1,1))



Y = data$NumberWords
Y_diff = diff(data$NumberWords)
Y_log = log(data$NumberWords)
(Y_log_diff = diff(log(data$NumberWords)))

Y[1]

Y
Y_log
cumsum(c(Y[1],exp(Y_log_diff)))


par(mfrow=c(4,2))
acf(Y, main="ACF of Number of words", frame=FALSE)
pacf(Y, main="PACF of Number of words")
acf(Y_diff, main="ACF of Diff(Number of words)", frame=FALSE)
pacf(Y_diff, main="PACF of Diff(Number of words)")
acf(Y_log, main="ACF of Log(Number of Words)")
pacf(Y_log, main="PACF of Log(Number of Words)")
acf(Y_log_diff, main="ACF of Diff(Log(Number of Words))")
pacf(Y_log_diff, main="PACF of Diff(Log(Number of Words))")

layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))

fit = arima(Y_log_diff, order=c(3,0,0))
summary(fit)
plot(fit$residuals, type="p", main="Residuals of ARIMA(1,1,1) model", frame=use_frame)
acf(fit$residuals, main="ACF on ARIMA(1,1,1) Residuals", frame=use_frame)
pacf(fit$residuals, main="PACF on ARIMA(1,1,1) Residuals", frame=use_frame)

forecast_data = as.data.frame(forecast(fit, h=10))
par(mfrow=c(1,1))

all_data = c(Y_log_diff, forecast_data$`Point Forecast`)
cumsum(exp(all_data))
plot(c(1:111),)


data$Government = as.factor(data$Government)
data_extendend = data.frame(one_hot(as.data.table(data)))
colnames(data_extendend)
government_names = c("Government_Conservative", 
                     "Government_Independent", 
                     "Government_Social.Democrats", 
                     "Government_Social.Liberals", 
                     "Government_Venstre")
length(data_extendend$Date)
length(Y_log_diff)


(auto_fit = auto.arima(Y_log_diff))


cbind(data_extendend[2:102, government_names])

cbind(data_extendend$Government_Conservative[2:102], data_extendend$Government_Independent[2:102])
x_reg = cbind(1, data_extendend$Government_Conservative, 
              data_extendend$Government_Independent,
              data_extendend$Government_Social.Democrats,
              data_extendend$Government_Social.Liberals,
              data_extendend$Government_Venstre
)

x_reg

for (name in government_names) {
  print(sum(data_extendend[, name]))
}


arima(Y_diff, order=c(1,0,1), xreg = x_reg[2:102,])




