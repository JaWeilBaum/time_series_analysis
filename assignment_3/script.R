library(forecast)
library(mltools)
library(data.table)
library(Matrix)
library(gets)

setwd("~/workspace/time_series_analysis/assignment_3/")

variance_over_time = function(data_series) {
  result_data = c()
  for (i in 1:length(data_series)) {
    result_data = c(result_data, var(data_series[1:i]))
  }
  return(result_data)
}

res = 200
height = 15
width = 25


data = read.csv("DataAssignment3.csv")

uniqe_parties = unique(data$Government)
parties_colors = unique(data$Color)

use_frame = FALSE
par(mfrow=c(2,1))
plot(Y_diff)
lines(Y_diff + sqrt(variance_over_time(Y_diff)), col="red")
lines(Y_diff - sqrt(variance_over_time(Y_diff)), col="red")

Y = data$NumberWords
Y_diff = diff(data$NumberWords)
Y_log = log(data$NumberWords)
(Y_log_diff = diff(log(data$NumberWords)))
(Y_log_diff_diff = diff(diff(log(data$NumberWords))))



plot(variance_over_time(Y_log_diff_diff))

#Normal data viz
png("ex_3_1_data_overview.png", width=width, height=height, units="cm", res=res)
par(mfrow=c(2,2))
plot(data$Date, Y, col=data$Color, frame=use_frame, main="Total # of words in legislation", xlab = "Year", ylab = "Total number of words")
legend(1920, 3*10^7, legend=uniqe_parties, col=parties_colors, pch=1)
plot(data$Date, Y_log, col=data$Color, frame=use_frame, main="Total log(# of words in l.)", xlab = "Year", ylab = "Log(Total words)")
plot(data$Date[2:102], Y_log_diff, col=data$Color, frame=use_frame, main="Diff. of log(# of words in l.)", xlab = "Year of change", ylab = "Diff. of Log(Total words)")
plot(data$Date[3:102], Y_log_diff_diff, col=data$Color, frame=use_frame, main="Diff. of Diff. log(# of words in l.)", xlab = "Year of change", ylab = "Diff. of Diff. of log(Total words)")
dev.off()

# Variance over time viz
png("ex_3_1_data_variance_over_time.png", width=width, height=height, units="cm", res=res)
par(mfrow=c(2,2))
plot(data$Date, variance_over_time(Y), frame=use_frame, main="Variance over time (total # of words)", xlab = "Year", ylab = "Total number of words")
plot(data$Date, variance_over_time(Y_log), frame=use_frame, main="vot. Total log(# of words in l.)", xlab = "Year", ylab = "vot. Log(Total words)")
plot(data$Date[2:102], variance_over_time(Y_log_diff), frame=use_frame, main="vot. Diff. of log(# of words in l.)", xlab = "Year of change", ylab = "vot. Diff. of Log(Total words)")
plot(data$Date[3:102], variance_over_time(Y_log_diff_diff), frame=use_frame, main="vot. Diff. of Diff. log(# of words in l.)", xlab = "Year of change", ylab = "vot .Diff. of Diff. of log(Total words)")
dev.off()

data

mean(Y)
mean(Y_log)
mean(Y_log_diff)
mean(Y_log_diff_diff)

par(mfrow=c(1,2))

acf(Y_log_diff_diff)
pacf(Y_log_diff_diff)

plot(auto.arima(Y_diff, allowdrift = F))

plot(auto.arima(Y_log))

hist(auto.arima(Y_log_diff_diff)$residuals)
plot(arima(Y_log_diff_diff, order=c(1,0,1)))
par(mfrow=c(1,1))


Y = data$NumberWords
Y_diff = diff(data$NumberWords)
Y_log = log(data$NumberWords)
(Y_log_diff = diff(log(data$NumberWords)))
(Y_log_diff_diff = diff(diff(log(data$NumberWords))))
plot(Y_log_diff_diff)



png("ex_3_2_acf_pacf_plots.png", width=width, height=height, units="cm", res=res)
par(mfrow=c(2,4))
acf(Y, main="ACF of Y", frame=FALSE)
acf(Y_log, main="ACF of Log(Y)", frame=FALSE)
acf(Y_log_diff, main="ACF of Diff(Log(Y))", frame=FALSE)
acf(Y_log_diff_diff, main="ACF of Diff(Diff(Log(Y)))", frame=FALSE)
pacf(Y, main="PACF of Y", frame=FALSE, xlim=c(0,20))
pacf(Y_log, main="PACF of Log(Y)", frame=FALSE, xlim=c(0,20))
pacf(Y_log_diff, main="PACF of Diff(Log(Y))", frame=FALSE, xlim=c(0,20))
pacf(Y_log_diff_diff, main="PACF of Diff(Diff(Log(Y)))", frame=FALSE, xlim=c(0,20))
dev.off()
# layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))

arma_1_1 = arima(Y_log_diff_diff, order=c(1,0,1))
ma_1 = arima(Y_log_diff_diff, order=c(0,0,1))

png("ex_3_2_MA_model.png", width=width, height=height, units="cm", res=res)
layout(matrix(c(1, 1, 2, 3, 4, 5), 2, 3, byrow = F))
plot(ma_1$residuals, type="p", main="MA(1) model", ylab="Residuals",frame=F)
abline(0,0, col="red")
acf(ma_1$residuals, main="ACF MA(1) model", frame=F)
pacf(ma_1$residuals, main="PACF MA(1) model", frame=F, xlim=c(0,20))
qqnorm(ma_1$residuals, main="MA(1) Normal Q-Q Plot",frame=F)
qqline(ma_1$residuals)
hist(ma_1$residuals, xlab="Residuals", main="MA(1) model residuals")
dev.off()


a_ma_1 = auto.arima(Y_log)

a_ma_1_forecast = forecast(a_ma_1)


ma_1_forecast = forecast(ma_1, h=10)
upper_bound = exp(data.frame(a_ma_1_forecast$upper)$X95.)
lower_bound = exp(data.frame(a_ma_1_forecast$lower)$X95.)
new_data = exp(a_ma_1_forecast$mean)
png("ex_3_3_prediction.png", width=width, height=height, units="cm", res=res)
plot(c(1919:2020), Y, xlim=c(1919,2030), 
     type="l",
     ylim = c(0, max(upper_bound)),
     main="Prediction with ARIMA(0,2,1) model", 
     ylab="Total number of words in legislation", 
     xlab="Year", 
     yaxt="n",
     frame=F)
lines(c(2021:2030), new_data, col="steelblue")
lines(c(2021:2030), upper_bound, col="red", lty=2)
lines(c(2021:2030), lower_bound, col="red", lty=2)
marks_l = c(0, "20 Mio.", "40 Mio.", "60 Mio.")
marks = c(0, 2e7, 4e7, 6e7)
axis(2,at=marks,labels=marks_l)
axis(2,at=marks,labels=marks_l)
dev.off()
par(mfrow=c(1,1))

data[102,]
all_data[102,]



par(mfrow=c(1,2))
plot(Y-exp(cumsum(c(Y_log[1], cumsum(c(Y_log_diff[1], Y_log_diff_diff))))))


all_data = cbind(c(1919:2030), exp(cumsum(c(Y_log[1], Y_log_diff, forecast_data$`Point Forecast`))))
low_95 = cbind(c(2020:2030), exp(cumsum(c(Y_log[1], Y_log_diff, forecast_data$`Lo 95`)))[102:112])
high_95 = cbind(c(2020:2030), exp(cumsum(c(Y_log[1], Y_log_diff, forecast_data$`Hi 95`)))[102:112])

plot(all_data, ylim=c(min(all_data[,2]), max(high_95)), xlab="Year", ylab="Number of words", frame=use_frame, )
lines(low_95, col="red")
lines(high_95, col="red")


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


#(auto_fit = auto.arima(Y_log_diff_diff, xreg = x_reg[3:102, ]))
# plot(auto_fit)

cbind(data_extendend[3:102, government_names])


x_reg = cbind(Conservative = data_extendend$Government_Conservative, 
              Independent = data_extendend$Government_Independent,
              Social_Democrats = data_extendend$Government_Social.Democrats,
              Social_Liberals = data_extendend$Government_Social.Liberals,
              Venstre = data_extendend$Government_Venstre
)

for (name in government_names) {
  print(name)
  print(sum(data_extendend[, name]))
}



fit2 = arima(Y_log_diff_diff, order=c(3,0,0), xreg=x_reg[3:102,], include.mean = F)

fit2

factors = c(-0.7582, -0.451, -0.1211)

Y_log_diff_diff[98:100] %*% factors - 0.0001

acf(fit2$residuals)
pacf(fit2$residuals)
head(x_reg)

num_years = 10
xreg_s_d = cbind(Conservative = rep(0,num_years),
                 Independent = rep(0,num_years),
                 Social_Democrats = rep(1,num_years),
                 Social_Liberals = rep(0,num_years),
                 Venstre = rep(0,num_years))

head(xreg_s_d)
fit
forecast(fit, h=10)
par(mfrow=c(1,1))
plot(Y_log_diff_diff)

fit2
predict(fit2, newxreg=xreg_s_d)

cbind(intercept=1,x_reg)

new_xreg = cbind(party_changed = c(0,as.numeric(data$Government[2:102] == data$Government[1:101])), x_reg)
svd(new_xreg[2:102,])$d

(auto_fit = auto.arima(Y_log_diff_diff, xreg = cbind(
  data_extendend$Government_Conservative, 
  data_extendend$Government_Social.Democrats,
  jitter(data_extendend$Government_Independent, factor = 0.0001),
  data_extendend$Government_Venstre,
  data_extendend$Government_Social.Liberals)[3:102,]))


a_f_f = forecast(auto_fit, xreg = cbind(0, rep(1, 10),0,0,0))

plot(exp(cumsum(c(Y_log[1], cumsum(c(Y_log_diff[1], c(a_f_f$x, a_f_f$mean)))))))

plot(forecast_data$`Point Forecast`)




plot(Y_log_diff_diff)
points(predict(fit2, newxreg=xreg_s_d)$pred, col="blue")

forecast()


c(Y_log_diff[1], Y_log_diff_diff, predict(fit2, newxreg=new_x_reg)$pred)

plot(exp(cumsum(c(Y_log[1], cumsum(c(Y_log_diff[1], Y_log_diff_diff, predict(fit2, newxreg=new_x_reg)$pred))))), type='l')

lines(exp(cumsum(c(Y_log[1], cumsum(c(Y_log_diff[1], Y_log_diff_diff, predict(fit2, newxreg=new_x_reg)$pred))))), col="red")
new_x_reg = cbind(rep(0,5),rep(0,5),rep(1,5),rep(0,5),rep(0,5))
lines(exp(cumsum(c(Y_log[1], cumsum(c(Y_log_diff[1], Y_log_diff_diff, predict(fit2, newxreg=new_x_reg)$pred))))), col="green")
Y[100]
forecast(fit2, newxreg=new_x_reg)

par(mfrow=c(2,2))
acf(fit2$residuals)
pacf(fit2$residuals)
plot(fit2)


