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
data
uniqe_parties = c(
  "Conservative",
  "Independent",
  "Social Democrats",
  "Social Liberals",
  "Venstre"
)
parties_colors = c(
  "black",
  "green",
  "red",
  "pink",
  "blue"
)
Y = data$NumberWords
Y_diff = diff(data$NumberWords)
Y_log = log(data$NumberWords)
(Y_log_diff = diff(log(data$NumberWords)))
(Y_log_diff_diff = diff(diff(log(data$NumberWords))))

use_frame = FALSE
par(mfrow=c(2,1))
plot(Y_diff)
lines(Y_diff + sqrt(variance_over_time(Y_diff)), col="red")
lines(Y_diff - sqrt(variance_over_time(Y_diff)), col="red")




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
par(mfrow=c(1,1))
plot(cumsum(c(Y_log[102], cumsum(c(Y_log_diff[101], forecast(ma_1)$mean)))), type="l")

lines(c(1:10), forecast(a_ma_1)$mean, col="red")



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

transformed_data_upper = data.frame(forecast(ma_1, h=10)$upper)$X95.
transformed_data_lower = data.frame(forecast(ma_1, h=10)$lower)$X95.

png("ex_3_3_raw_prediction.png", width=width, height=height, units="cm", res=res)
plot(c(1921:2020), forecast(ma_1, h=10)$x, frame=F, col="black", main="MA(1) prediction", ylab="Diff(Diff(log(Y)))", xlab="Year", xlim=c(1919,2030))
lines(c(2021:2030), forecast(ma_1, h=10)$mean, col="steelblue", type="p")
lines(c(2021:2030), transformed_data_upper, col="red", lty=2)
lines(c(2021:2030), transformed_data_lower, col="red", lty=2)
legend(1920, .25, legend=c("Observations", "Prediction", "95% Prediciton interval"), col=c("black", "steelblue", "red"), pch=c(1,1, NA), lty=c(NA, NA, 2))
dev.off()

diff(exp(forecast(a_ma_1, h=10)$mean))
forecast(ma_1, h=10)$mean



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


set.seed(1)


jitter_factor = 0.00001

design_matrix =  cbind(
  conservative=jitter(data_extendend$Government_Conservative, factor = jitter_factor), 
  independant=jitter(data_extendend$Government_Independent, factor = jitter_factor),
  social_democrats=jitter(data_extendend$Government_Social.Democrats, factor = jitter_factor),
  social_liberals=jitter(data_extendend$Government_Social.Liberals, factor = jitter_factor),
  venstre=jitter(data_extendend$Government_Venstre, factor = jitter_factor))

diff_design_matrix = apply(apply(design_matrix, 2, diff), 2, diff)

(auto_fit = arima(Y_log_diff, order=c(1,0,1), include.mean = F, xreg = design_matrix[2:102,]))

auto.arima(Y_log_diff_diff, allowmean = F, xreg = design_matrix[3:102,])

(auto_fit = auto.arima(Y_log_diff, xreg = design_matrix[2:102,]))

png("ex_3_5_residuals.png", width=width, height=height, units="cm", res=res)
# layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))
par(mfrow=c(1,2))
# ccf(auto_fit$residuals, Y_log_diff_diff)
acf(auto_fit$residuals)
pacf(auto_fit$residuals)
dev.off()

par(mfrow=c(1,1))

plot(forecast(auto_fit, xreg=xreg_test))

# auto_fit$xreg = design_matrix[3:102,]

xreg_test = cbind(
  conservative=rep(0, 10), 
  independant=0, 
  social_democrats=rep(1, 10), 
  social_liberals=0,
  venstre=0)

auto_fit_predict = forecast(auto_fit, xreg=xreg_test)
auto_fit_predict.lower = data.frame(auto_fit_predict$lower)
auto_fit_predict.upper = data.frame(auto_fit_predict$upper)

auto_fit_predict_transformed = exp(cumsum(c(Y_log[102], auto_fit_predict$mean)))

upper_95 = exp(cumsum(c(Y_log[102], auto_fit_predict.upper$X95.)))[2:11]
lower_95 = exp(cumsum(c(Y_log[102], auto_fit_predict.lower$X95.)))[2:11]

png("ex_3_5_social_democrats.png", width=width, height=height, units="cm", res=res)
plot(c(1919:2020), Y, xlim=c(1960, 2030), ylim=c(0,1e8),
     frame=F,
     xlab="Year",
     ylab="Number of words",
     yaxt="n",
     main="Prediction of Social Democrats (2021- 2030)")
marks_l = c(0, "20 Mio.", "40 Mio.", "60 Mio.", "80 Mio.", "100 Mio.")
marks = c(0, 2e7, 4e7, 6e7, 8e7, 1e8)
axis(2,at=marks,labels=marks_l)
lines(c(2021:2030), auto_fit_predict_transformed[3:12], col=parties_colors[3])
lines(c(2021:2030), lower_95, col="red",lty=2)
lines(c(2021:2030), upper_95, col="red",lty=2)
legend(1960, 1e8, legend=c("Observations", "Prediction", "95% Prediction interval"), col=c("black", "red", "red"), lty=c(NA, 1, 2), pch=c(1,NA,NA))
dev.off()



png("ex_3_5_all_parties.png", width=width, height=height, units="cm", res=res)
par(mfrow=c(1,1))
plot(c(1919:2020), Y, xlim=c(1950,2030), ylim=c(0,1e8), col=data$Color, 
     main="Total number of words in danish legislation",
     ylab="Number of words",
     xlab="Year",
     yaxt="n",frame=F)

marks_l = c(0, "20 Mio.", "40 Mio.", "60 Mio.", "80 Mio.", "100 Mio.")
marks = c(0, 2e7, 4e7, 6e7, 8e7, 1e8)
axis(2,at=marks,labels=marks_l)

uniqe_parties

return_data = c()

for (c_idx in c(1:5)) {
  xreg_test = cbind(
    conservative=rep(0, 10), 
    independant=0, 
    social_democrats=0, 
    social_liberals=0,
    venstre=0) 
  xreg_test[, c_idx] = 1
  auto_fit_predict = forecast(auto_fit, xreg=xreg_test)
  
  auto_fit_predict_transformed = exp(cumsum(c(Y_log[102], auto_fit_predict$mean)))
  return_data = cbind(return_data, auto_fit_predict_transformed)
  lines(c(2021:2030), auto_fit_predict_transformed[2:11], col=parties_colors[c_idx])
}
legend(1950, 1e8, legend=uniqe_parties, col=parties_colors,lty=1)
colnames(return_data) = uniqe_parties
rownames(return_data) = c(2020:2030)
write.csv(return_data, "government_predictions.csv", )
dev.off()




