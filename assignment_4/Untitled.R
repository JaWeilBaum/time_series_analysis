library(forecast)
library(tidyverse)
library(xts)
library(zoo)
library(marima)

setwd("~/workspace/time_series_analysis/assignment_4/")

df = read.csv("A4_data.csv", sep = "\t")
df$yyyymm = as.character(df$yyyymm)
df$yyyymm = parse_date(df$yyyymm, format="%Y%m")
# df$year = as.numeric(format(parse_date(df$yyyymm, format="%Y%m"), "%Y"))
# df$month = as.numeric(format(parse_date(df$yyyymm, format="%Y%m"), "%m"))

columns = c("Cnsmr", "Manuf", "HiTec", "Hlth", "Other")

for (col in c("b.m", "ntis", "infl")) {
  png(paste("ex_4_5_", col ,".png", sep = ""), width=20, height=7.5, units="cm", res=200)
  layout(matrix(c(1, 1, 2), 1, 3, byrow = F))
  plot(z[, col], type="l", ylab = "Return", xlab = "Year", main=paste("Time Series of", col), bty="n", frame.plot=F)
  hist(z[,col], las=1, xlab = "Return", main = paste("Distrubution of", col))
  dev.off()
}




seas_arima = auto.arima(ts(df.used$Cnsmr[-c(921:924)], frequency=12))
seas_forecast = forecast(seas_arima)
plot(seas_forecast)


for (col in columns) { 
  df[,col] = (df[,col] - mean(df[, col])) / sd(df[, col])
}
head(df)
mean(df$Cnsmr)

z = read.zoo(df)
plot(z)

colors = c(1:5)

par(mfrow=c(1,5))
plot(z$Cnsmr, type="l")
for (col_idx in 2:5) {
  plot(z[,columns[col_idx]], col=colors[col_idx])
  print(mean(z[,columns[col_idx]]))
  print(sd(z[,columns[col_idx]]))
}

par(mfrow=c(1,2))
for (col in columns) {
  acf(as.numeric(z[, col]), main=col, lag.max = 15)
  pacf(as.numeric(z[, col]), main=col, lag.max = 15)
}
head(z)

df.used = df[158:1081,]
rownames(df.used) = NULL
df.used[(NROW(df.used)-3):NROW(df.used),] <- NA

df.used$b.m = df$b.m[158:1081]
df.used$ntis = df$ntis[158:1081]
df.used$infl = df$infl[158:1081]
df.used$true_b.m = df$b.m[158:1081]
df.used$true_ntis = df$ntis[158:1081]
df.used$true_infl = df$infl[158:1081]


tail(df.used, 10)

model_arma_1_1_ind = define.model(kvar = ncol(df.used) - 1, ar=c(1), ma=c(1), rem.var=c(6:11))
model_arma_1_1_ind$ar.pattern[2,2,2] = 0
# model_arma_1_1_ind$ar.pattern[2,2,2] = 0


arma_1_1_ind = marima(DATA = df.used[1:920,-1],
                      means = 1,
                      ar.pattern = model_arma_1_1_ind$ar.pattern, 
                      ma.pattern = model_arma_1_1_ind$ma.pattern,
                      Check = F,
                      Plot="log.det",
                      penalty = 2, max.iter = 400)
short.form(round(arma_1_1_ind$Constant, 3), leading = F)
short.form(round(arma_1_1_ind$ar.estimates,3), leading = F)
short.form(round(arma_1_1_ind$ar.fvalues,1), leading = F)
short.form(round(arma_1_1_ind$ma.estimates, 3), leading = F)
short.form(round(arma_1_1_ind$ma.fvalues, 1), leading = F)

array_to_LaTeX <- function(arr){
  rows <- apply(arr, MARGIN=1, paste, collapse = " & ")
  matrix_string <- paste(rows, collapse = " \\\\ ")
  return(paste("\\begin{bmatrix}", matrix_string, "\\end{bmatrix}"))
}

array_to_LaTeX(round(arma_1_1_ind$Constant, 3))

## One next step could be to see if year is significant as an explanatory variable.
source("../exercises/step.slow.p.marima_2017.R")
## We should used slow.step instead
slarma_1_1_ind <- step.slow.p(model_arma_1_1_ind, data=z)

forecast = arma.forecast(series = df.used[-1], nstart = 920, nstep = 4, marima = arma_1_1_ind)

par(mfrow=c(1,1))
acf(df[158:1081,c(-1,-7,-8, -9)], lag.max = 10, frame=F)

for (index in c(1:5)) {
  
  pred.int <- forecast$forecasts[index,921:924] + cbind(rep(0, 4), -1, 1)*qnorm(0.975)*sqrt(forecast$pred.var[index,index,])
  png(paste("ex_4_4_", columns[index] ,".png", sep = ""), width=20, height=10, units="cm", res=200)
  par(mfrow=c(1,1), mai = c(1,1, 0.4, 0.1))
  
  plot(df.used[,index + 1], 
       type="p", 
       ylab="Return", 
       xlab="Year",
       xaxt="n",
       main=paste("Series:", columns[index]), 
       ylim = c(min(pred.int[,2] * 1.1),max(min(pred.int[,3]*1.1))), 
       xlim=c(877,925), 
       frame.plot = F, 
       bty="n")
  lines(forecast$forecasts[index,])
  matlines(921:924, pred.int, lty=c(1,2,2), col=2, lwd=2 )
  lines(921:924,df[1078:1081,index + 1], type="p", col="red")
  marks_l = c(seq(2013,2017,1))
  marks = c(seq(877,925,12))
  axis(1,at=marks,labels=marks_l)
  legend(877, 11, c("Train data", "Test data", "1-step predicitons", "Prediction", "95% Prediction Interval"),
         col = c(1,2,1,2,2), 
         pch=c(1,1,NA,NA,NA), 
         lty=c(NA,NA,1,1,2),
         horiz = F,
         cex=0.75,
         inset=2)
  dev.off()
}

png("ex_4_4_residuals.png", width=20, height=10, units="cm", res=200)
layout(matrix(c(1,2,3,4,0,5), 2, 3, byrow = T))
for (idx in c(1:5)) { 
  hist(forecast$residuals[idx,-c(921:924)], xlab="Residuals", main=columns[idx])
}


for (index in c(1:5)) {
  pred.int <- cbind(rep(1, 4))*qnorm(0.975)*sqrt(forecast$pred.var[index,index,])
  pred.int <- cbind(forecast$forecasts[index,921:924], pred.int)
  print(pred.int)
}




# model_arma_1_1_ind$ar.pattern[2,2,2] = 0
#model_arma_1_1_ind$ma.pattern[5,1,2] = 0

x_pure = ts(df.used$b.m[-c(921:924)], frequency = 12)
x = diff(x_pure)
tail(df.used)
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = T))
plot(df.used$d_b.m[-c(921:924)], main="Differenced book to market ratio (b.m)", xaxt="n", xlab="Year", ylab="Diff(b.m)")
marks_l = c(seq(1940,2016,25))
marks = c(seq(1,925,25*12))
axis(1,at=marks,labels=marks_l)
#hist(x)
acf(x, main="ACF")
pacf(x, main="PACF")

b.m_forecast = forecast(auto.arima(x_pure))

pred.int = cbind(test_forecast$mean, data.frame(test_forecast$lower)$X95, data.frame(test_forecast$upper)$X95)

plot(test_forecast$x, type="p", xlim=c(70,78), ylim=c(0.1, 0.55), frame=F)
lines(test_forecast$fitted)
lines(pred.int,pred.int[1], col=2)
lines(seq(77 + 8/12, 78 - 1/12, 1/12),ts(df.used$true_b.m, frequency = 12)[921:924], type="p", col=2, xlim=c(76,80))







dev.off()
png("ex_4_5_reg_var_pred.png", width=23, height=12.5, units="cm", res=300)
par(mfrow=c(1,3))
plot(b.m_forecast, xlim=c(70, 78), ylim=c(0.15, 0.6), frame=F, xaxt="n", type="p", main="Book to market ratio")
lines(seq(77 + 8/12, 78 - 1/12, 1/12),ts(df.used$true_b.m, frequency = 12)[921:924], type="l", lwd=3, col=2, xlim=c(76,80))
lines(b.m_forecast$fitted, type="l")
marks_l = c(seq(2009, 2017, 2))
marks = c(seq(70, 78, 2))
axis(1,at=marks,labels=marks_l)

plot(ntis_forecast, xlim=c(70, 78), ylim=c(-0.05, 0.05), frame=F, xaxt="n", type="p", main="Net equity expansion")
lines(seq(77 + 8/12, 78 - 1/12, 1/12),ts(df.used$true_ntis, frequency = 12)[921:924], type="l", lwd=3, col=2, xlim=c(76,80))
lines(ntis_forecast$fitted, type="l")
marks_l = c(seq(2009, 2017, 2))
marks = c(seq(70, 78, 2))
axis(1,at=marks,labels=marks_l)
legend(72, 0.04, c("Train-data", "1-Step prediction", "Forecast", "Test-data"), col=c("black", "black", "steelblue", "red"), lty=c(NA, 1, 1, 1), pch=c(1, NA, NA, NA))

plot(infl_forecast, xlim=c(70, 78), ylim=c(-0.02, 0.03), frame=F, xaxt="n", type="p", main="Inflation")
lines(seq(77 + 8/12, 78 - 1/12, 1/12),ts(df.used$true_infl, frequency = 12)[921:924], type="l", lwd=3, col=2, xlim=c(76,80))
lines(infl_forecast$fitted, type="l")
marks_l = c(seq(2009, 2017, 2))
marks = c(seq(70, 78, 2))
axis(1,at=marks,labels=marks_l)
dev.off() 


pred = round(cbind(
      b.m=df.used$true_b.m[921:924], 
      b.m_pred=b.m_forecast$mean[1:4], 
      ntis=df.used$true_ntis[921:924],
      ntis_pred=ntis_forecast$mean[1:4], 
      infl=df.used$true_infl[921:924],
      infl_pred=infl_forecast$mean[1:4]),3)


array_to_LaTeX(round(pred, 3))


plot(test_forecast$residuals)
acf(test_forecast$residuals)
pacf(test_forecast$residuals)

png("ex_4_5_reg_var_.png", width=20, height=10, units="cm", res=200)
par(mfrow=c(1,3))
hist(df$b.m, main = "Book to market ration", xlab="b.m")
hist(df$ntis, main = "Net equity expansion", xlab="ntis")
hist(df$infl, main = "Inflation", xlab="infl")
dev.off()


x_pure = ts(df.used$ntis[-c(921:924)], frequency = 12)
x = x_pure
par(mfrow=c(1,1))
plot(x)
hist(x)
acf(x)
pacf(x)
dev.off()
(model = auto.arima(x_pure))
ntis_forecast = forecast(model)

plot(test_forecast$x, type="p")
lines(test_forecast$fitted)
plot(test_forecast$residuals)
acf(test_forecast$residuals)
pacf(test_forecast$residuals)
hist(test_forecast$residuals)

x_pure = ts(df.used$infl[-c(921:924)], frequency = 12)
x = x_pure

plot(x)
hist(x)
acf(x)
pacf(x)
infl_forecast = forecast(auto.arima(x_pure))

plot(test_forecast$x, type="p")
lines(test_forecast$fitted)
plot(test_forecast$residuals)
acf(test_forecast$residuals)
pacf(test_forecast$residuals)






par(mfrow=c(1,1))

result_data = c()

# for (ar in c(0:3)) {
#   for (ma in c(0:3)) {
#     if (ar == 0 && ma == 0) {
#       next
#     }
#     ar_part = c(1:ar)
#     if (ar == 0) {
#       ar_part = c(0)
#     }
#     
#     ma_part = c(1:ma)
#     if (ma == 0) {
#       ma_part = c(0)
#     }
    model_arma_1_1_ind = define.model(kvar = ncol(df.used) - 1, ar=c(1), ma=c(1), reg.var = c(6,7,8), rem.var=c(9,10,11))
    model_arma_1_1_ind$ma.pattern[5,1,2] = 0
    arma_1_1_ind = marima(DATA = df.used[1:920,-1],
                          means = 1,
                          ar.pattern = model_arma_1_1_ind$ar.pattern, 
                          ma.pattern = model_arma_1_1_ind$ma.pattern,
                          Check = F,
                          Plot="log.det",
                          penalty = 2,
                          max.iter = 200
                          )
    short.form(round(arma_1_1_ind$ar.estimates,3), leading = F)
    short.form(round(arma_1_1_ind$ar.fvalues,1), leading = F)
    short.form(round(arma_1_1_ind$ma.estimates, 3), leading = F)
    short.form(round(arma_1_1_ind$ma.fvalues, 1), leading = F)
    
    
    
    # array_to_LaTeX(round(arma_1_1_ind$ma.estimates[,,2], 3))
    
    forecast = arma.forecast(series = df.used[-1], nstart = 920, nstep = 4, marima = arma_1_1_ind)
#     
#     
#     data_test = c(
#       Cnsmr_test=sum((df$Cnsmr[1078:1081] - forecast$forecasts[1,921:924])^2),
#       Manuf_test=sum((df$Manuf[1078:1081] - forecast$forecasts[2,921:924])^2)/4,
#       HiTec_test=sum((df$HiTec[1078:1081] - forecast$forecasts[3,921:924])^2)/4,
#       Hlth_test=sum((df$Hlth[1078:1081] - forecast$forecasts[4,921:924])^2)/4,
#       Other_test=sum((df$Other[1078:1081] - forecast$forecasts[5,921:924])^2)/4
#     )
#     
#     data_train = c(
#       Cnsmr_train=sum(forecast$residuals[1,]^2) / 920,
#       Manuf_train=sum(forecast$residuals[2,]^2) / 920,
#       HiTec_train=sum(forecast$residuals[3,]^2) / 920,
#       Hlth_train =sum(forecast$residuals[4,]^2) / 920,
#       Other_train=sum(forecast$residuals[5,]^2) / 920
#     )
#     
#     data = c(ar=ar, ma=ma, data_test, data_train, sqe_train=sum(data_train), sqe_test=sum(data_test))
#     result_data = rbind(result_data, data)
#     print(data)
#   }
# }


# png("ex_4_4_residuals.png", width=20, height=10, units="cm", res=200)
layout(matrix(c(1,2,3,4,0,5), 2, 3, byrow = T))
for (idx in c(1:5)) { 
  hist(forecast$residuals[idx,-c(921:924)], xlab="Residuals", main=columns[idx])
}
    
    
round(result_data, 3)

array_to_LaTeX(round(arma_1_1_ind$Constant, 3))

par(mfrow=c(1,1))
acf(df[158:1081,c(-1,-7,-8, -9)], lag.max = 10, frame=F)

for (index in c(1:5)) {
  
  png(paste("ex_4_6_", columns[index] ,".png", sep = ""), width=20, height=10, units="cm", res=300)
  pred.int <- forecast$forecasts[index,921:924] + cbind(rep(0, 4), -1, 1)*qnorm(0.975)*sqrt(forecast$pred.var[index,index,])
  par(mfrow=c(1,1), mai = c(1,1, 0.4, 0.1))
  
  plot(df.used[,index + 1], 
       type="p", 
       ylab="Return", 
       xlab="Year",
       xaxt="n",
       main=paste("Series:", columns[index]), 
       ylim = c(min(pred.int[,2] * 1.1),max(min(pred.int[,3]*1.1))), 
       xlim=c(877,925), 
       frame.plot = F, 
       bty="n")
  lines(forecast$forecasts[index,])
  matlines(921:924, pred.int, lty=c(1,2,2), col=2, lwd=2 )
  lines(921:924,df[1078:1081,index + 1], type="p", col="red")
  marks_l = c(seq(2013,2017,1))
  marks = c(seq(877,925,12))
  axis(1,at=marks,labels=marks_l)
  legend(877, 10, c("Train data", "Test data", "1-step predicitons", "Prediction", "95% Prediction Interval"),
         col = c(1,2,1,2,2), 
         pch=c(1,1,NA,NA,NA), 
         lty=c(NA,NA,1,1,2),
         horiz = F,
         cex=0.75,
         inset=2)
  dev.off()
}

for (index in c(1:5)) {
  pred.int <- cbind(rep(1, 4))*qnorm(0.975)*sqrt(forecast$pred.var[index,index,])
  pred.int <- cbind(forecast$forecasts[index,921:924], pred.int)
  print(round(pred.int,3))
}


png("ex_4_5_residuals.png", width=20, height=30, units="cm", res=300)
layout(matrix(c(1, 1, 2, 3, 3, 4, 5, 5, 6, 7, 7, 8, 9, 9, 10), 5, 3, byrow = T))
for (index in c(1:5)) {
  plot(forecast$residuals[index,-c(921:924)], type="l", ylab = "Error", xlab = "Time", main=paste("Residuals of", columns[index]), bty="n", frame.plot=F)
  hist(forecast$residuals[index,-c(921:924)], xlab = "Error", main = paste("Residual distrubution of", col))
}
  dev.off()







