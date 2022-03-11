library(forecast)
setwd("workspace/time_series_analysis/assignment_2")
arima_season_simulate = function(p, d, q, P, D, Q, 
                                 Period, years, 
                                 input) {
  set.seed(123)
  
  n <- 12 * years
  values = ts(rnorm(n),freq=Period)
  
  
  model <- Arima(values, order=c(p,d,q), seasonal=c(P,D,Q), fixed=input)
  print(model)
  Y <- simulate(model, nsim=n)
  Y = ts(Y, freq=12)
  # print(Y)
  # par(mar=c(1,1,1,1))
  layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))
  
  max_value = max(max(abs(Y)), max(abs(values)))
  
  ts.plot(Y, main="Simulation", xlab="Year")
  acf(Y,main="ACF", lag.max = Period * 4, xlab="Lag (Year)")
  pacf(Y,main="PACF", lag.max = Period * 4, xlab="Lag (Year)")
}

res = 200
height = 15
width = 20


png("arima_1_0_0_x_0_0_0.png", width=width, height=height, units="cm", res=res)
arima_season_simulate(1, 0, 0, 
                      0, 0, 0, 
                      12, 50,
                      c(-.8, NA))
dev.off()


png("arima_0_0_0_x_1_0_0.png", width=width, height=height, units="cm", res=res)
arima_season_simulate(0, 0, 0, 
                      1, 0, 0, 
                      12, 50,
                      c(.8, NA))
dev.off()

png("arima_1_0_0_x_0_0_1.png", width=width, height=height, units="cm", res=res)
arima_season_simulate(1, 0, 0, 
                      0, 0, 1, 
                      12, 50,
                      c(-.9, .7, NA))
dev.off()

png("arima_1_0_0_x_1_0_0.png", width=width, height=height, units="cm", res=res)
arima_season_simulate(1, 0, 0, 
                      1, 0, 0, 
                      12, 50,
                      c(.6, .8, NA))
dev.off()

png("arima_0_0_1_x_0_0_1.png", width=width, height=height, units="cm", res=res)
arima_season_simulate(0, 0, 1, 
                      0, 0, 1, 
                      12, 50,
                      c(-.4, .8, NA))
dev.off()

png("arima_0_0_1_x_1_0_0.png", width=width, height=height, units="cm", res=res)
arima_season_simulate(0, 0, 1, 
                      1, 0, 0, 
                      12, 50,
                      c(.4, -.7, NA))
dev.off()

