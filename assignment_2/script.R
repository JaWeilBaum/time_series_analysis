library(forecast)

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
  print(Y)
  layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))
  
  max_value = max(max(abs(Y)), max(abs(values)))
  
  ts.plot(Y, main="Simulation", xlab="Year")
  acf(Y,main="ACF", lag.max = Period * 7, xlab="Lag (Year)")
  pacf(Y,main="PACF", lag.max = Period * 7, xlab="Lag (Year)")
  dev.off()
}

arima_season_simulate(1, 0, 0, 
                      1, 0, 0, 
                      12, 75,
                      c(.6, .8, NA))

png("arima_1_0_0_x_1_0_0.png", width=18, height=10, units="cm", res=200)
