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
                      c(.8, NA))
dev.off()


png("arima_0_0_0_x_1_0_0.png", width=width, height=height, units="cm", res=res)
arima_season_simulate(0, 0, 0, 
                      1, 0, 0, 
                      12, 50,
                      c(-.8, NA))
dev.off()

png("arima_1_0_0_x_0_0_1.png", width=width, height=height, units="cm", res=res)
arima_season_simulate(1, 0, 0, 
                      0, 0, 1, 
                      12, 50,
                      c(.9, -.7, NA))
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





simulation = function(seeds) {
  layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))
  count = 0
  for (s in seeds) {
    set.seed(s)
    epsilon = rnorm(500)
    sum_epsilon = c()
    for (i in 1:length(epsilon)) {
      sum_epsilon = c(sum_epsilon, sum(epsilon[1:i]))
    }
    
    y_t = sum_epsilon + exp(2)
    if (count == 0) {
      plot(y_t, type = 'l', ylim=c(-35, 50), col=s, main="Realizations", ylab = "Value of Y_t", xlab="t")
    } else {
      lines(y_t, type = "l", col=s)
    }
    count = count + 1
  }

  count = 0
  for (s in seeds) {
    set.seed(s)
    epsilon = rnorm(500)
    sum_epsilon = c()
    for (i in 1:length(epsilon)) {
      sum_epsilon = c(sum_epsilon, sum(epsilon[1:i]))
    }
    
    y_t = sum_epsilon + exp(2)
    acf_values = acf(y_t, lag.max = 25, plot=FALSE)$acf
    if (count == 0) {
      plot(c(0:25), acf_values, type = 'l', col=s, main="ACF plot of all realizations", xlab="Lag", ylim=c(-.2, 1), ylab="ACF value")
    } else {
      lines(c(0:25), acf_values, type = "l", col=s)
    }
    count = count + 1
  }
  lines(cbind(c(-1:26), 0), col="black", lty=5)
  
  count = 0
  for (s in seeds) {
    set.seed(s)
    epsilon = rnorm(500)
    sum_epsilon = c()
    for (i in 1:length(epsilon)) {
      sum_epsilon = c(sum_epsilon, sum(epsilon[1:i]))
    }
    
    y_t = sum_epsilon + exp(2)
    pacf_values = pacf(y_t, , lag.max = 25, plot=FALSE)$acf
    if (count == 0) {
      plot(c(1:25), pacf_values, type = 'l', col=s, main="PACF plot of all realizations", xlab="Lag", ylab="PACF value")
    } else {
      lines(c(1:25), pacf_values, type = "l", col=s)
    }
    count = count + 1
  }
  lines(cbind(c(0:26), 0), col="black", lty=5)
  
  
}

png("random_walk_realisations.png", width=width, height=height, units="cm", res=res)
simulation(c(1:10))
dev.off()


