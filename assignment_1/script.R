library(ggplot2)
df = read.csv(file = "~/workspace/time_series_analysis/assignment_1/A1_annual.txt", header = TRUE, sep = "\t")
df$train_test = "train"
head(df)
total_records = length(df$train_test)
df$train_test[(total_records - 4):total_records] = "test"
df$time = c(-163:5)

ggplot(data=df, mapping=aes(year, nh, color=train_test)) + 
  geom_point() +
  scale_color_discrete("Type of data") +
  labs(x = "Year", y = "Temp. anomalie in K", title = "Tempertaure anomalie in the northern hemisphare") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), legend.direction = "horizontal", legend.position = c(0.2,0.9)) + 
  scale_x_continuous(breaks=seq(1800,2020,10))

ggsave("exercise_1_data_overview.png", width=18, height = 9, units = "cm", dpi=300)


  ###############
#
# Exercise 2
#
###############

train_df = df[df$train_test == "train",]
( test_df = df[df$train_test == "test",] )
train_df$time = c(-1 * (length(train_df$year) - 1):0)

tail(train_df)

( x_164 = cbind(1, data.matrix(train_df[,c("time")])) )
( y_164 = data.matrix(train_df[, "nh"]))
( F_164 = t(x_164) %*% x_164 )
( h_164 = t(x_164) %*% y_164)  

( beta_hat_164 = solve(F_164) %*% h_164)

( x_3 = cbind(1, data.matrix(train_df[1:3,c("time")])) )
( y_3 = data.matrix(train_df[1:3, "nh"]))
( F_3 = t(x_3) %*% x_3 )
( h_3 = t(x_3) %*% y_3)  

( beta_hat_3 = solve(F_3) %*% h_3)

y_hats = cbind(1, c(df[1:3, "time"])) %*% beta_hat_3

( epsilon = cbind(y_hats - df[1:3, "nh"]) )

(t(epsilon) %*% epsilon) / 1

y_hats = cbind(1, c(df[df$train_test == "train", "time"])) %*% beta_hat_164

epsilon = cbind(y_hats - df[df$train_test == "train", "nh"])

(t(epsilon) %*% epsilon) / 162

cbind(1, c((-1 * (3 - 1)):0))

run = function(alpha) {
  prediction_data = c()
  F_n = cbind(c(0,0), c(0,0))
  h_n = cbind(c(0, 0))
  
  L = cbind(c(1, 1), c(0, 1))
  
  # Saving sigma_hat_sq outside the iteration, since the last sigma_hat_sq
  # will be used during the forecast of the test data.
  sigma_hat_sq = 0
  
  for (obersvation_idx in 0:(nrow(train_df) - 1)) {
    
    degrees_of_freedom = -1 + obersvation_idx
    
    new_insert_vector = c(1, -1 * obersvation_idx)
    row = train_df[(obersvation_idx + 1),]
    F_n = F_n + new_insert_vector %*% t(new_insert_vector)
    h_n = solve(L) %*% h_n + c(1, 0) * row$nh
    
    if (obersvation_idx > 1) {
      beta_hat_n = solve(F_n) %*% h_n
      y_hat = c(1, 1) %*% beta_hat_n 
      
      curr_df = df[1:obersvation_idx, ]
      
      y_hats = cbind(1, c((-1 * (obersvation_idx - 1)):0)) %*% beta_hat_n
      epsilon = cbind(y_hats - curr_df[curr_df$train_test == "train", "nh"])
      
      sse = (t(epsilon) %*% epsilon)
      sigma_hat_sq = sse / degrees_of_freedom 
      
      ## Variance prediction error
      var_e_pred = sigma_hat_sq * (1 + t(c(1, 1)) %*% solve(F_n) %*% c(1, 1))
      
      upper_bound = y_hat + qt(1-(alpha/2), degrees_of_freedom) * sqrt(var_e_pred)
      lower_bound = y_hat - qt(1-(alpha/2), degrees_of_freedom) * sqrt(var_e_pred)
      
      # Adding + 1 here since we do a 1-step prediction with the 
      # data which was observed until the given point in time
      
      prediction_data = rbind(prediction_data, c(as.numeric(row$year) + 1, as.numeric(row$nh), as.numeric(y_hat), lower_bound, upper_bound, var_e_pred))
    } else {
      prediction_data = rbind(prediction_data, c(as.numeric(row$year) + 1, as.numeric(row$nh), 0, 0, 0, 0))
    }
  }
  
  for (test_row_idx in 1:length(test_df)) {
    row = test_df[test_row_idx,]
    forecast_vec = c(1, row$time)
    
    y_hat = forecast_vec %*% beta_hat_n
    
    ## Variance prediction error
    var_e_pred = sigma_hat_sq * (1 + t(forecast_vec) %*% solve(F_n) %*% forecast_vec)
    
    upper_bound = y_hat - qt(alpha/2, degrees_of_freedom) * sqrt(var_e_pred)
    lower_bound = y_hat + qt(alpha/2, degrees_of_freedom) * sqrt(var_e_pred)
    prediction_data = rbind(prediction_data, c(as.numeric(row$year), as.numeric(row$nh), as.numeric(y_hat), lower_bound, upper_bound, var_e_pred))
  }
  
  
  
  return(prediction_data)
}
prediction_data = data.frame(run(alpha=.05))
colnames(prediction_data) = c("year", "nh", "prediction", "lower_bound", "upper_bound", "var_e")
head(prediction_data)
prediction_data$train_test = "train"
prediction_data[165:169,"train_test"] = "test"
tail(prediction_data)

ggplot(data=prediction_data, mapping=aes(year, nh, color=train_test)) +
  geom_errorbar(mapping=aes(year, ymin=lower_bound, ymax=upper_bound, color="prediction"), alpha=.5, show.legend = TRUE) +
  geom_point() +
  geom_point(data=prediction_data, mapping=aes(year, prediction, color="prediction"), alpha=.5) +
  #lims(y=c(-1.5,1.5)) +
  scale_color_manual(values=c("orange", "#E26860", "#53B0B5")) +
  labs(x = "Year", y = "Temp. anomalie in K", col="Type of data", title = "Temperature anomalie predcition with gloabl trend model") +
  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), legend.direction = "horizontal", legend.position = c(0.5,0.1),) + 
  scale_x_continuous(breaks=seq(1800,2020,10))

ggsave("exercise_2_prediction_overview.png", width=18, height = 9, units = "cm", dpi=500)

tail(prediction_data)

##########
#
# Exercise 3
#
##########

diag(.8^c(-2:0))
  
local_trend_model = function(alpha, lambda, return_sse) {
  prediction_data = c()
  F_n = cbind(c(0,0), c(0,0))
  h_n = cbind(c(0, 0))
  
  L = cbind(c(1, 1), c(0, 1))
  
  total_sse = c()
  
  for (row_idx in 1:(nrow(train_df))) {
  # for (row_idx in 1:5) {
    
    degrees_of_freedom = -2 + row_idx
    new_insert_vector = c(1, -1 * (row_idx - 1))
    row = train_df[(row_idx),]
    F_n = F_n + (lambda^row_idx * (new_insert_vector %*% t(new_insert_vector)))
    h_n = (lambda * solve(L, h_n)) + c(1, 0) * row$nh
    
    
    if (row_idx > 2) {
      beta_hat_n = solve(F_n) %*% h_n
      y_hat_pred = c(1, 1) %*% beta_hat_n 
      
      curr_df = df[1:row_idx, ]
      
      y_hats = cbind(1, c((row_idx - 1):0)) %*% beta_hat_n
      epsilon = cbind(y_hats - curr_df$nh)
      big_sigma = diag((lambda^c((row_idx - 1):0)))
      #print(big_sigma)
      sse = (t(epsilon) %*% big_sigma %*% epsilon)
      sse_no_sigma = (t(epsilon) %*% epsilon)
      if (row_idx > 5) {
        total_sse = cbind(total_sse, c(y_hat_pred - df[row_idx,"nh"]))
      }
      # print(length(y_hats))
      
      capital_T = sum(lambda^c(0:(row_idx - 1)))
      
      sigma_hat_sq = sse / (capital_T - 2)  
      inner_part = t(c(1, 1)) %*% solve(F_n) %*% c(1, 1)
      var_e_pred = sigma_hat_sq * (1 + inner_part)
      
      upper_bound = y_hat_pred + qt(1-(alpha/2), degrees_of_freedom) * sqrt(var_e_pred)
      lower_bound = y_hat_pred - qt(1-(alpha/2), degrees_of_freedom) * sqrt(var_e_pred)
      
      prediction_data = rbind(prediction_data, c(-1 * row_idx, as.numeric(row$year), as.numeric(row$nh), as.numeric(y_hat_pred), lower_bound, upper_bound, var_e_pred))
    } else {
      prediction_data = rbind(prediction_data, c(-1 * row_idx, as.numeric(row$year), as.numeric(row$nh), 0, 0, 0, 0))
    }
  }
  if (return_sse) {
    return(t(total_sse))
  } else {
    return(prediction_data)
  }
}
prediction_data = data.frame(local_trend_model(alpha=.05, lambda=.8, return_sse=FALSE))
colnames(prediction_data) = c("row_idx", "year", "nh", "prediction", "lower_bound", "upper_bound", "var_e")
head(prediction_data)

ggplot(data=prediction_data, mapping=aes(year, nh)) +
  geom_point() +
  # lims(y=c(-2.5,2.5)) +
  geom_line(mapping=aes(year, prediction), na.rm = TRUE, color="red") +
  geom_errorbar(mapping = aes(year, ymax=upper_bound, ymin=lower_bound))

print()

local_trend_model_sse = function(lambda) {
  prediction_data = c()
  F_n = cbind(c(0,0), c(0,0))
  h_n = cbind(c(0, 0))
  
  L = cbind(c(1, 1), c(0, 1))
  
  global_sse = 0
  big_sigma = c()
  for (row_idx in 0:(nrow(train_df) - 1)) {
  # for (row_idx in 0:4) {
    sse = 0
    degrees_of_freedom = -1 + row_idx
    new_insert_vector = c(1, -1 * row_idx)
    row = train_df[(row_idx + 1),]
    F_n = F_n + (lambda^row_idx * (new_insert_vector %*% t(new_insert_vector)))
    h_n = (lambda * solve(L, h_n)) + c(1, 0) * row$nh
    
    
    if (row_idx > 4) {
      beta_hat_n = solve(F_n) %*% h_n
      y_hat_pred = c(1, 1) %*% beta_hat_n   
      
      curr_df = df[1:(row_idx + 1), ]
      
      y_hats = cbind(1, c((-1 * row_idx):0)) %*% beta_hat_n
      
      epsilon = cbind(abs(y_hats - curr_df$nh))
      # print(epsilon)
      big_sigma = diag(1/(lambda^c((-1 * row_idx):0)))

      # print(big_sigma)
      global_sse = global_sse + (t(epsilon) %*% big_sigma %*% epsilon)
      # global_sse = global_sse + (t(epsilon) %*% epsilon)
      # global_sse = global_sse + (y_hat_pred - df[row_idx + 1, "nh"])^2
    }
  }
  return(global_sse)
}
var(c(-1,2,3))

var(local_trend_model(alpha=.05, lambda=.95,return_sse=TRUE))
local_trend_model(alpha=.05, lambda=.85,return_sse=TRUE)
local_trend_model(alpha=.05, lambda=.9,return_sse=TRUE)
sse_data = c()

resolution = 100

for (i in c((7.5 * resolution):(9.0 * resolution))/(10 * resolution)) {
  sse_data = rbind(sse_data, c(i,var(local_trend_model(alpha=.05,lambda=i,return_sse=TRUE))))
}
plot(sse_data[,1], sse_data[,2])
min(sse_data[,2])
sse_data[which.min(sse_data[,2]),1]

(prediction_data$prediction - prediction_data$upper_bound - abs(prediction_data$prediction - prediction_data$lower_bound)) < 0.001

tail(prediction_data)
