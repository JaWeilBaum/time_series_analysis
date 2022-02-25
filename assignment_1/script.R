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

( beta_hat_164 = solve(F_164, h_164))

run = function(alpha) {
  prediction_data = c()
  F_n = cbind(c(0,0), c(0,0))
  h_n = cbind(c(0, 0))
  
  L = cbind(c(1, 1), c(0, 1))
  
  for (row_idx in 0:(nrow(train_df) - 1)) {
    
    degrees_of_freedom = -1 + row_idx
    
    new_insert_vector = c(1, -1 * row_idx)
    row = train_df[(row_idx + 1),]
    F_n = F_n + new_insert_vector %*% t(new_insert_vector)
    h_n = solve(L) %*% h_n + c(1, 0) * row$nh
    
    if (row_idx > 1) {
      beta_hat_n = solve(F_n) %*% h_n
      y_hat_pred = c(1, 1) %*% beta_hat_n 
      
      sse = 0
      for (j in 1:(row_idx + 1)) {
        y_j = df$nh[j]
        y_hat_j = c(1, -1 * j) %*% beta_hat_n
        sse = sse + (y_j - y_hat_j)^2
      }
      
      sigma_hat_sq = sse / degrees_of_freedom 
      ## Variance prediction error
      inner_part = t(c(1, 1)) %*% solve(F_n) %*% c(1, 1)
      var_e_pred = sigma_hat_sq * (1 + inner_part)
      # conv_pred = sqrt(sigma_hat_sq * inner_part)
      
      ## Prediction interval 
      
      upper_bound = y_hat_pred + qt(1-(alpha/2), degrees_of_freedom) * sqrt(var_e_pred)
      lower_bound = y_hat_pred - qt(1-(alpha/2), degrees_of_freedom) * sqrt(var_e_pred)
      
      prediction_data = rbind(prediction_data, c(-1 * row_idx, as.numeric(row$year), as.numeric(row$nh), as.numeric(y_hat_pred), lower_bound, upper_bound, var_e_pred))
    } else {
      prediction_data = rbind(prediction_data, c(-1 * row_idx, as.numeric(row$year), as.numeric(row$nh), 0, 0, 0, 0))
    }
    # print(cat("pred_vector = ", prediction_vector, "y_hat_pred = ", y_hat_pred, "Interval [", upper_bound, ", ", lower_bound, "]"))
    # pred_data = rbind(pred_data, c(i, y_hat_pred, upper_bound, lower_bound, y_hat_pred + conv_pred, y_hat_pred - conv_pred))
  }
  
  
  for (test_row_idx in 1:length(test_df)) {
    row = test_df[test_row_idx,]
    forecast_vec = c(1, row$time)
    
    y_hat_pred = forecast_vec %*% beta_hat_n
    print(c(forecast_vec, y_hat_pred))
    sse = 0
    for (j in 1:164) {
      y_j = df$nh[j]
      y_hat_j = c(1, -1 * j) %*% beta_hat_n
      sse = sse + (y_j - y_hat_j)^2
    }
    
    sigma_hat_sq = sse / (164 - 2)
    ## Variance prediction error
    inner_part = t(forecast_vec) %*% solve(F_n) %*% forecast_vec
    var_e_pred = sigma_hat_sq * (1 + inner_part)
    
    upper_bound = y_hat_pred + qt(1-(alpha/2), degrees_of_freedom) * sqrt(var_e_pred)
    lower_bound = y_hat_pred - qt(1-(alpha/2), degrees_of_freedom) * sqrt(var_e_pred)
    prediction_data = rbind(prediction_data, c(as.numeric(row$time), as.numeric(row$year), as.numeric(row$nh), as.numeric(y_hat_pred), lower_bound, upper_bound, var_e_pred))
  }
  
  
  
  return(prediction_data)
}
prediction_data = data.frame(run(alpha=.05))
colnames(prediction_data) = c("row_idx", "year", "nh", "prediction", "lower_bound", "upper_bound", "var_e")

prediction_data$train_test = "train"
prediction_data[165:169,"train_test"] = "test"
tail(prediction_data)

ggplot(data=prediction_data, mapping=aes(year, nh, color=train_test)) +
  geom_point() +
  geom_point(data=prediction_data, mapping=aes(year, prediction, color="prediction")) +
  lims(y=c(-1.5,1.5)) +
  scale_color_manual(values=c("orange", "#E26860", "#53B0B5")) +
  labs(x = "Year", y = "Temp. anomalie in K", col="Type of data", title = "Temperature anomalie predcition with gloabl trend model") +
  geom_ribbon(mapping=aes(year, ymin=lower_bound, ymax=upper_bound, color=train_test), alpha=.2, show.legend = TRUE) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), legend.direction = "horizontal", legend.position = c(0.5,0.1),) + 
  scale_x_continuous(breaks=seq(1800,2020,10))

ggsave("exercise_2_prediction_overview.png", width=18, height = 9, units = "cm", dpi=300)

tail(prediction_data)

##########
#
# Exercise 3
#
##########

local_trend_model = function(alpha, lambda) {
  prediction_data = c()
  F_n = cbind(c(0,0), c(0,0))
  h_n = cbind(c(0, 0))
  
  L = cbind(c(1, 1), c(0, 1))
  
  for (row_idx in 0:(nrow(train_df) - 1)) {
    
    degrees_of_freedom = -2 + row_idx
    new_insert_vector = c(1, -1 * row_idx)
    row = train_df[(row_idx + 1),]
    F_n = F_n + (lambda^row_idx * (new_insert_vector %*% t(new_insert_vector)))
    h_n = (lambda * solve(L, h_n)) + c(1, 0) * row$nh
    
    if (row_idx > 2) {
      beta_hat_n = solve(F_n, h_n)
      y_hat_pred = c(1, 1) %*% beta_hat_n 
      
      sse = 0
      capital_T = 0
      for (j in 0:(row_idx)) {
        y_j = df$nh[j + 1]
        y_hat_j = c(1, -1 * j) %*% beta_hat_n
        sse = sse + (lambda^j * (y_j - y_hat_j)^2)
        capital_T = capital_T + lambda^j
      }
      
      sigma_hat_sq = sse / (capital_T - 2) 
      ## Variance prediction error
      inner_part = t(c(1, 1)) %*% solve(F_n) %*% c(1, 1)
      var_e_pred = sigma_hat_sq * (1 + inner_part)
      # conv_pred = sqrt(sigma_hat_sq * inner_part)
      
      ## Prediction interval 
      
      upper_bound = y_hat_pred + (qt(alpha/2, degrees_of_freedom) * sqrt(var_e_pred))
      lower_bound = y_hat_pred - (qt(alpha/2, degrees_of_freedom) * sqrt(var_e_pred))
      
      prediction_data = rbind(prediction_data, c(row_idx, row$nh, y_hat_pred, lower_bound, upper_bound, var_e_pred))
    } else {
      prediction_data = rbind(prediction_data, c(row_idx, row$nh, NA, NA, NA, NA))
    }
    
    
    
    # print(cat("pred_vector = ", prediction_vector, "y_hat_pred = ", y_hat_pred, "Interval [", upper_bound, ", ", lower_bound, "]"))
    # pred_data = rbind(pred_data, c(i, y_hat_pred, upper_bound, lower_bound, y_hat_pred + conv_pred, y_hat_pred - conv_pred))
  }
  return(prediction_data)
}
prediction_data = data.frame(local_trend_model(alpha=.05, lambda=.582))
colnames(prediction_data) = c("row_idx", "nh", "prediction", "lower_bound", "upper_bound", "var_e")


ggplot(data=prediction_data, mapping=aes(row_idx, nh)) +
  geom_point() +
  lims(y=c(-2,2)) +
  geom_line(mapping=aes(row_idx, prediction), na.rm = TRUE, color="red") +
  geom_line(mapping = aes(row_idx, lower_bound)) +
  geom_line(mapping = aes(row_idx, upper_bound))



local_trend_model_sse = function(lambda) {
  prediction_data = c()
  F_n = cbind(c(0,0), c(0,0))
  h_n = cbind(c(0, 0))
  
  L = cbind(c(1, 1), c(0, 1))
  
  for (row_idx in 0:(nrow(train_df) - 1)) {
    
    degrees_of_freedom = -2 + row_idx
    new_insert_vector = c(1, -1 * row_idx)
    row = train_df[(row_idx + 1),]
    F_n = F_n + (lambda^row_idx * (new_insert_vector %*% t(new_insert_vector)))
    h_n = (lambda * solve(L, h_n)) + c(1, 0) * row$nh
    
    if (row_idx > 2) {
      beta_hat_n = solve(F_n, h_n)
      y_hat_pred = c(1, 1) %*% beta_hat_n 
      
      sse = 0
      capital_T = 0
      for (j in 0:(row_idx)) {
        y_j = df$nh[j + 1]
        y_hat_j = c(1, -1 * j) %*% beta_hat_n
        sse = sse + (y_j - y_hat_j)^2
        capital_T = capital_T + lambda^j
      }
      
      sigma_hat_sq = sse / (capital_T - 2) 
      ## Variance prediction error
      inner_part = t(c(1, 1)) %*% solve(F_n) %*% c(1, 1)
      var_e_pred = sigma_hat_sq * (1 + inner_part)
      # conv_pred = sqrt(sigma_hat_sq * inner_part)
      
      ## Prediction interval 
      
      # upper_bound = y_hat_pred + qt(alpha/2, degrees_of_freedom) * sqrt(var_e_pred)
      # lower_bound = y_hat_pred - qt(alpha/2, degrees_of_freedom) * sqrt(var_e_pred)
      
      # prediction_data = rbind(prediction_data, c(row_idx, row$nh, y_hat_pred, lower_bound, upper_bound, var_e_pred))
    } else {
      # prediction_data = rbind(prediction_data, c(row_idx, row$nh, NA, NA, NA, NA))
    }
    
    
    
    # print(cat("pred_vector = ", prediction_vector, "y_hat_pred = ", y_hat_pred, "Interval [", upper_bound, ", ", lower_bound, "]"))
    # pred_data = rbind(pred_data, c(i, y_hat_pred, upper_bound, lower_bound, y_hat_pred + conv_pred, y_hat_pred - conv_pred))
  }
  return(sse)
}
sse_data = c()
for (i in c(20:90)/100) {
  sse_data = rbind(sse_data, c(i,local_trend_model_sse(i)))
}
plot(sse_data[,1], sse_data[,2])
which.min(sse_data[,2])
sse_data[which.min(sse_data[,2]),1]

(prediction_data$prediction - prediction_data$upper_bound - abs(prediction_data$prediction - prediction_data$lower_bound)) < 0.001

tail(prediction_data)
