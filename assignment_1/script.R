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


df[1:4,]

###############
#
# Exercise 1.2
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

( x_2 = cbind(1, c(-1:0)) )
( y_2 = data.matrix(train_df[1:2, "nh"]))
( F_2 = t(x_2) %*% x_2 )
( h_2 = t(x_2) %*% y_2)  

( beta_hat_2 = solve(F_2) %*% h_2)

c(1,1) %*% beta_hat_2

y_hats = cbind(1, c(df[1:2, "time"])) %*% beta_hat_3

( epsilon = cbind(y_hats - df[1:3, "nh"]) )

(t(epsilon) %*% epsilon) / 1

y_hats = cbind(1, c(df[df$train_test == "train", "time"])) %*% beta_hat_164

epsilon = cbind(y_hats - df[df$train_test == "train", "nh"])

(t(epsilon) %*% epsilon) / 162

cbind(1, c((-1 * (3 - 1)):0))

t(c(df[1, c("nh", "year")]))

run = function(alpha) {
  prediction_data = rbind(c(df[1, "year"], df[1, "nh"], NA, NA, NA, NA))
  F_n = cbind(c(0,0), c(0,0))
  h_n = cbind(c(0, 0))
  
  L = cbind(c(1, 1), c(0, 1))
  
  # Saving sigma_hat_sq outside the iteration, since the last sigma_hat_sq
  # will be used during the forecast of the test data.
  sigma_hat_sq = 0
  
  # Observation idx starts @ zero since most calcualtions see the 
  # frist datapoint as 0
  for (obersvation_idx in 0:(nrow(train_df) - 1)) {
    
    degrees_of_freedom = -1 + obersvation_idx
    
    new_insert_vector = c(1, -1 * obersvation_idx)
    row = train_df[(obersvation_idx + 1),]
    F_n = F_n + new_insert_vector %*% t(new_insert_vector)
    h_n = solve(L) %*% h_n + c(1, 0) * row$nh
    
    # Predictions will be done starting with the third observation, 
    # due to multiple reasons, one being the degrees of freedom 
    # are equal to 0 during the second observation.
    if (obersvation_idx > 1) {
      beta_hat_n = solve(F_n) %*% h_n
      y_hat = c(1, 1) %*% beta_hat_n 
      
      curr_df = df[1:(obersvation_idx + 1), ]
      
      y_hats = cbind(1, c((-1 * (obersvation_idx)):0)) %*% beta_hat_n
      epsilon = cbind(y_hats - curr_df[curr_df$train_test == "train", "nh"])
      
      sse = (t(epsilon) %*% epsilon)
      sigma_hat_sq = sse / degrees_of_freedom 
      
      ## Variance prediction error
      var_e_pred = sigma_hat_sq * (1 + t(c(1, 1)) %*% solve(F_n) %*% c(1, 1))
      
      upper_bound = y_hat + qt(1-(alpha/2), degrees_of_freedom) * sqrt(var_e_pred)
      lower_bound = y_hat - qt(1-(alpha/2), degrees_of_freedom) * sqrt(var_e_pred)
      
      # Adding + 1 here since we do a 1-step prediction with the 
      # data which was observed until the given point in time
      
      prediction_data = rbind(prediction_data, c(as.numeric(row$year) + 1, as.numeric(df[obersvation_idx + 2, "nh"]), as.numeric(y_hat), lower_bound, upper_bound, var_e_pred))
    } else {
      prediction_data = rbind(prediction_data, c(as.numeric(row$year) + 1, as.numeric(df[obersvation_idx + 2, "nh"]), NA, NA, NA, NA))
    }
  }
  
  # Starting at 2015 here (second test value), since the value for 2014 was
  # already predicted in the iteration before!
  for (test_row_idx in 2:length(test_df)) {
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
head(df)
prediction_data$train_test = "train"
prediction_data[165:169,"train_test"] = "test"
tail(prediction_data)

ggplot(data=prediction_data, mapping=aes(year, nh, color=train_test)) +
  geom_errorbar(mapping=aes(year, ymin=lower_bound, ymax=upper_bound, color="95% prediction interval"), alpha=.5, show.legend = TRUE) +
  geom_point(alpha=.5) +
  geom_point(data=prediction_data, mapping=aes(year, prediction, color=train_test), shape=4, na.rm = TRUE) +
  #lims(y=c(-1.5,1.5)) +
  scale_color_manual(values=c("orange", "#E26860", "#53B0B5")) +
  labs(x = "Year", y = "Temp. anomalie in K", col="Type of data", title = "Temperature anomalie predcition with gloabl trend model") +
  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), legend.direction = "horizontal", legend.position = c(0.5,0.1),) + 
  scale_x_continuous(breaks=seq(1800,2020,10))

ggsave("exercise_2_prediction_overview.png", width=18, height = 9, units = "cm", dpi=500)

tail(prediction_data)

##########
#
# Exercise 1.3
#
##########
  
local_trend_model = function(alpha, lambda, return_sse) {
  prediction_data = rbind(c(df[1, "year"], df[1, "nh"], NA, NA, NA, NA))
  F_n = cbind(c(0,0), c(0,0))
  h_n = cbind(c(0, 0))
  
  L = cbind(c(1, 1), c(0, 1))
  
  total_sse = c()
  sgima_hat_sq = 0
  for (obersvation_idx in 0:(nrow(train_df) - 1)) {
    
    degrees_of_freedom = -1 + obersvation_idx
    new_insert_vector = c(1, -1 * obersvation_idx)
    row = train_df[(obersvation_idx + 1),]
    F_n = F_n + (lambda^obersvation_idx * (new_insert_vector %*% t(new_insert_vector)))
    h_n = (lambda * solve(L, h_n)) + c(1, 0) * row$nh
    
    
    if (obersvation_idx > 3) {
      beta_hat_n = solve(F_n) %*% h_n
      y_hat = c(1, 1) %*% beta_hat_n 
      
      curr_df = df[1:(obersvation_idx + 1), ]
      
      y_hats = cbind(1, c((obersvation_idx):0)) %*% beta_hat_n
      epsilon = cbind(y_hats - curr_df$nh)
      big_sigma = diag((lambda^c((obersvation_idx):0)))
      
      # print(epsilon)
      # print(big_sigma)
      sse = (t(epsilon) %*% big_sigma %*% epsilon)
      sse_no_sigma = (t(epsilon) %*% epsilon)
      
      capital_T = sum(lambda^c(0:(obersvation_idx)))
      
      sigma_hat_sq = sse / (capital_T - 2)  
      inner_part = t(c(1, 1)) %*% solve(F_n) %*% c(1, 1)
      var_e_pred = sigma_hat_sq * (1 + inner_part)
      
      # collecting the one_step prediction errors
      if (obersvation_idx > 3) {
        total_sse = cbind(total_sse, c(y_hat - df[(obersvation_idx + 2),"nh"]))
      }
      upper_bound = y_hat + qt(1-(alpha/2), degrees_of_freedom) * sqrt(var_e_pred)
      lower_bound = y_hat - qt(1-(alpha/2), degrees_of_freedom) * sqrt(var_e_pred)
      
      prediction_data = rbind(prediction_data, c(as.numeric(row$year) + 1, df[(obersvation_idx + 2), "nh"], as.numeric(y_hat), lower_bound, upper_bound, var_e_pred))
    } else {
      prediction_data = rbind(prediction_data, c(as.numeric(row$year) + 1, df[(obersvation_idx + 2), "nh"], NA, NA, NA, NA))
    }
  }
  
  for (test_row_idx in 2:length(test_df)) {
    row = test_df[test_row_idx,]
    forecast_vec = c(1, row$time)
    
    y_hat = forecast_vec %*% beta_hat_n
    
    ## Variance prediction error
    var_e_pred = sigma_hat_sq * (1 + t(forecast_vec) %*% solve(F_n) %*% forecast_vec)
    
    upper_bound = y_hat - qt(alpha/2, degrees_of_freedom) * sqrt(var_e_pred)
    lower_bound = y_hat + qt(alpha/2, degrees_of_freedom) * sqrt(var_e_pred)
    prediction_data = rbind(prediction_data, c(as.numeric(row$year), as.numeric(row$nh), as.numeric(y_hat), lower_bound, upper_bound, var_e_pred))
  }
  
  if (return_sse) {
    return(t(total_sse))
  } else {
    return(prediction_data)
  }
}
prediction_data = data.frame(local_trend_model(alpha=.05, lambda=.8, return_sse=FALSE))
colnames(prediction_data) = c("year", "nh", "prediction", "lower_bound", "upper_bound", "var_e")
prediction_data$train_test = "train"
prediction_data[165:169,"train_test"] = "test"

tail(prediction_data, 10)



ggplot(data=prediction_data, mapping=aes(year, nh, color=train_test)) +
  geom_errorbar(mapping = aes(year, ymax=upper_bound, ymin=lower_bound, color="95 % prediction interval"), alpha=.5) +
  geom_point(alpha=.5) +
  # lims(y=c(-2.5,2.5)) +
  geom_point(mapping=aes(year, prediction), shape=4, na.rm = TRUE) +
  scale_color_manual(values=c("orange", "#E26860", "#53B0B5")) +
  labs(x = "Year", y = "Temp. anomalie in K", col="Type of data", title = "Temperature anomalie predcition with local trend model (lambda = .8)") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), legend.direction = "horizontal", legend.position = c(0.5,0.1),) + 
  scale_x_continuous(breaks=seq(1800,2020,10))

ggsave("exercise_3_prediction_overview.png", width=18, height = 9, units = "cm", dpi=500)

var(local_trend_model(alpha=.05, lambda=.95,return_sse=TRUE))
var(local_trend_model(alpha=.05, lambda=.8,return_sse=TRUE))
var(local_trend_model(alpha=.05, lambda=.9,return_sse=TRUE))


##########
#
# Exercise 1.4
#
##########

sse_data = c()

resolution = 100

for (i in c((8.0 * resolution):(9.0 * resolution))/(10 * resolution)) {
  sse_data = rbind(sse_data, c(i,var(local_trend_model(alpha=.05,lambda=i,return_sse=TRUE))))
}
sse_data = data.frame(sse_data)
colnames(sse_data) = c("lambda", "variance_of_residuals")

sse_data$min_value = FALSE
sse_data[which.min(sse_data$variance_of_residuals), "min_value"] = TRUE

ggplot(data = sse_data, mapping=aes(lambda, variance_of_residuals, color=min_value)) +
  geom_point() +
  labs(x = "lambda", y = "Variance of all 1-step predictions", col="Minimum data", title = "Minimizing the 1-step prediction variance")

ggsave("exercise_4_minimizing_residual_error.png", width=18, height = 9, units = "cm", dpi=500)

prediction_data = data.frame(local_trend_model(alpha=.05, lambda=sse_data[which.min(sse_data$variance_of_residuals), "lambda"], return_sse=FALSE))
colnames(prediction_data) = c("year", "nh", "prediction", "lower_bound", "upper_bound", "var_e")
prediction_data$train_test = "train"
prediction_data[165:169,"train_test"] = "test"

ggplot(data=prediction_data, mapping=aes(year, nh, color=train_test)) +
  geom_errorbar(mapping = aes(year, ymax=upper_bound, ymin=lower_bound, color="95 % prediction interval"), alpha=.5) +
  geom_point(alpha=.5) +
  # lims(y=c(-2.5,2.5)) +
  geom_point(mapping=aes(year, prediction), shape=4, na.rm = TRUE) +
  scale_color_manual(values=c("orange", "#E26860", "#53B0B5")) +
  labs(x = "Year", y = "Temp. anomalie in K", col="Type of data", title = "Temperature anomalie predcition with local trend model (lambda = .848)") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), legend.direction = "horizontal", legend.position = c(0.5,0.1),) + 
  scale_x_continuous(breaks=seq(1800,2020,10))

ggsave("exercise_4_prediction_overview_lambda_848.png", width=18, height = 9, units = "cm", dpi=500)
