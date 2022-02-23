x = cbind(1, c(4,4,3.5,4,2,2.5,1.5))
y = c(1, .5, 2, 2, 3.5, 3, 4)

# Exercis 3.1.1
beta = solve(t(x)%*%x)%*%t(x)%*%y

beta

# Exercise 3.1.2

x_8 = c(1, .5)
y_8 = x_8 %*% beta

y_8

( sigma_sq = t(y - x%*%beta) %*% (y - x%*%beta) / (7 - 2) )
sqrt(sigma_sq)

( var_e_8 = sigma_sq * (1 + c(1,.5) %*% solve(t(x) %*% x) %*% c(1, .5)) )

sqrt(var_e_8)

y_8 - qt(0.05, 7-2) * sqrt(var_e_8)
y_8 + qt(0.05, 7-2) * sqrt(var_e_8)

# Exercise 3.1.3

new_x = cbind(x[,3], x[,2])

plot(x[,3], x[,2])

# Ohne x Werte


# Exercise 3.4.1

x = cbind(1, c(0,0,0,0,1,1,1,1), 1:8)
x = cbind(c(1,1,1,1,0,0,0,0), c(0,0,0,0,1,1,1,1), 1:8)
y = log(c(4.4, 3.4, 3.3, 2.5, 7.3, 4.9, 4.8, 4.4))

plot(x[,3], y)

beta_2 = solve(t(x)%*%x)%*%t(x)%*%y
beta_2

x_9 = c(1, 0, 9)
x_9%*%beta_2

