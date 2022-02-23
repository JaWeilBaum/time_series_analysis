y_6 = c(2, 2.5, 3.5, 3, 4, 3.5)
x_6 = cbind(1, -5:0)

F_6 = t(x_6) %*% x_6
h_6 = t(x_6) %*% y_6

th.hat_6 = solve(F_6, h_6)

th.hat_6
plot(0:5, y_6)

sig.hat_6_sq = (t(y - x_6%*%th.hat_6) %*% (y - x_6%*%th.hat_6)) / (6-2)
sqrt(sig.hat_6_sq)

F_7 = F_6 + c(1, -6) %*% t(c(1, -6))
h_7 = solve(cbind(c(1, 1), c(0, 1))) %*% h_6 + c(1, 0) * 3.5

th.hat_7 = solve(F_7, h_7)
