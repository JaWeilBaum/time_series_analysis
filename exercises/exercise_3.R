y_t = c(1, .5, 2, 2, 3.5, 3, 4)
x = cbind(1, c(-6:0))
lambda = .9
( L = cbind(c(1, 1), c(0, 1)) )

sigma_diag = 1/(lambda^c(6:0))
sigma_diag
sig = diag(sigma_diag)

( F_7 = t(x) %*% solve(sig) %*% x )
( h_7 = t(x) %*% solve(sig) %*% y_t)
      
( beta_7 = solve(F_7, h_7) )

( y_hat_8 = c(1, 1) %*% beta_7) 

( F_8 = F_7 + (lambda^7 * c(1, -7) %*% t(c(1, -7))) )
( h_8 = lambda * solve(L) %*% h_7 + c(1, 0)*5.0)

( beta_8 = solve(F_8, h_8))

( y_hat_9 = c(1, 1) %*% beta_8)
