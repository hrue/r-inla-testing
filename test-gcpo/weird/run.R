library(mvtnorm)
library(INLA)
inla.setOption(num.threads = "1:1", smtp = "taucs", inla.call = INLA:::inla.call.builtin())
inla.setOption(num.threads = "1:1", smtp = "taucs", inla.call = "inla.mkl.work")

rho <- 0.80
C <- solve(toeplitz(rho^(0:(n-1))))
C <- 0.5 * (C + t(C))
s <- 5000
y <- scale(as.vector(rmvnorm(1, sigma = solve(C)))) + rnorm(n, sd = s)

Q <- C + (1/s^2) * diag(n)
S <- solve(Q)
R <- diag(1/sqrt(diag(S)))
Cor <- R %*% S %*% R
round(Cor, dig = 3)

r <-  inla(y ~ -1 +
               f(idx, model = "ar1", cyclic = !TRUE,
                 hyper = list(prec = list(initial = 0, fixed = TRUE),
                              rho = list(initial = inla.models()$latent$ar1$hyper$theta2$to.theta(rho), fixed = TRUE))), 
##           f(idx, model = "generic", Cmatrix = C, hyper = list(prec = list(initial = 0, fixed = TRUE))), 
           data = list(y = y, idx = 1:n), 
           family = "normal",
           control.family = list(
               list(hyper = list(prec = list(initial = log(1/s^2), fixed = TRUE)))), 
           control.compute = list(config = TRUE), 
           control.inla = list(int.strategy = "eb", stencil = 9), 
           verbose = TRUE)
g <- inla.group.cv(r, num.level.sets = 5, verbose = T)
g$groups
round(Cor[n, ], dig = 3)
