n = 1000
C <- matrix(rnorm(n^2), n, n)
C[abs(C) < 0.5] <-  0
C <- C %*% t(C)
C <- C %*% t(C)
phi = 0.95
C = inla.as.sparse(C * exp(mean(log(diag(inla.qinv(C))))))

tau.v = 1
tau.u = 100
v = inla.qsample(1, Q=C)
u = v + rnorm(n,  sd = 1/sqrt(tau.u))

y = u
r = (inla(y ~ -1 + f(idx,  model="generic2", Cmatrix=C,
                     hyper = list(
                         theta1 = list(initial = log(tau.v), param = c(1, 1/tau.v)),
                         theta2 = list(initial = log(tau.u), param = c(1, 1/tau.u)))), 
          family = "gaussian",
          control.family = list(hyper = list(prec = list(initial = 10,  fixed=TRUE))), 
          data = data.frame(y, idx=1:n),
          verbose=TRUE, keep = T))
