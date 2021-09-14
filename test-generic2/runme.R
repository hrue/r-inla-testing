n = 5000
phi = 0.95
C = inla.as.sparse(toeplitz(c(1+phi^2,  -phi,  rep(0, n-3),  -phi)))
C = inla.as.sparse(C * inla.qinv(C)[n %/% 2,  n %/% 2])
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
          verbose=TRUE))

rr = (inla(y ~ -1 + f(idx,  model="generic0", Cmatrix=C,
                      hyper = list(theta = list(initial = log(tau.v), param = c(1, 1/tau.v)))), 
           family = "gaussian",
           control.family = list(hyper = list(theta = list(initial = log(tau.u), param = c(1, 1/tau.u)))), 
           data = data.frame(y, idx=1:n),
           control.compute = list(config=TRUE), 
           verbose=TRUE))

xx = inla.hyperpar.sample(n*100,  rr)
rr.tau.v = xx[, 2]
rr.tau.u = xx[, 1]
hist(1/rr.tau.v/(1/rr.tau.v + 1/rr.tau.u),  n = 100,  prob=TRUE, main = "histogram of h2")
lines(inla.smarginal(r$marginals.hyperpar$h2))

