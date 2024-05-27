library(INLA)
inla.setOption(num.threads = "1:1", smtp = "taucs", inla.call = INLA:::inla.call.builtin())
inla.setOption(num.threads = "1:1", smtp = "band", inla.call = "inla.mkl.work")
n <- 10
s <- 0.1
y <- scale(arima.sim(n, model = list(ar = 0.9))) + rnorm(n, sd = s)
r <-  inla(y ~ -1 +
               f(idx, model = "ar1", values = 1:n,
                 hyper = list(prec = list(initial = 0, fixed = TRUE),
                              rho = list(initial = 3, fixed = TRUE))), 
           data = list(y = y, idx = 1:n), 
           family = "normal",
           control.family = list(list(hyper = list(prec = list(initial = log(1/s^2), fixed = TRUE)))),
           control.compute = list(config = TRUE), 
           control.inla = list(int.strategy = "eb", stencil = 5), 
           verbose = TRUE)
g <- inla.group.cv(r, num.level.sets = 2, verbose = T)
g$groups
