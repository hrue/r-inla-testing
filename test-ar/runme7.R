n = 300
p = 3
s <- 0.1
pacf = runif(p)
phi = inla.ar.pacf2phi(pacf)
y = arima.sim(n, model = list(ar = phi))
y = scale(y) + rnorm(n, sd = s)
idx = 1:n
param.phi = c(rep(0, p),  diag(p))
param.prec = c(1, 0.01)

inla.setOption(verbose = FALSE, num.threads = 4)

r = inla(y ~ -1 + f(idx, model='ar', order = p), 
         family = "gaussian", 
         control.family = list(initial = log(1/s^2), fixed=TRUE), 
         control.compute = list(internal.opt = FALSE), 
         control.expert = list(disable.gaussian.check = TRUE), 
         data = data.frame(y, idx))

r.retry = inla(y ~ -1 + f(idx, model='ar', order = p), 
         family = "gaussian", 
         control.family = list(initial = log(1/s^2), fixed=TRUE), 
         control.compute = list(internal.opt = FALSE), 
         data = data.frame(y, idx))

rr = inla(y ~ -1 + f(idx, model='ar', order = p),
          family = "gaussian", 
          control.family = list(initial = log(1/s^2), fixed=TRUE), 
          control.compute = list(internal.opt = FALSE), 
          data = data.frame(y, idx),
          inla.call = "inla.mkl.work",
          control.inla = list(tolerance = 1e-10), 
          control.expert = list(disable.gaussian.check = TRUE))

rr.retry = inla(y ~ -1 + f(idx, model='ar', order = p),
          family = "gaussian", 
          control.family = list(initial = log(1/s^2), fixed=TRUE), 
          control.compute = list(internal.opt = FALSE), 
          data = data.frame(y, idx),
          control.inla = list(tolerance = 1e-10), 
          inla.call = "inla.mkl.work")

r$mlik - r.retry$mlik
r$mlik - rr$mlik
max(abs(r$mode$theta - r.retry$mode$theta))
max(abs(r$mode$theta - rr$mode$theta))
max(abs(r$mode$x - r.retry$mode$x))
max(abs(rr$mode$x - rr.retry$mode$x))
max(abs(r$mode$x - rr$mode$x))
