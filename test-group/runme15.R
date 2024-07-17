n = 10^4
p = 5
pacf = runif(p)
phi = inla.ar.pacf2phi(pacf)
y = arima.sim(n, model = list(ar = phi))
y = scale(y)
idx = 1:n
g = 1:n
one = rep(1, n)

inla.setOption(num.threads=1, inla.call = "inla.mkl.work", smtp = "taucs")

r = (inla(y ~ -1 + f(idx, model="ar1",
                     hyper = list(
                         prec = list(
                             prior = "pc.prec",
                             param = c(3, 0.01)),
                         rho = list(
                             prior = "pc.cor0",
                             param = c(0.5, 0.5)))), 
          control.predictor = list(hyper = list(prec = list(initial = 16))), 
          control.family = list(hyper =
              list(prec = list(
                       initial = 12,
                       fixed = TRUE))),
          data = data.frame(y, idx, g, one)))
rg <- inla.group.cv(r, num.level.sets = 100, verbose = T)
