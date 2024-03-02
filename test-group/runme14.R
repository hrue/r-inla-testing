n = 40000
p = 1
pacf = runif(p)
phi = inla.ar.pacf2phi(pacf)
y = arima.sim(n, model = list(ar = phi))
y = scale(y)
idx = 1:n
g = 1:n
one = rep(1, n)

inla.setOption(num.threads=4, inla.call = NULL, smtp = "taucs")
inla.setOption(num.threads=4, inla.call = "inla.mkl.work", smtp = "taucs")

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
          keep = TRUE,
          control.inla = list(tolerance = 1e-16), 
          data = data.frame(y, idx, g, one)))
##r = inla.rerun(r)

rr = (inla(y ~ -1 + f(one, model="iid",
                      hyper = list(
                          prec = list(
                              prior = "pc.prec",
                              param = c(3, 0.01))),
                      group = g,
                      control.group = list(
                          model = "ar1",
                          hyper = list(
                              rho = list(
                                  prior = "pc.cor0",
                                  param = c(0.5, 0.5))))), 
           control.predictor = list(hyper = list(prec = list(initial = 16))), 
           control.family = list(hyper =
               list(prec = list(
                        initial = 12,
                        fixed = TRUE))),
           keep=TRUE, 
           control.inla = list(tolerance = 1e-16), 
           data = data.frame(y, idx, g, one)))
##rr = inla.rerun(rr)

print(cbind(r=r$internal.summary.hyperpar$mode, rr=rr$internal.summary.hyperpar$mode,
            diff=r$internal.summary.hyperpar$mode - rr$internal.summary.hyperpar$mode))
