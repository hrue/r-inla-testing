n = 200
m = 30
N = n+m
y = c(scale(cumsum(rnorm(N))))
y[(n+1):N] = NA

r = inla(y ~ 1 + f(idx, model = "ar1",
                    hyper = list(
                        rho = list(
                            prior = "pc.cor1",
                            param = c(0.9, 0.5)),
                        prec = list(
                            prior = "pc.prec",
                            param = c(1, 0.01)))), 
         control.inla = list(h = 0.01, int.strategy = "eb"), 
         data = data.frame(y, idx = 1:N),
         control.family = list(hyper = list(prec = list(initial = 12, fixed=TRUE))),
         control.predictor = list(compute=TRUE), 
         verbose=T)

rr = inla(y ~ -1 + f(idx, model = "rw1",
                     scale.model=TRUE,
                     constr=FALSE,
                     hyper = list(
                         prec = list(
                             prior = "pc.prec",
                             param = c(1, 0.01)))), 
         data = data.frame(y, idx = 1:N),
         control.inla = list(h = 0.01, int.strategy = "eb"), 
         control.family = list(hyper = list(prec = list(initial = 12, fixed=TRUE))),
         control.predictor = list(compute=TRUE), 
         verbose=T)

y.r = range(c(
    c(r$summary.linear.predictor$"0.025quant",
      r$summary.linear.predictor$"0.975quant"), 
    c(rr$summary.linear.predictor$"0.025quant",
      rr$summary.linear.predictor$"0.975quant")))

plot(r$summary.linear.predictor$"mean", type="l", ylim = y.r)
lines(r$summary.linear.predictor$"0.025quant")
lines(r$summary.linear.predictor$"0.975quant")

lines(rr$summary.linear.predictor$"mean", lwd=2, col = "blue")
lines(rr$summary.linear.predictor$"0.025quant", lwd=2, col = "blue")
lines(rr$summary.linear.predictor$"0.975quant", lwd=2, col = "blue")

