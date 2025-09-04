INLA:::inla.my.update(b = T)

n <- 55
Y <- inla.mdata(rnorm(n))
cloglike <- inla.cloglike.define(model = "inla_cloglike_gaussian",
                                 shlib = "cloglike-demo-gaussian.so",
                                 debug = FALSE)

rr <- inla(y ~ 1, 
          data = data.frame(y = Y[, 1]),
          family = "gaussian",
          control.family = list(hyper = list(prec = list(param = c(1, 1)))))

r <- inla(Y ~ 1, 
          data = list(Y = Y),
          family = "cloglike",
          control.family = list(cloglike = cloglike))

r$mlik - rr$mlik
r$mode$theta - rr$mode$theta
max(r$mode$x - rr$mode$x)
