INLA:::inla.my.update(b = T)

inla.setOption(num.threads = "1:1")
n <- 10^5
m <- 10^2
idx <- sample(1:m, n, replace = TRUE)
y <- rpois(n, exp(1))
Y <- inla.mdata(cbind(y))

rr <- inla(y ~ 1 + f(idx), data = list(y = Y[, 1], idx = idx), family = "poisson")

Y <- inla.mdata(cbind(y, lfactorial(y)))
cloglike <- inla.cloglike.define(model = "inla_cloglike_poisson", shlib = "cloglike-demo-poisson.so")
r <- inla(Y ~ 1 + f(idx), data = list(Y = Y, idx = idx), family = "cloglike", control.family = list(cloglike = cloglike))

rr$cpu.used
r$cpu.used

