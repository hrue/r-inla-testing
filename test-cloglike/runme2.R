INLA:::inla.my.update(b = T)

## first try: do not include the normalizing constant 
n <- 55
y <- rpois(n, exp(1))
Y <- inla.mdata(cbind(y))

cloglike <- inla.cloglike.define(model = "inla_cloglike_poisson", shlib = "cloglike-demo-poisson.so")
rr <- inla(y ~ 1, data = list(y = Y[, 1]), family = "poisson")
r <- inla(Y ~ 1, data = list(Y = Y), family = "cloglike", control.family = list(cloglike = cloglike))
## we add it back here
(r$mlik - sum(log(factorial(y)))) - rr$mlik
max(r$mode$x - rr$mode$x)

## section try: precompute the normalizing constant and add it as the 2nd column in the data
Y <- inla.mdata(cbind(y, lfactorial(y)))
cloglike <- inla.cloglike.define(model = "inla_cloglike_poisson", shlib = "cloglike-demo-poisson.so")
r <- inla(Y ~ 1, data = list(Y = Y), family = "cloglike", control.family = list(cloglike = cloglike))
r$mlik - rr$mlik
max(r$mode$x - rr$mode$x)

## use the cache-version, see the C-code
Y <- inla.mdata(cbind(y))
cloglike <- inla.cloglike.define(model = "inla_cloglike_poisson_cache", shlib = "cloglike-demo-poisson.so")
r <- inla(Y ~ 1, data = list(Y = Y), family = "cloglike", control.family = list(cloglike = cloglike))
r$mlik - rr$mlik
max(r$mode$x - rr$mode$x)
