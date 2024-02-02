n <- 100
x <- rnorm(n)

eta <- 1 + 0.3 * x
y <- rpois(n, exp(eta))

## log-likelihood is in the fl-parameterisation:
## log(y!) + y * eta - 0.5 * 0 * (0 - eta)^2 - 1 * exp(0 + 1 *eta)
C <- cbind(-lfactorial(y), y, 0, 0, 1, 0, 1)

## this should give the same results
r <- inla(inla.mdata(C) ~ 1 + x, family = "fl", data = list(C = C, x = x),
          control.inla = list(cmin = 99999999),
          control.compute = list(po = T, cpo = T, waic = T))
rr <- inla(y ~ 1 + x, family = "poisson", data = data.frame(y, x),
           control.compute = list(po = T, cpo = T, waic = T))
           

summary(rr)
summary(r)
r$mlik - rr$mlik

