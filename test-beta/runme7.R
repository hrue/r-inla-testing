## the precision parameter in the beta distribution
phi = 2

## generate simulated data
n = 10^4
z = rnorm(n, sd=.2)
eta = 1 + z
mu = exp(eta)/(1+exp(eta))
a = mu * phi
b = -mu * phi + phi
y = rbeta(n, a, b)

## this is the censoring
cens <- 0.05
y[y <= cens] <- 0
y[y >= 1-cens] <- 1

inla.setOption(num.threads = "1:1")

## estimate the model
formula = y ~ 1 + z
r = inla(formula, data = data.frame(y, z), family = "beta",
         control.family = list(beta.censor.value = cens))
summary(r)
rr = inla(formula, data = data.frame(y, z), family = "beta",
          control.family = list(beta.censor.value = cens),
          inla.call = "inla.mkl.work")
summary(r)
summary(rr)

r$mlik - rr$mlik
r$cpu.intern
rr$cpu.intern
