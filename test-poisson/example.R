n = 100
x = rnorm(n, sd = 1)
eta = 2 + x
quantile = 0.9
lambda = qgamma(1.0 - quantile, shape = exp(eta)+1.0, rate=1)
y = rpois(n, lambda)

library(INLA)
r = inla(y ~ 1 + x,
         family = "poisson",
         control.family = list(
             control.link = list(
                 model = "quantile",
                 quantile = quantile)),
         data = data.frame(y, x))
summary(r)
