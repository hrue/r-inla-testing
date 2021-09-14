n = 100
x = rnorm(n, sd = 1)
eta = 4 + x
quantile = 0.99
lambda = qgamma(1.0 - quantile, shape = exp(eta)+1.0, rate=1)
y = rpois(n, lambda)
r = inla(y ~ 1 + x,
         family = "poisson",
         control.family = list(
             control.link = list(
                 model = "quantile",
                 quantile = quantile)),
         control.predictor = list(compute=TRUE), 
         control.compute = list(cpo=TRUE, dic=TRUE, waic=TRUE), 
         keep = TRUE, 
         data = data.frame(y, x))



