n = 5
eta = rnorm(n)
lambda = exp(eta)
y = rpois(n, lambda)
r = inla(y ~ 1,
         data = data.frame(y),
         family = "poisson",
         verbose=TRUE)
r$mode
