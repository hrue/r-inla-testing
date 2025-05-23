n = 10^4
x = rnorm(n,  sd = 0.2)
eta = 3 + x
E = runif(n, min = 1, max=10)
E[] =1

size = 20
q = E*exp(eta);
quantile <- 0.5
p = 1.0 - qbeta(quantile, q + 1.0, size, lower.tail = FALSE);
lambda = size * (1.0 - p) / p / E;
mu = E * lambda
y = rnbinom(n, size, mu=mu)

r = inla(y ~ 1 + x, data = data.frame(y, x, E),
         family = "nbinomial", E=E, verbose=T)
