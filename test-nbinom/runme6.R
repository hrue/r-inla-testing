n = 10^4
x = rnorm(n,  sd = 0.2)
off <- rnorm(n)
eta = 3 + x + off
E = runif(n, min = 1, max=10)
E[] =1

size = 20
q = E*exp(eta);
quantile <- 0.5
p = 1.0 - qbeta(quantile, q + 1.0, size, lower.tail = FALSE);
lambda = size * (1.0 - p) / p / E;
mu = E * lambda
y = rnbinom(n, size, mu=mu)

r = inla(y ~ 1 + x + offset(off),
         data = data.frame(y, x, E, off),
         family = "nbinomial",
         E=E,
         verbose=T)
rr = inla(y ~ 1 + x + offset(off),
          data = data.frame(y, x, E, off),
          family = "nbinomial",
          E=E,
          verbose=T,
          inla.call = "inla.mkl.work")
