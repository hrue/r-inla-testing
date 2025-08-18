n = 1000000
m <- 40
x = rnorm(n,  sd = 0.2)
eta = 3 + x
E = runif(n, min = 1, max=10)
E[] =1
true.size = 30
quantile = 0.9

size = true.size
q = E*exp(eta);
p = 1.0 - qbeta(quantile, q + 1.0, size, lower.tail = FALSE);
lambda = size * (1.0 - p) / p / E;
mu = E * lambda
y = rnbinom(n, size, mu=mu)

r = inla(y ~ 1 + x + f(idx),
         data = data.frame(y, x, E, idx = rep(1, n)),
         family = "nbinomial", E=E, verbose=T, 
         control.inla = list(cmin = 0), 
         keep = TRUE, inla.call = "", safe = FALSE, 
         control.family = list(list(
             hyper = list(
                 theta = list(param = 7, initial = log(true.size), fixed=FALSE)))))
  
