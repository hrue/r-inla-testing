n = 1000
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

r = inla(y ~ 1 + x, data = data.frame(y, x, E),
         family = "nbinomial", E=E, verbose=T, 
         control.inla = list(cmin = 0), 
         control.family = list(list(
             hyper = list(
                 theta = list(param = 7, initial = log(true.size), fixed=FALSE)), 
             control.link = list(model = "quantile",  quantile = quantile))))

if (FALSE) {
    mu = E * exp(eta)
    size = E*true.size
    prob = size/(size + mu)
    y = rnbinom(n, size, mu=mu)
    rr = inla(y ~ 1 + x, data = data.frame(y, x, E),
              family = "nbinomial",
              control.family = list(variant = 1),
              E=E)
}

