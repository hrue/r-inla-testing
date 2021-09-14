n = 1000
x = rnorm(n,  sd = 0.2)
eta = 1 + x
E = runif(n, min = 0, max=10)

mu = exp(eta)
size = 3000
y = rnbinom(n, size, mu=mu)
r = inla(y ~ 1 + x, data = data.frame(y, x, E),
         family = "nbinomial",
         control.family = list(
             hyper = list(
                 size = list(
                     prior = "pc.mgamma",
                     param = 10))))




                 

