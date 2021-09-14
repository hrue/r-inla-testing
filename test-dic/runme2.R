n = 200
x = rnorm(n,  sd = 0.2)
eta = 15 + x
E = runif(n, min = 0, max=10)
true.size = 3

mu = E * exp(eta)
size = true.size
y = rnbinom(n, size, mu=mu)
r = inla(y ~ 1 + x, data = data.frame(y, x, E),
         family = "nbinomial", E=E,
         control.compute = list(dic = TRUE),
         verbose = TRUE, 
         control.family = list(
             hyper = list(
                 theta = list(param = 7))))



