n = 2000
x = rnorm(n,  sd = 0.2)
eta = 1 + x
E = runif(n, min = 0, max=10)
true.size = 3

mu = E * exp(eta)
size = true.size
y = rnbinom(n, size, mu=mu)
r = inla(y ~ 1 + x, data = data.frame(y, x, E),
         family = "nbinomial", E=E,
         control.family = list(
             hyper = list(
                 theta = list(param = 7))))


mu = E * exp(eta)
size = E*true.size
prob = size/(size + mu)
y = rnbinom(n, size, mu=mu)
rr = inla(y ~ 1 + x, data = data.frame(y, x, E),
    family = "nbinomial",
    control.family = list(variant = 1),
    E=E)


