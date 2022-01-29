n = 3000
x = rnorm(n,  sd = 0.2)
eta = 1 + x
E = runif(n, min = 0.1, max=10)
S = runif(n, min = 0.5, max=2)

mu = E * exp(eta)
size = S 
prob = size/(size + mu)
y = rnbinom(n, size, mu=mu)

r = inla(y ~ 1 + x, data = data.frame(y, x, E, S),
    family = "nbinomial",
    control.family = list(variant = 2,
                          hyper = list(theta = list(initial = 0,
                                                    fixed = TRUE))), 
    E=E, scale = S, verbose = T)
summary(r)
