n = 1000
N = 20
z = rnorm(n)
eta = -3 + 0.1 * z
prob = exp(eta)
y = rbinom(n, N, prob = prob)

r = inla(y ~ 1 + z,  data = data.frame(y, z), verbose = TRUE,
        offset = rep(3, n), 
        Ntrials = rep(N, n), family = "binomial",
        control.family = list(link = "log"))
