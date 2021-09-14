## overdispersion parameter in the betabinomial
rho = 0.1

n = 10000
z = rnorm(n, sd=0.4)
Ntrials = sample(1:20, n, replace=TRUE)
eta = 1 + z
p = exp(eta)/(1+exp(eta))
s = runif(n)
m = Ntrials * p
v = Ntrials * p * (1.0 - p) * (1.0 + s * (Ntrials - 1) * rho)
y = rnorm(n, mean = m, sd = sqrt(v))

formula = y ~ 1 + z
data = data.frame(y, z, s)
r = inla(formula, data = data, scale = s, 
         family = "betabinomialna", Ntrials=Ntrials, verbose = TRUE,
         control.inla = list(strategy = "adaptive"))
summary(r)

