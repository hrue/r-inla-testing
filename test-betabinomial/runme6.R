## overdispersion parameter in the betabinomial
rho = 0.1

n = 10^5
z = rnorm(n, sd=0.4)
Ntrials = sample(1:1000, n, replace=TRUE)
eta = 1 + z
p.eta = exp(eta)/(1+exp(eta))
a = p.eta * (1-rho)/rho
b = (p.eta * rho - p.eta - rho + 1)/rho
p = rbeta(n, a, b)
y = rbinom(n, Ntrials, p)
s = 1

formula = y ~ 1 + z
data = data.frame(y, z, s)
r = inla(formula, data = data, scale = s, 
         family = "betabinomial", Ntrials=Ntrials, verbose = !TRUE,
         num.threads = 1)
r$cpu.intern

rr = inla(formula, data = data, scale = s, 
         family = "betabinomial", Ntrials=Ntrials, verbose = !TRUE,
         num.threads = 1,
         inla.call = "inla.mkl.work")
rr$cpu.intern

