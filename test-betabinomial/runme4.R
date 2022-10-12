inla.setOption(inla.mode = "experimental")
rho = 0.1

n = 30000
z = rnorm(n, sd=0.2)
Ntrials = sample(10:20, n, replace=TRUE)
eta = 1 + z
p.eta = exp(eta)/(1+exp(eta))
a = p.eta * (1-rho)/rho
b = (p.eta * rho - p.eta - rho + 1)/rho
p = rbeta(n, a, b)
y = rbinom(n, Ntrials, p)
s = 1

formula = y ~ 1 + z
data = data.frame(y, z)
r = inla(formula, data = data,
        family = "betabinomial", Ntrials=Ntrials, verbose=TRUE,
        control.compute = list(cpo = TRUE))
rr = inla(formula, data = data,
          inla.call = "inla.mkl.work", 
        family = "betabinomial", Ntrials=Ntrials, verbose = TRUE, 
        control.compute = list(cpo = TRUE))
summary(r)
summary(rr)

