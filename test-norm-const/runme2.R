n = 1000
x = rnorm(n, sd = 0.2)
eta = 1 + 1.1*x 
p = exp(eta)/(1 + exp(eta))
size = sample(100:500, n, replace=TRUE)
y = rnbinom(n, size = size, prob = p)
r = inla(y ~ 1 + x + f(idx), family = "nbinomial2", Ntrials = size,
         data = data.frame(y, x, size, idx = 1:n),
         inla.call = "inla.mkl.work")
rr = inla(y ~ 1 + x + f(idx), family = "nbinomial2", Ntrials = size,
         data = data.frame(y, x, size, idx = 1:n),
         inla.call = "inla.mkl")
