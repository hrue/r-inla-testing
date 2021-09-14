n = 1000
x = rnorm(n, sd = 1)
eta = 1.1 + 0.77*x
p = 1.0/(1+exp(-eta))
y = sample(1:3, size=n, replace=TRUE)
ntrials = y + rnbinom(n, y, p)
r = inla(y ~ 1 + x,
         family = "binomial",
         control.family = list(variant = 1), 
         Ntrials = ntrials, 
         data = data.frame(y, x, ntrials),
         verbose=TRUE)
