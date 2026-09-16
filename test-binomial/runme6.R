n = 10^5
x = rnorm(n, sd = 1)
eta = 1.1 + 0.77*x
p = 1.0/(1+exp(-eta))
y = sample(1:3, size=n, replace=TRUE)
ntrials = y + rnbinom(n, y, p)
r = inla(y ~ 1 + x + f(idx),
         family = "binomial",
         Ntrials = ntrials, 
         data = data.frame(y, x, ntrials, idx = 1:n),
         inla.call = "",
         keep = TRUE,
         verbose = TRUE)
