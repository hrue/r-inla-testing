n = 100000
x = rnorm(n, sd = 1)
eta = 1 + x
p = 1.0/(1+exp(-eta))
y = rbinom(n, prob = p, size = 1)
r = inla(y ~ 1 + x + f(idx),
         family = "binomial",
         keep = T, 
         data = data.frame(y, x, idx = 1:n))
