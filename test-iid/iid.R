n = 1000
scale = 1/1:n
x = rnorm(n, sd = 1/sqrt(scale))
y = x

idx = 1:n
r = inla(y ~ -1 + f(idx, model="iid",  scale = scale),
        family = "gaussian",
        data = data.frame(y, idx, scale), 
        control.family = list(hyper = list(prec = list(initial = 6, fixed = TRUE))))
