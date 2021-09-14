s = 0.9
e = 0.7

n = 1000
x = 1+rnorm(n)
pp = exp(x) / (1 + exp(x))
p = pp * s + (1-pp) * (1-e)

logit = function(p) log(p/(1-p))
size = 1
y = rbinom(n, size=size, prob = p)

r = inla(y ~ 1+x, data = data.frame(y, x),
        family = "testbinomial1",
        control.fixed = list(prec.intercept = 1,  prec = 1), 
        Ntrials = size)
