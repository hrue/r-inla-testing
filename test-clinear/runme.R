
n = 100
s = 0.01
beta = 1
x = rnorm(n)
y = 1 + beta*x + rnorm(n, sd=s)
r = inla(y ~ 1 + f(x, model="clinear", range = c(0, Inf)),
        data = data.frame(y, x),
        verbose=TRUE, keep=TRUE, 
        family = "gaussian",
        control.family = list(hyper = list(prec = list(initial = log(1/s^2), fixed=TRUE))))
summary(r)
