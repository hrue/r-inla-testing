n = 2000
x = arima.sim(n, model= list(ar = 0.95))
x = x/sd(x)
y = rnorm(n, mean=0, sd = sqrt(exp(x)))

r = inla(y ~ 1 + f(idx, model="ar1"), 
        data = data.frame(y, idx=1:n),
        family = "stochvol")
