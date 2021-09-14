n = 500
x = arima.sim(n, model=list(ar=0.9))
x = x/sd(x)
y = 1 + x + rnorm(n, sd = 0.1)
r = inla(y ~ 1 + f(idx, model="ar1"),
    data = data.frame(y, idx=1:n),
    family = "gaussian",
    control.family = list(
        hyper = list(
            prec = list(
                prior = "pc.prec",
                param = c(3, 0.01)))))
summary(r)

