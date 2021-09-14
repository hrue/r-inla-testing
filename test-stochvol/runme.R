n = 2000
x = arima.sim(n, model= list(ar = 0.95))
x = x/sd(x)
offset.prec = 1
y = rnorm(n, mean=0, sd = sqrt(exp(x) + 1/offset.prec))

r = inla(y ~ 1 + f(idx, model="ar1"), 
        data = data.frame(y, idx=1:n),
        family = "stochvol",
        control.family = list(
                hyper = list(
                        prec = list(
                                initial = log(offset.prec),
                                fixed = FALSE))))

