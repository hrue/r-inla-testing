n = 500
x = arima.sim(n, model=list(ar=0.9))
x = x/sd(x)
y = 1 + x + rnorm(n, sd = 0.1)
y.save = y
y[c(n-1, n)] <- NA
r = inla(y ~ 1 + f(idx, model="ar1"),
    data = data.frame(y, idx=1:n),
    family = "gaussian",
    control.predictor = list(compute = TRUE), 
    control.compute = list(config = TRUE), 
    control.family = list(
        hyper = list(
            prec = list(
                prior = "pc.prec",
                param = c(3, 0.01)))))
summary(r)

xx <- inla.posterior.sample(1000, r)

y.pred <- inla.posterior.sample.eval(
    function(n) {
    prec <- theta[1]
    y.pred <- rnorm(2, mean = Predictor[c(n-1, n)], sd = sqrt(1/prec))
}, xx, n = n)

par(mfrow = c(1, 2))
plot(density(y.pred[1, ]))
abline(v = y.save[n-1])
plot(density(y.pred[2, ]))
abline(v = y.save[n])


