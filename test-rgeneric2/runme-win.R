INLA:::inla.my.update(b=T)
inla.setOption(inla.call="/home/hrue/p/inla/builds/testing/windows64/local/bin/inla.exe")
n = 100
y = arima.sim(n, model = list(ar = 0.9))
y = scale(y) + rnorm(n, sd = 0.1)
s = 0.1
model = inla.rgeneric2.define(inla.rgeneric.ar1.model, n =n, ntheta = 2,
    debug=FALSE, R.init.file = "win-init.R")
r2 = inla(y ~ -1 + f(idx, model=model), 
    data = data.frame(y = y, idx = 1:n),
    control.inla = list(tolerance = 1e-12), 
    control.family = list(hyper = list(prec = list(initial = log(1/s^2), fixed=TRUE))), keep=TRUE)

r = inla(y ~ -1 + f(idx, model="ar1",
    hyper = list(
        prec = list(
            prior = "loggamma",
            param = c(1, 1)),
        rho = list(
            prior = "normal",
            param = c(0, 1)))), 
    data = data.frame(y = y, idx = 1:n), 
    control.inla = list(tolerance = 1e-12), 
    control.family = list(hyper = list(prec = list(initial = log(1/s^2), fixed=TRUE))))

r2$mlik - r$mlik
