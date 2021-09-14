n = 500
x = arima.sim(n, model=list(ar=0.9))
x = c(scale(x))
s = 0.1
y = 1 + x + rnorm(n, sd = s)
idx = 1:n

hyper = list(
    prec = list(
        prior = "pc.prec",
        param = c(3, 0.01)),
    rho = list(
        prior = "pc.cor0",
        param = c(0.5, 0.5)))
cf = list(hyper = list(prec = list(initial = log(1/s^2), fixed=TRUE)))

r = inla(y ~ -1 + f(idx, model="ar1", hyper = hyper), 
    data = data.frame(y, idx),
    family = "gaussian", control.family = cf)

rr = inla(y ~ -1 + f(idx, model="ar1c", hyper = hyper, 
                    args.ar1c = list(Z = matrix(0, n, 1), Q.beta = matrix(1, 1, 1))), 
    data = data.frame(y, idx),
    family = "gaussian", control.family = cf,
    verbose=TRUE)
