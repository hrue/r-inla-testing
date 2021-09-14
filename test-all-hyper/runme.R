INLA:::inla.my.update(b=T)
n = 3
idx = 1:n
y = arima.sim(n, model =list(ar = 0.5))
y = scale(y)
r = inla(y ~ 1 + f(idx, model="ar1",
    hyper = list(
        rho = list(
    prior = "betacorrelation",
            initial = 0.75,
            param = c(30, 10)))), 
    data = data.frame(y, idx), 
    control.family = list(hyper= list(prec = list(initial = 12,  fixed=TRUE))), 
    keep=TRUE,
    verbose=TRUE)

plot(r,  prior = TRUE, internal.scale = TRUE)
