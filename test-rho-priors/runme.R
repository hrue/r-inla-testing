n = 100
x = arima.sim(n, model = list(ar = 0.5))
x = scale(x)
prec.fixed = list(prec = list(initial = 10,  fixed=TRUE))

idx = 1:n
y = x
r = inla(y ~ -1 + f(idx, model="ar1",
        hyper = list(
                prec = list(
                        prior = "pc.prec",
                        param = c(0.5, 0.001)), 
                rho = list(
                        prior = "pc.rho0",
                        param = c(0.1, 0.01)))), 
        data = data.frame(y, idx),
        control.family = list(hyper = prec.fixed))
summary(r)


n = 10
x = arima.sim(n, model = list(ar = 0.99))
x = scale(x)
prec.fixed = list(prec = list(initial = 10,  fixed=TRUE))

idx = 1:n
y = x
r = inla(y ~ -1 + f(idx, model="ar1",
        hyper = list(
                prec = list(
                        prior = "pc.prec",
                        param = c(0.5, 0.001)), 
                rho = list(
                        prior = "pc.rho1",
                        param = c(0.9, 0.9)))), 
        data = data.frame(y, idx),
        control.family = list(hyper = prec.fixed),
        verbose=TRUE, keep=TRUE,
        control.inla = list(reordering = "metis"))
summary(r)
