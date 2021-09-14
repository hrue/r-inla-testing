n = 300
p = 5
pacf = runif(p)
phi = inla.ar.pacf2phi(pacf)
y = arima.sim(n, model = list(ar = phi))
y = scale(y)*2
idx = 1:n
one = rep(1, n)

r = inla(y ~ -1 + f(idx, model='ar', order = p,
                    hyper = list(prec = list(
                                     initial = -2,
                                     fixed=T, 
                                     prior = "pc.prec",
                                     param = c(3, 0.01)))), 
        family = "gaussian", 
        control.family = list(initial = 12, fixed=TRUE), 
        control.inla = list(int.strategy = "eb"), 
        data = data.frame(y, idx),
        keep=TRUE, 
        verbose=TRUE)

                
rr = inla(y ~ -1 + f(one, model='iid', group = idx,
                     hyper = list(prec = list(
                                      initial = -2,
                                      fixed=T, 
                                     prior = "pc.prec",
                                     param = c(3, 0.01))), 
        control.group=list(model='ar', order = p)), 
        family = "gaussian", 
        control.family = list(initial = 12, fixed=TRUE), 
        control.inla = list(int.strategy = "eb"), 
        data = data.frame(y, idx),
        keep=TRUE, 
        verbose=TRUE)

                
                
