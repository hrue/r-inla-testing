n = 100
p = 3
pacf = runif(p)
phi = inla.ar.pacf2phi(pacf)
y = arima.sim(n, model = list(ar = phi))
y = scale(y)*2
idx = 1:n
param.phi = c(rep(0, p),  diag(p))
param.prec = c(1, 0.01)

r = inla(y ~ -1 + f(idx, model='ar',
                order = p, 
                hyper = list(
                        prec = list(initial = 1, fixed=FALSE, param = param.prec),
                        theta2 = list(initial = 0, fixed=T, param = param.phi))), 
        family = "gaussian", 
        control.family = list(initial = 12, fixed=TRUE), 
        data = data.frame(y, idx),
        keep=TRUE, 
        verbose=TRUE)

                
r = inla(y ~ -1 + f(idx, model='ar', order = p), 
        family = "gaussian", 
        control.family = list(initial = 12, fixed=TRUE), 
        data = data.frame(y, idx),
        keep=TRUE, 
        verbose=TRUE)

                
                
