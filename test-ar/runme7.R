n = 300
p = 5
pacf = runif(p)
phi = inla.ar.pacf2phi(pacf)
y = arima.sim(n, model = list(ar = phi))
y = scale(y)*2
idx = 1:n
param.phi = c(rep(0, p),  diag(p))
param.prec = c(1, 0.01)

rr = inla(y ~ -1 + f(idx, model='ar', order = p+1,
                     ngroup = 2, 
    hyper = list(theta4 = list(
                     param = c(0.5, 1e-40)))), 
        family = "gaussian", 
        control.family = list(initial = 10, fixed=TRUE), 
    data = data.frame(y, idx),
    verbose= TRUE,
    keep = TRUE)

                
                
