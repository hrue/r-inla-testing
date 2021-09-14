n = 20
p = 5
pacf = runif(p)
phi = inla.ar.pacf2phi(pacf)
y = arima.sim(n, model = list(ar = phi))
y = scale(y)
idx = 1:n
g = 1:n
one = rep(1, n)

inla.setOption(num.threads=1)

r = (inla(y ~ -1 + f(idx, model="ar", order = p, 
                     hyper = list(
                         prec = list(
                             initial = 0, 
                             prior = "pc.prec",
                             param = c(3, 0.01)),
                         pacf1 = list(
                             initial = pacf[1]
                             ##prior = "pc.rho0",
                             ##param = c(0.5, 0.5)
                             ), 
                         pacf2 = list(
                             initial = pacf[2] 
                             ##prior = "pc.rho0",
                             ##param = c(0.5, 0.4)
                             ))), 
          control.family = list(hyper =
              list(prec = list(
                       initial = 8,
                       fixed = TRUE))),
          control.inla = list(tolerance = 1e-16), 
          data = data.frame(y, idx, g, one)))
r = inla.rerun(r)

r1 = (inla(y ~ -1 + f(idx, model="ar1", 
                     hyper = list(
                         prec = list(
                             initial = 0, 
                             prior = "pc.prec",
                             param = c(3, 0.01)),
                         rho = list(
                             initial = pacf[1], 
                             prior = "pc.rho0",
                             param = c(0.5, 0.5)))), 
          control.family = list(hyper =
              list(prec = list(
                       initial = 8,
                       fixed = TRUE))),
          control.inla = list(tolerance = 1e-16), 
          data = data.frame(y, idx, g, one)))
r1 = inla.rerun(r1)

rr = (inla(y ~ -1 + f(one, model="iid",
                      hyper = list(
                          prec = list(
                              initial = 0, 
                              prior = "pc.prec",
                              param = c(3, 0.01))),
                      group = g,
                      control.group = list(
                          model = "ar",
                          order = p, 
                          hyper = list(
                              pacf1 = list(
                                  initial = pacf[1]
                                  ##prior = "pc.rho0",
                                  ##param = c(0.5, 0.5)
                                  ), 
                              pacf2 = list(
                                  initial = pacf[2]
                                  ##prior = "pc.rho0",
                                  ##param = c(0.5, 0.4)
                                  )))), 
           control.family = list(hyper =
               list(prec = list(
                        initial = 8,
                        fixed = TRUE))),
           verbose=FALSE, 
           control.inla = list(tolerance = 1e-16), 
           data = data.frame(y, idx, g, one)))
rr = inla.rerun(rr)

rr1 = (inla(y ~ -1 + f(one, model="iid",
                       hyper = list(
                           prec = list(
                               initial = 0, 
                               prior = "pc.prec",
                               param = c(3, 0.01))),
                       group = g,
                       control.group = list(
                           model = "ar1",
                           hyper = list(
                               rho = list(
                                   initial = pacf[1], 
                                   prior = "pc.rho0",
                                   param = c(0.5, 0.5))))), 
            control.family = list(hyper =
                list(prec = list(
                         initial = 8,
                         fixed = TRUE))),
            verbose=FALSE, 
            control.inla = list(tolerance = 1e-16), 
            data = data.frame(y, idx, g, one)))
rr1 = inla.rerun(rr1)

if (p == 1) {
    res = (cbind(r=r$internal.summary.hyperpar$mode, r1 = r1$internal.summary.hyperpar$mode,
                 rr=rr$internal.summary.hyperpar$mode, rr1 =
                 rr1$internal.summary.hyperpar$mode))
    print(cbind(res, sd=apply(res, 1, sd)))
    
} else {
    print(cbind(r=r$internal.summary.hyperpar$mode, rr=rr$internal.summary.hyperpar$mode,
                diff=r$internal.summary.hyperpar$mode - rr$internal.summary.hyperpar$mode))
}

                
    
            



