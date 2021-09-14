n = 100
idx = 1:n
y = rnorm(n)
r = inla(y ~ 1 + f(idx, hyper = list(prec = list(param = c(10, 1)))), 
         family = "gaussian",
         control.family = list(hyper = list(prec = list(param = c(100, 10)))), 
         data = data.frame(y, idx))

jp = function(theta) {
    if (!is.null(.theta.desc)) {
        for(i in seq_along(.theta.desc))
            print(paste0("    theta[", i, "]=", .theta.desc[i]))
    }
    param.1 = c(100, 10) 
    param.2 = c(10, 1)  
    lprior = (dgamma(exp(theta[1]), shape = param.1[1], rate = param.1[2], log=TRUE) +
              theta[1] +
              dgamma(exp(theta[2]), shape = param.2[1], rate = param.2[2], log=TRUE) +
              theta[2])
    return (lprior)
}
jpr = inla.jp.define(jp) 
iid = inla.rgeneric.define(inla.rgeneric.iid.model, n = n)
rr = inla(y ~ 1 + f(idx, model=iid), 
          family = "gaussian",
          data = data.frame(y, idx),
          control.expert = list(jp = jpr),
          verbose = TRUE)

r$mlik - rr$mlik
for(theta in 1:2) {
    inla.dev.new()
    x.range = inla.qmarginal(c(0.001, 0.999), r$internal.marginals.hyperpar[[theta]])
    plot(r$internal.marginals.hyperpar[[theta]], xlim = x.range,
         pch = 19, main = paste("theta.", theta, sep=""))
    lines(inla.smarginal(rr$internal.marginals.hyperpar[[theta]]))
}


