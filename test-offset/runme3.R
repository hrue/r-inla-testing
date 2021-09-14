n = 100
A = diag(n)
offsets = sqrt(1:n)
intercept = -3
eta = rep(intercept, n)
eta.star = A %*% eta + offsets

y = rpois(n, lambda = exp(eta.star))
r = inla(y ~ 1,
        data = data.frame(y,  offsets),
        control.fixed = list(prec.intercept = 1), 
        control.compute = list(config=TRUE),
        control.predictor = list(A=A), offset = offsets, keep=T)

samples = inla.posterior.sample(10, r)

samples[[1]]$latent[1:n] - offsets

                      
