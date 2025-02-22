library(statmod)
## the precision parameter in the beta distribution
phi = 4

## generate simulated data
n = 100
z = rnorm(n, sd=0.2)
eta = 1 + z
mu = exp(eta)/(1+exp(eta))
a = mu * phi
b = -mu * phi + phi
y = rbeta(n, a, b)

## estimate the model
formula = y ~ 1 + z
r = inla(formula, data = data.frame(y, z), family = "beta",
         inla.call = "inla.mkl.work", verbose = T,
         control.inla = list(int.strategy = "eb"), 
         control.compute = list(cpo=TRUE, config = TRUE, 
                                control.gcpo = list(enable = TRUE)))
plot(r$cpo$cpo, r$gcpo$gcpo)
abline(a=0,b=1)
est <- numeric(n)
for(idx in 1:n) {
    m <- r$misc$configs$config[[1]]$gcpodens.moments[idx, 1]
    s <- sqrt(r$misc$configs$config[[1]]$gcpodens.moments[idx, 2])
    gq <- gauss.quad.prob(11, dist = "normal", mu = m, sigma = s)

    val <- 0
    ph <- exp(r$mode$theta[1])
    for(j in 1:length(gq$nodes)) {
        mu = 1/(1+exp(-gq$nodes[j]))
        a = mu * ph
        b = -mu * ph + ph
        val <- val + dbeta(y[idx], a, b) * gq$weights[j]
    }
    est[idx] <- val
}
     
lines(r$cpo$cpo, est)
