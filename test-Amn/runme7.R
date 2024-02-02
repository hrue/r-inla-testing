## cofficients is an AR1
## fixed covariates is an AR1
n = 300
phi = 0.9
x = scale(arima.sim(n=n,  model = list(ar = phi)))
covariate <- scale(arima.sim(n=n,  model = list(ar = 0.5)))
eta = 2 + x * covariate

## then we do a convolution
h = 5L
nh = 2L*h + 1L
kern = dnorm(-h:h, sd= h/3)
kern = kern / sum(kern)

A = toeplitz(c(kern[-(1:h)],  rep(0,  n-nh),  kern[1:h]))
eta.star <- A %*% eta
y <- rpois(n, lambda = exp(eta.star))

r = inla(y ~ 1 + f(idx, covariate, model="ar1"), 
         control.predictor = list(A = A),
         data = data.frame(y, idx = 1:n, covariate),
         family = "poisson")

idx <- 1:n
par(mfrow = c(2, 2))
plot(idx, y, pch=19,  ylim = range(pretty(y)))

plot(idx, A %*% eta, col="blue",  lwd=2,  type="l",  ylim = range(pretty(A %*% eta)))
lines(idx, r$summary.linear.predictor$mean[1:n], col="red",  lwd=2)
title("eta* and  E(eta*)")

plot(idx, x, col="blue",  lwd=2,  type="l",  ylim = range(pretty(x)))
lines(idx, r$summary.random$idx$mean[1:n], col="red",  lwd=2)
title("AR1 and E(AR1)")

m = r$marginals.hyperpar[[2]]
plot(inla.smarginal(m),  type="l",  lwd=2)
abline(v = phi, col="red", lwd=2)
title("Lag one correlation")
