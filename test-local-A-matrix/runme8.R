## cofficients is an AR1
## fixed covariates is an AR1
n = 300
phi = 0.9
x = scale(arima.sim(n=n,  model = list(ar = phi)))
z <- rnorm(n, sd = 0.2)
zz <- rnorm(n, sd = 0.3)

h = 10L
nh = h + 1L
kern = dnorm(-h:0, sd= h/2)
kern = kern / sum(kern)

A = inla.as.sparse(my.toeplitz(c(kern[nh],  rep(0,  n-nh),  kern[1:h])))
eta <- 0.5 + 0.5 * z + 0.5 * zz + as.vector(A %*% x)
y <- rpois(n, lambda = exp(eta))

r = inla(y ~ 1 + z + zz + f(idx, w, model="ar1", A.local = A), 
         data = list(y = y, z = z, zz = zz, idx = 1:n, A = A, w = rep(0, n)),
         family = "poisson")

idx <- 1:n
par(mfrow = c(2, 2))
plot(idx, y, pch=19,  ylim = range(pretty(y)))

plot(idx, eta, col="blue",  lwd=2,  type="l",  ylim = range(pretty(eta)))
lines(idx, r$summary.linear.predictor$mean, col="red",  lwd=2)
title("eta.true and  E(eta|...)")

plot(idx, x, col="blue",  lwd=2,  type="l",  ylim = range(pretty(x)))
lines(idx, r$summary.random$idx$mean[1:n], col="red",  lwd=2)
title("AR1 and E(AR1)")

m = r$marginals.hyperpar[[2]]
plot(inla.smarginal(m),  type="l",  lwd=2)
abline(v = phi, col="red", lwd=2)
title("Lag one correlation")

summary(r)

