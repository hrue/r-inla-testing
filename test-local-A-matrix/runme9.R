n = 300
phi = 0.95
x = scale(arima.sim(n=n,  model = list(ar = phi)))
xx = scale(arima.sim(n=n,  model = list(ar = phi)))

h = 10L
nh = h + 1L
kern = dnorm(-h:0, sd=h)
kern = kern / sum(kern)
A = inla.as.sparse(INLA:::inla.toeplitz(c(kern[nh], rep(0, n-nh), kern[1:h])))

h = 30L
nh = h + 1L
kern = dnorm(-h:0, sd= h)
kern = kern / sum(kern)
B = inla.as.sparse(INLA:::inla.toeplitz(c(kern[nh], rep(0, n-nh), kern[1:h])))

eta <- 3 + as.vector(A %*% x) + as.vector(B %*% xx)
y <- rpois(n, lambda = exp(eta))

AB <- cbind(A, B)
r <- inla(y ~ 1 + f(idx, w, model="ar1", constr = T, A.local = AB, nrep = 2), 
          data = list(y = y, idx = 1:n, AB = AB, w = rep(0, n)),
          family = "poisson")

idx <- 1:n
par(mfrow = c(4, 1))
plot(idx, y, pch=19, ylim = range(pretty(y)))

plot(idx, eta, col="blue", lwd=2, type="l", ylim = range(pretty(eta)))
lines(idx, r$summary.linear.predictor$mean, col="red", lwd=2)
title("eta.true and E(eta|...)")

plot(idx, x, col="blue", lwd=2, type="l", ylim = range(pretty(x)))
lines(idx, r$summary.random$idx$mean[1:n], col="red", lwd=2)
title("AR1 and E(AR1)")

plot(idx, xx, col="blue", lwd=2, type="l", ylim = range(pretty(x)))
lines(idx, r$summary.random$idx$mean[n + 1:n], col="red", lwd=2)
title("AR1 and E(AR1)")

summary(r)
