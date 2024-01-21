y <- runif(1)
mu <- runif(1)
xx <- c()
yy <- c()
log.phi <- seq(-3, 3, by = 0.01)
phi <- exp(log.phi)
xx <- log.phi
yy <- dbeta(y, mu * phi, - mu * phi + phi, log = TRUE)
par(mfrow = c(1, 2))
plot(xx, yy, type = "l")
s <- splinefun(xx, yy)
plot(xx, s(xx, deriv = 2), type = "l")
print(all(s(xx, deriv = 2) < 0))

