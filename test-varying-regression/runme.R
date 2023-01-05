n <- 300
x0 <- rep(1, n)
x1 <- rnorm(n)
x2 <- rnorm(n)
s <- 0.1

beta0 <- scale(arima.sim(n, model = list(ar = 0.999)))
beta1 <- scale(arima.sim(n, model = list(ar = 0.995)))
beta2 <- scale(arima.sim(n, model = list(ar = 0.95)))

y <- numeric(n)
for(i in 1:n) {
    y[i] <- beta0[i] * x0[i] + beta1[i] * x1[i] +
        beta2[i] * x2[i] + rnorm(n, sd = s)
}

data <- data.frame(y,
                   t0 = 1:n,
                   t1 = 1:n,
                   t2 = 1:n)

hyper = list(prec = list(
                 prior = "pc.prec",
                 param = c(0.5, 0.01)),
             rho = list(
                 prior = "pc.cor1",
                 param = c(0.866, 0.5)))

r <- inla(y ~ -1 +
               f(t0, x0, model = "ar1", hyper = hyper) +
               f(t1, x1, model = "ar1", hyper = hyper) +
               f(t2, x2, model = "ar1", hyper = hyper),
           data = data,
           family = "gaussian")
               
par(mfrow = c(2, 2))
plot(r$summary.random$t0$mean, pch = 19)
lines(beta0, lwd = 3)

plot(r$summary.random$t1$mean, pch = 19)
lines(beta1, lwd = 3)

plot(r$summary.random$t2$mean, pch = 19)
lines(beta2, lwd = 3)
