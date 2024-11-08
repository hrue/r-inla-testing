m <- 98
n <- 100
phi <- 0.99
x0 <- scale(arima.sim(n, model = list(ar = phi)))
eta <- c()
xx <- list()
for(i in 1:m) {
    xx[[i]] <- scale(arima.sim(n, model = list(ar = phi)))
    eta <- c(eta, x0 + xx[[i]])
}

s <- 0.01
y <- eta + rnorm(n*m, sd = s)

## we let x0 be replicate m+1

res <- inla(y ~ -1 +
                f(idx1, model = "ar1", nrep = m + 1, replicate = r1) +
                f(idx2, copy = "idx1", replicate = r2, nrep = m+1),
            data = data.frame(y,
                              idx1 = rep(1:n, m),
                              idx2 = rep(1:n, m),
                              r1 = rep(1:m, each = n),
                              r2 = rep(m+1, n*m)
                              ),
            family = "normal",
            control.family = list(hyper = list(prec = list(initial = log(1/s^2),
                                                           fixed = TRUE))))


par(mfrow = rep(ceiling(sqrt(m)), 2))
plot(res$summary.random$idx2$mean[m*n + 1:n], x0); abline(a=0,b=1)
for(i in 1:m) {
    plot(res$summary.random$idx1$mean[(i-1)*n + 1:n], xx[[i]])
    abline(a=0,b=1)
}
plot(eta, res$summary.linear.predictor$mean)
abline(a = 0, b = 1)
