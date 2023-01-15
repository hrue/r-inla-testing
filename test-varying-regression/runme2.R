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

hyper = list(prec = list(
                 prior = "pc.prec",
                 param = c(0.5, 0.01)),
             rho = list(
                 prior = "pc.cor1",
                 param = c(0.866, 0.5)))

tt0 <- c(t(cbind(rep(1, n), NA, NA)))
tt2 <- c(t(cbind(NA, rep(1, n), NA)))
tt3 <- c(t(cbind(NA, NA, rep(1, n))))

gg0 <- 1:n
gg1 <- 1:n
gg2 <- 1:n

data <- data.frame(y,
                   zero = rep(0, n),
                   one = rep(1, n),
                   two = rep(1, n),
                   g0 = 1:n,
                   g1 = 1:n,
                   g2 = 1:n,
                   
                   
                   

r <- inla(y ~ -1 +
              f(zero, x0, model = "ar1",
                group = g0, ngroup = n, values = 1:3) + 
              f(one, x1, copy = "zero", group = g1) + 
              f(two, x2, copy = "zero", group = g2), 
          data = data,
          family = "gaussian")
               
