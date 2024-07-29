n <- 500
J <- 10
phi = seq(1, J, len = J)
y <- matrix(NA, n*J, J)
x <- c()

for(j in 1:J) {
    z <- rnorm(n, sd=0.2)
    eta <- 1 + z
    mu <- exp(eta)/(1+exp(eta))
    a <- mu * phi[j]
    b <- -mu * phi[j] + phi[j]
    yy <- rbeta(n, a, b)
    x <- c(x, z)
    y[((j-1)*n + 1):((j-1)*n + n), j] <- yy
}

intercept <- rep(1, n*J)
formula <- y ~ -1 + intercept + z
r <- inla(formula,
          family = rep("beta", J),
          data = list(y = y, intercept = intercept, z = x),
          verbose = TRUE)

cbind(r$summary.hyperpar[, "mean"],  phi)
