INLA:::inla.my.update(b = T)
inla.setOption(num.threads = "6:3")

if (TRUE) {
    invlink <- inla.link.invcloglog
    link.name <- "cloglog"
} else {
    invlink <- inla.link.invlogit
    link.name <- "logit"
}

n <- 50000
m <- 10
nc <- 3
beta <- c(-1, rnorm(nc-1, sd = 0.2))
Y <- matrix(NA, n, m)
X <- matrix(NA, n, m*nc)
z <- rnorm(n, mean = 0, sd = 0.3)
eta <- 3 + z
p.obs <- invlink(eta)
for (i in 1:n) {
    is.zero <- rbinom(1, size = 1, prob = 1 - p.obs[i])
    nyy <- sample(2:m, 1)
    for(j in 1:m) {
        off <- (j-1) * nc
        if (j <= nyy) {
            X[i, off + 1:nc] <- c(1, rnorm(nc-1, sd = 0.2))
            eeta <- sum(X[i, off + 1:nc] * beta)
            p <- invlink(eeta)
            if (is.zero) {
                Y[i, j] <- 0
            } else {
                Y[i, j] <- rbinom(1, size = 1, prob = p)
            }
        } else {
            X[i, off + 1:nc] <- rep(NA, nc)
            Y[i, j] <- NA
        }
    }
}

r <- inla(inla.mdata(Y, X) ~ 1 + z,
          family = "occupancy",
          control.family = list(link = link.name, link.simple = link.name), 
          data = list(Y = Y, X = X, z = z),
          safe = FALSE,
          verbose = TRUE,
          control.fixed = list(prec.intercept = 1))
summary(r)
cbind(beta, r$summary.hyperpar[, "mean"])

