n <- 10^5
X1 <- rnorm(n)
X2 <- rnorm(n)
XX1 <- rep(1, n)
XX2 <- rnorm(n)
E <- runif(n)
z <- numeric(n)
zz <- numeric(n)
intercept <- rnorm(n, sd = 1)
prob <- 1/(1+exp(-(-0.5 * XX1 + 0.5 * XX2)))
y <- numeric(n)
for(i in 1:n) {
    m <- 25
    lambda <- E[i] * exp(0.25 * X1[i] + 0.5 * X2[i])
    pp <- dpois(0:m, lambda = lambda) * (1-prob[i])
    pp[1] <- pp[1] + prob[i]
    y[i] <- sample(0:m, 1, prob = pp)
    if (y[i] == 0) {
        p0 <- prob[i] / pp[1]
        if (runif(1) < p0) {
            z[i] <- 0
        } else {
            z[i] <- 1
        }
    } else {
        z[i] <- 1
    }
    if (runif(1) < 1+0.25) {
        zz[i] <- 1-z[i]
    } else {
        zz[i] <- NA
    }
}

r <- inla(
    inla.mdata(cbind(y, E, z, zz), cbind(XX1, XX2)) ~ -1 + X1 + X2, 
    family = "00poisson",
    data = data.frame(y,E,z, zz, intercept, X1, X2, XX1, XX2), 
    control.inla = list(int.strategy = "eb", cmin = 0), 
    verbose=T,
    safe = F, 
    num.threads = "2:8")
summary(r)
