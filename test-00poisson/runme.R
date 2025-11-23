n <- 10^5
X1 <- rnorm(n)
X2 <- rnorm(n)
XX1 <- rep(-1, n)
XX2 <- rnorm(n)
E <- runif(n, min = 0.5, max = 2.0)
z <- numeric(n)
prob <- 1/(1+exp(-(0.25 * XX1 + 0.5 * XX2)))
y <- numeric(n)
lambda <- E * exp(1 + 0.25 * X1 + 0.5 * X2)
q <- 0.95
for(i in 1:n) {
    ## sample which value to inflate, uniformly
    m <- qpois(q, lambda[i])
    z[i] <- sample(0:m, 1)
    z[i] <- 0
    ## sample the inflated value of not
    if (runif(1) < prob[i]) {
        y[i] <- z[i]
    } else {
        y[i] <- rpois(1, lambda[i])
    }
}

Y <- inla.mdata(cbind(y, E), cbind(XX1, XX2))
Y <- inla.mdata(cbind(y, E, z), cbind(XX1, XX2))
r <- inla(
    Y ~ 1 + X1 + X2, 
    family = "0poisson",
    data = list(Y = Y, X1 = X1, X2 = X2), 
    control.inla = list(int.strategy = "auto", cmin = 0), 
    verbose=T,
    safe = F, 
    num.threads = "2:10")
summary(r)
