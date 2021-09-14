model {
    for(i in 1:N) {
        eta[i] <- intercept + u[i]
        u[i] ~ dnorm(0, tau)
        log(lambda[i]) <- eta[i]
        y[i] ~ dpois(E[i]*lambda[i])
    }
    intercept ~ dnorm(0, 1)
}
