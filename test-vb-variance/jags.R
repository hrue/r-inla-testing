model {
    for(i in 1:N) {
        log(lambda[i]) <- intercept + beta1 * x[i] + beta2 * xx[i] + beta3 * xxx[i]
        y[i] ~ dpois(lambda[i])
    }
    intercept ~ dnorm(0, 0.1)
    beta1 ~ dnorm(0, 0.1)
    beta2 ~ dnorm(0, 0.1)
    beta3 ~ dnorm(0, 0.1)
}
