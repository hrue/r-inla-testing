library(tweedie)

my.dtweedie <- function(y, alpha, gamma, lambda) {

    mu <- lambda * alpha * gamma
    p <- (alpha+2)/(alpha+1)
    phi <- lambda^(1-p) * (alpha*gamma)^(2-p) / (2-p)

    print(c(mu = mu, p = p, phi = phi))

    sum <- 0.0
    for(tt in 0:100) {
        sum <- sum + dgamma(y, shape = tt*alpha, scale = gamma) * dpois(tt, lambda = lambda)
    }
    return(sum)
}

phi <- 1
xi <- 1.5
mu <- 1.234
y <- 2.345

print(dtweedie(y, xi, mu, phi))
print(ptweedie(y, xi, mu, phi))
