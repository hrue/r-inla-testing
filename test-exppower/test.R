p <- runif(1)
beta <- 1.5 + runif(1)
lambda <- runif(1)
mu <- runif(1)


sign(p - 0.5) * qgamma(abs(p - 0.5) * 2, shape = 1/beta, scale = 1/lambda)^(1/beta) + mu
sign(p - 0.5) * (1/lambda * qgamma(abs(p - 0.5) * 2, shape = 1/beta, scale = 1))^(1/beta) + mu
