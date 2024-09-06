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
          control.fixed = list(prec.intercept = 1, prec = 1), 
          verbose = !TRUE)
cbind(r$summary.hyperpar[, "mean"],  phi)


jp = function(theta, theta.desc = NULL) {
    ##for(i in seq_along(theta.desc)) print(paste0("    theta[", i, "]=", theta.desc[i]))
    p.mean <- theta[J+1]
    p.lprec <- theta[J+2]

    lprior <- (sum(dnorm(theta[1:J], mean = p.mean, sd = sqrt(1/exp(p.lprec)), log = TRUE)) +
               dnorm(p.mean, mean = 1.5, sd = sqrt(1/exp(p.lprec)), log = TRUE) +
               (dgamma(exp(p.lprec), shape = 10, rate = 10, log=TRUE) + p.lprec) +
               ## to correct normalizing constants in 'generic'
               (-dnorm(0, sd = 1/sqrt(exp(p.mean)), log = TRUE)) +
               (-dnorm(0, sd = 1/sqrt(exp(p.lprec)), log = TRUE)))
    return (lprior)
}

jpr = inla.jp.define(jp, J = J, n = n) 
formula <- y ~ -1 + intercept + z +
    f(idx.na1, model = "generic", Cmatrix = matrix(1, 1, 1)) + 
    f(idx.na2, model = "generic", Cmatrix = matrix(1, 1, 1))

idx.na1 <- rep(NA, n*J)
idx.na2 <- rep(NA, n*J)

rr <- inla(formula,
          family = rep("beta", J),
          data = list(y = y, intercept = intercept, z = x,
                      idx.na1 = idx.na1, idx.na2 = idx.na2), 
          control.fixed = list(prec.intercept = 1, prec = 1), 
          verbose = TRUE,
          control.expert = list(jp = jpr))

           
