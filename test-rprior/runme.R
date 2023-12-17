log.dgamma <- function(x) {
    return (dgamma(exp(x), a, b, log = TRUE) + x)
}

y <- rnorm(10)
rr <- inla(y ~ 1,
          data = data.frame(y), 
          family = "gaussian",
          control.fixed = list(prec.intercept = 3), 
          control.family = list(hyper = list(prec = list(prior = "loggamma", param = c(10, 10)))), 
          verbose = TRUE,
          safe = FALSE,
          keep = TRUE)

rprior.log.dgamma <- inla.rprior.define(log.dgamma, a = 10, b = 10)

r <- inla(y ~ 1,
          data = data.frame(y), 
          family = "gaussian",
          control.fixed = list(prec.intercept = 3), 
          control.family = list(hyper = list(prec = list(prior = rprior.log.dgamma))), ##, param = numeric(0)))), 
          verbose = TRUE,
          safe = FALSE,
          keep = TRUE)

r$mlik - rr$mlik

