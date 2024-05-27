inla.setOption(num.threads = "4:1", internal.opt = FALSE)

log.dgamma <- function(x) {
    return (dgamma(exp(x), a, b, log = TRUE) + x)
}

n <- 1000
x <- rnorm(n)
y <- x + rnorm(n)
rr <- inla(y ~ x + f(idx, hyper = list(
                              prec = list(initial = 10,
                                          prior = "loggamma",
                                          param = c(10, 1)))), 
          data = data.frame(y, idx = 1:n, x), 
          family = "gaussian",
          control.fixed = list(prec.intercept = 3), 
          control.family = list(hyper = list(prec = list(initial = 0,
                                                         prior = "loggamma",
                                                         param = c(10, 10)))), 
          verbose = TRUE,
          safe = FALSE,
          keep = TRUE)
##rr <- inla.rerun(rr)

rprior.log.dgamma <- inla.rprior.define(log.dgamma, a = 10, b = 10)
rprior.log.dgamma2 <- inla.rprior.define(log.dgamma, a = 10, b = 1)

r <- inla(y ~ x + f(idx, hyper = list(
                             prec = list(initial = 10,
                                         prior = rprior.log.dgamma2,
                                         param = numeric(0)))), 
          data = data.frame(y, idx = 1:n, x), 
          family = "gaussian",
          control.fixed = list(prec.intercept = 3), 
          control.family = list(hyper =
                                    list(prec = list(
                                             prior = rprior.log.dgamma,
                                             param = numeric(0)))), 
          verbose = TRUE,
          safe = FALSE,
          keep = !TRUE)
##r <- inla.rerun(r)

r$mode$theta - rr$mode$theta
r$mlik - rr$mlik
