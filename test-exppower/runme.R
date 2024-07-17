library(gnorm)
INLA:::inla.my.update(b = T)

n <- 10^5
x <- rnorm(n)
sigma <- 2.0
beta <- 1.5
alpha <- sqrt(gamma(1/beta)/gamma(3/beta)) * sigma
y <- 1 + x + rgnorm(n, alpha = alpha, beta = beta)

r <- inla(y ~ 1 + x,
          data = data.frame(y, x),
          family = "exppower",
          control.family = list(hyper = list(power = list(initial = log(beta-1), fixed = !TRUE),
                                             prec = list(initial = log(1/sigma^2), fixed = !TRUE))), 
          control.fixed = list(prec.intercept = 1), 
          control.inla = list(cmin = 0), 
          control.compute = list(cpo = T), 
          num.threads = "1:1", 
          verbose = TRUE,
          safe = FALSE)

rr <- inla(y ~ 1 + x,
          data = data.frame(y, x),
          family = "gaussian",
          control.family = list(hyper = list(prec = list(initial = log(1/sigma^2), fixed = !TRUE))), 
          control.fixed = list(prec.intercept = 1), 
          control.compute = list(cpo = T))


print(c(1/sigma^2))
summary(r)

