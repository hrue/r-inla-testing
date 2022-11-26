library(tweedie)
n <- 10000
x <- rnorm(n, sd = 0.3)
eta <- 1 + x
mu <- exp(eta)

p <- 1.25
phi <- 1.0
y <- numeric(n)
for(i in 1:n) {
    y[i] <- rtweedie(1, xi = p, mu = mu[i], phi = phi)
}

run <- function() {
    r <- inla(y ~ 1 + x + f(idx),
              data = data.frame(y, x, idx = 1:n),
              family = "tweedie",
              control.family = list(
                  hyper = list(
                      phi = list(initial = 0),
                      p = list(initial = inla.models()$likelihood$tweedie$hyper$theta1$to.theta(p), 
                               fixed = FALSE))), 
              control.fixed = list(prec.intercept = 1, prec = 1), 
              control.inla = list(cmin = 0), 
              control.compute = list(cpo = TRUE), 
              inla.call = "inla.mkl.work", 
              verbose = T)
    summary(r)
    return(r)
}
