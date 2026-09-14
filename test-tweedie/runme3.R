library(tweedie)
n <- 10^5
x <- rnorm(n, sd = 1)
eta <- 2 + x
mu <- exp(eta)

p <- 1.25
phi <- 1.5
y <- numeric(n)
for(i in 1:n) {
    y[i] <- rtweedie(1, xi = p, mu = mu[i], phi = phi)
}

INLA:::inla.my.update()
inla.setOption(num.threads = "4:1:1")

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
          verbose = T)

rr <- inla(y ~ 1 + x + f(idx),
          data = data.frame(y, x, idx = 1:n),
          family = "tweedie",
          control.family = list(
              hyper = list(
                  phi = list(initial = 0),
                  p = list(initial = inla.models()$likelihood$tweedie$hyper$theta1$to.theta(p), 
                           fixed = FALSE))), 
          control.fixed = list(prec.intercept = 1, prec = 1), 
          control.inla = list(cmin = 0),
          inla.call = "/home/hrue/bin/inla.mkl.work", 
          verbose = T)
