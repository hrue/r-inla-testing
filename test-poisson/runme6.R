n <- 10E2
x <- rnorm(n, sd = 1)
eta <- 0 + 0.3 * x
lam <- exp(eta)
y <- rpois(n, lambda = lam)

inla.setOption(num.threads = "1:1",
               safe = FALSE,
               keep = T, 
               verbose = TRUE)

r <- inla(y ~ 1 + x, 
          family = "poisson",
          data = data.frame(y, x))
