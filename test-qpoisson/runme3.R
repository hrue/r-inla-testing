n <- 300
x <- rnorm(n, sd = 0.3)
eta <- 1 + x
alpha <- 0.8
lambda <- qgamma(alpha, shape = exp(eta)+1, lower.tail = FALSE)
y <- rpois(n, lambda)
r <- inla(y ~ 1 + x,
          family = "poisson",
          control.family = list(
              control.link = list(model = "quantile",
                                  quantile = alpha)),
          data = data.frame(y, x))
summary(r)

