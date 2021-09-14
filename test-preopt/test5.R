n <- 25
y <- rpois(n, lambda = exp(1+rnorm(n, sd = 0.1)))

r <- inla(y ~ 1,
          data = data.frame(y),
          family = "nbinomial", 
          verbose = TRUE,
          keep = TRUE)
