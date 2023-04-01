
n <- 10^5
x <- inla.group(runif(n))
y <- rpois(n, exp(1 + x))

r <- inla(y ~ 1 + f(x),
          data = data.frame(y, x),
          verbose = TRUE,
          keep = TRUE,
          safe = FALSE,
          inla.call = "")

              
