n <- 10^6
x <- inla.group(runif(n))
y <- rpois(n, exp(1 + x))

r <- inla(y ~ 1 + f(x),
          data = data.frame(y, x),
          family = "poisson", 
          control.fixed = list(prec.intercept = 1), 
          verbose = TRUE,
          keep = TRUE,
          safe = FALSE,
          inla.call = "")

              
