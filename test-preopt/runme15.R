n <- 10^6
x <- rnorm(n)
y <- 1 + x + rnorm(n, sd = 0.1)
inla.setOption(num.threads = "2:4")

r <- inla(y ~ 1 + x, 
          family = "gaussian", 
          data = data.frame(y, x),
          control.fixed = list(prec.intercept = 1),
          inla.mode = "experimental",
          ##inla.call = "inla.mkl.work",
          safe = FALSE, 
          verbose = TRUE)
