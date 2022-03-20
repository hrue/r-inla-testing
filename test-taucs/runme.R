n <- 1000
y <- rnorm(n)
x <- rnorm(n)
xx <- rnorm(n)

r <- inla(y ~ 1 + x*xx,
          data = data.frame(y, x, xx),
          control.compute = list(smtp = "taucs"),
          verbose = TRUE,
          num.threads = "1:1", 
          keep = TRUE)

rr <- inla(y ~ 1 + x*xx,
          data = data.frame(y, x, xx),
          control.compute = list(smtp = "pardiso"),
          verbose = TRUE,
          num.threads = "1:1", 
          keep = TRUE)

