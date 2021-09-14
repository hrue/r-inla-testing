n <- 25
y <- (1:n) + rnorm(n)
idx <- inla.group(1:n)

r <- inla(y ~ 1 + f(idx, model = "rw2", scale.model = TRUE),
          data = data.frame(y, idx),
          inla.call = "inla.mkl.work",
          inla.arg = "-v -t1 -b",
          verbose = TRUE)

rr <- inla(y ~ 1 + f(idx, model = "rw2", scale.model = TRUE),
          data = data.frame(y, idx),
          control.mode = list(result = r, restart = FALSE), 
          control.inla = list(verbose = TRUE), 
          inla.call = "inla.mkl.work",
          inla.arg = "-v -t1 -b -P",
          verbose = TRUE)

