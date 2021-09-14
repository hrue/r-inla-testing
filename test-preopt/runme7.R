n <- 2500
y <- (1:n) + rnorm(n)
idx <- inla.group(1:n)

d.add <- 1e-4
r <- inla(y ~ 1 + f(idx, model = "rw2", scale.model = TRUE, diagonal = d.add),
          data = data.frame(y, idx),
          control.inla = list(diagonal = 100), 
          inla.call = "inla.mkl.work",
          inla.arg = "-v -t4 -b",
          verbose = TRUE)
rr <- inla(y ~ 1 + f(idx, model = "rw2", scale.model = TRUE, diagonal = d.add),
          data = data.frame(y, idx),
          control.inla = list(diagonal = 100), 
          inla.call = "inla.mkl.work",
          inla.arg = "-v -t4 -b -P",
          verbose = TRUE)

