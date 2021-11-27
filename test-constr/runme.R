n <- 25

y <- rnorm(n)
r <- inla(y ~ -1 + f(idx, model = "rw2", scale.model = TRUE,
                     values = 1:n, constr = TRUE,
                     extraconstr = list(A = matrix(1, 1, n), e = 0)),
          data = list(y = y, idx = 1:n),
          verbose = TRUE,
          inla.call = "inla.mkl.work")
