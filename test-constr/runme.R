n <- 100
nc <- 25
A <- matrix(rnorm(nc * n), nc, n)
e <- rnorm(nc)
y <- rnorm(n)
r <- inla(y ~ -1 + f(idx, model = "rw2", scale.model = TRUE,
                     values = 1:n, constr = TRUE,
                     extraconstr = list(A = A, e = e)), 
          data = list(y = y, idx = 1:n),
          verbose = TRUE, keep = T, 
          num.threads = "2:4", 
          inla.call = "inla.mkl.work")
