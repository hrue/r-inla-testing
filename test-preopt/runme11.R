n <- 10
A <- matrix(rnorm(n^2), n, n)
Q <- A %*% t(A)

y <- rep(0, n)

r <- inla(y ~ -1 + f(idx, model = "generic0",
                     Cmatrix = Q,
                     hyper =  list(prec = list(initial = 0,
                                               fixed = TRUE))),
          control.family = list(hyper = list(prec = list(
                                                 initial = 0,
                                                 fixed = TRUE))),
          data = list(y = y, n = n, idx = 1:n),
          verbose = TRUE,
          control.compute = list(config = TRUE),
          inla.mode = "experimental", 
          inla.call = "inla.mkl.work")

QQ <- Q
QQ[lower.tri(QQ)] <- 0
r$misc$configs$config[[1]]$Q  - QQ


