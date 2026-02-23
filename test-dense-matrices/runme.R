n <- 10^3
Q <- matrix(rnorm(n^2), n, n)
Q <- Q %*% t(Q) / n
y <- rnorm(n)

r <- inla(y ~ -1 + f(idx, model = "generic", Cmatrix = Q, 
                     hyper = list(prec = list(initial = 0,
                                              fixed = TRUE))),
          data = list(Q = Q, y = y, n = n, idx = 1:n), 
          family = "stdnormal",
          verbose = TRUE,
          control.compute = list(smtp = "band", config = TRUE), 
          control.inla = list(reordering = "identity"),
          safe = FALSE,
          ##keep = TRUE,
          num.threads = "1:8:8")
xx <-  inla.posterior.sample(10^4, r)

              
