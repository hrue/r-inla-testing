inla.setOption(num.threads = 1)
n <- 1000
A <- inla.as.sparse(matrix(-runif(n^2) *
                           rbinom(n^2, size = 1, prob = 5/n),
                           n, n))
A <- A %*% t(A)
diag(A) <- 0
diag(A) <- rowSums(A) + sqrt(n)

y <- rnorm(n)

rr <- inla(y ~ -1 + f(idx, model = "generic", Cmatrix = A),
          family = "normal",
          data = list(idx = 1:n, A = A, y = y),
          verbose = TRUE,
          control.compute = list(smtp = "taucs"),
          inla.call = "inla.mkl", 
          safe = FALSE)

r <- inla(y ~ -1 + f(idx, model = "generic", Cmatrix = A),
          family = "normal",
          data = list(idx = 1:n, A = A, y = y),
          verbose = TRUE,
          control.compute = list(smtp = "taucs"),
          inla.call = "inla.mkl.work", 
          safe = FALSE)


          
