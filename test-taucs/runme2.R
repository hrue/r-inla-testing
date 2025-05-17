INLA:::inla.my.update(b = T)
inla.setOption(num.threads = "1:12")
set.seed(834)
n <- 1000
A <- inla.as.sparse(matrix(-runif(n^2) *
                           rbinom(n^2, size = 1, prob = 5/n),
                           n, n))
A <- A %*% t(A)
diag(A) <- 0
diag(A) <- rowSums(A) + sqrt(n)

nc <- 300
constr <- list(
    A = matrix(rnorm(n*nc), nc, n),
    e = rnorm(nc))

y <- rnorm(n)
r <- inla(y ~ -1 + f(idx,
                      model = "generic",
                      Cmatrix = A,
                      extraconstr = constr),
          family = "normal",
          data = list(idx = 1:n, A = A, y = y),
          verbose = TRUE,
          control.compute = list(smtp = "taucs", cpo = T, waic = T, dic = T),
          control.taucs = list(block.size = 12), 
          safe = FALSE)
## 12 17
