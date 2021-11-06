library(mvtnorm)
set.seed(123)
n <- 300
m <- 20
N <- m*n
A <- matrix(rnorm(n^2), n, n)
S <- A %*% t(A)
S <- S / exp(mean(log(diag(S))))
Q <- solve(S)
y <- c(rmvnorm(m, sigma = S)) + rnorm(m*n, sd = 0.1)

nc <- 20
A <- matrix(rnorm(nc*n), nc, n)
e <- rnorm(nc)
constr <- list(A = A, e = e)

r <- inla(y ~ -1 + f(idx, model = "generic",
                     replicate = r, Cmatrix = Q,
                     extraconstr = constr),
          inla.mode = "experimental", 
          data = data.frame(y, idx = rep(1:n, m), r = rep(1:m, each = n)),
          num.threads = "2:2", 
          inla.call = "inla.mkl.work", 
          control.pardiso = list(nrhs = -1), 
          verbose = TRUE)

          
