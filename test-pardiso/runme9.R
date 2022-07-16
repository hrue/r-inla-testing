library(mvtnorm)
set.seed(123)
n <- 5
A <- matrix(rnorm(n^2), n, n)
S <- A %*% t(A)
S <- S / exp(mean(log(diag(S))))
Q <- solve(S)
y <- c(rmvnorm(n, sigma = S)) + rnorm(n, sd = 0.1)

r <- inla(y ~ -1 + f(idx, model = "generic",
                     initial = 0, fixed = TRUE, 
                     Cmatrix = Q), 
          inla.mode = "experimental", 
          control.family = list(hyper =  list(
                                    prec = list(
                                        initial = log(1/0.1^2),
                                        fixed = TRUE))), 
          data = data.frame(y, idx = 1:n), 
          num.threads = "1:1", 
          inla.call = "inla.mkl.work", 
          control.pardiso = list(nrhs = -1), 
          verbose = TRUE,
          keep = TRUE, 
          safe = FALSE)

