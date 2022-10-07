n <- 1000
nc <- 800
while(TRUE) {
    A <- rnorm(nc * n) * rbinom(n * nc, prob = 0.1, size = 1)
    A <- matrix(A, nc, n)
    if (all(rowSums(A) != 0)) break;
}

e <- rnorm(nc)
y <- rnorm(n)

r <- inla(y ~ -1 + f(idx, model = "rw2",
                     scale.model = TRUE,
                     values = 1:n,
                     constr = FALSE,
                     extraconstr = list(A = A, e = e)), 
          data = list(y = y, idx = 1:n, A = A, e = e),
          verbose = TRUE, 
          safe = F, keep = !T, 
          inla.mode = "experimental", 
          num.threads = "4:1", 
          inla.call = "inla.mkl")

rr <- inla(y ~ -1 + f(idx, model = "rw2",
                     scale.model = TRUE,
                     values = 1:n,
                     constr = FALSE,
                     extraconstr = list(A = A, e = e)), 
          data = list(y = y, idx = 1:n, A = A, e = e),
          verbose = TRUE, 
          safe = F, keep = !T, 
          inla.mode = "experimental", 
          num.threads = "4:1", 
          inla.call = "inla.mkl.work")

r$mlik - rr$mlik

