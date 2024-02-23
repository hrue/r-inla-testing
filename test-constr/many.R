n <- 1500
nc <- 501
while(TRUE) {
    A <- rnorm(nc * n) * rbinom(n * nc, prob = 0.1, size = 1)
    A <- matrix(A, nc, n)
    if (all(rowSums(A) != 0)) break;
}

e <- rnorm(nc)
y <- rnorm(n)

m = get("inla.models", inla.get.inlaEnv())
m$latent$rw2$min.diff = NULL
assign("inla.models", m, inla.get.inlaEnv())

r <- inla(y ~ -1 + f(idx, model = "rw2",
                     scale.model = TRUE,
                     values = 1:n,
                     constr = FALSE,
                     extraconstr = list(A = A, e = e)), 
          data = list(y = y, idx = 1:n, A = A, e = e),
          verbose = TRUE, 
          safe = F, keep = !T, 
          num.threads = "4:1", 
          control.compute = list(config = TRUE))

rr <- inla(y ~ -1 + f(idx, model = "rw2",
                     scale.model = TRUE,
                     values = 1:n,
                     constr = FALSE,
                     extraconstr = list(A = A, e = e)), 
          data = list(y = y, idx = 1:n, A = A, e = e),
          verbose = TRUE, 
          safe = F, keep = !T, 
          num.threads = "4:1", 
          inla.call = "inla.mkl.work",
          control.compute = list(config = TRUE))

r$mlik - rr$mlik

