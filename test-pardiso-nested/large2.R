m <- 15
n <- m^3
Q1 <- INLA:::inla.rw2(m, scale.model = TRUE)
A <- kronecker(Q1, kronecker(Q1, Q1))
idx.A <- sample(1:m^3, n, replace = TRUE)
Q2 <- INLA:::inla.rw1(m, scale.model = TRUE)
B <- kronecker(Q2, kronecker(Q2, Q2))
idx.B <- sample(1:m^3, n, replace = TRUE)
idx.C <- sample(1:m^3, n, replace = TRUE)
x <- rnorm(n)
y <- rnorm(n)

r <- inla(y ~ -1 + f(idx.A, model = "generic", Cmatrix = A, diagonal = 1) +
              f(idx.B, model = "generic", Cmatrix = B, diagonal = 1) + 
              f(idx.C, x, model = "generic", Cmatrix = B, diagonal = 1), 
          data = data.frame(y, idx.A, idx.B, idx.C, x),
          inla.call = "inla.mkl.work",
          num.threads = "2:4",
          verbose = TRUE)


