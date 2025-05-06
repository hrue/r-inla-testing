INLA:::inla.my.update(b = T)
inla.setOption(safe = FALSE, verbose = TRUE)

n <- 300
m <- 5
Z1 <- matrix(rnorm(m*n), n, m)

m <- m+1
Z2 <- matrix(rnorm(m*n), n, m)

m <- m+1
Z3 <- matrix(rnorm(m*n), n, m)

y <- rnorm(n)
r <- inla(y ~ -1 +
              f(idx1, model = "z", Z = Z1) +
              f(idx2, model = "z", Z = Z2) +
              f(idx3, model = "z", Z = Z3) +
              f(idx4, model = "fgn") +
              f(idx5, model = "fgn2"), 
          data = list(y = y, idx1 = 1:n, idx2 = 1:n, idx3 = 1:n,
                      idx4 = 1:n, idx5 = 1:n, 
                      Z1 = Z1, Z2 = Z2, Z3 = Z3))
