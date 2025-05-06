INLA:::inla.my.update(b = T)
set.seed(123)
n <- 7
nc <- 5

A <- matrix(rnorm(n*nc), nc, n)
e <- rep(0, nc)

C <- matrix(rbinom(n^2, size = 1, prob = 1/n),n, n)
C <- inla.as.sparse(C %*% t(C))
diag(C) <- n+1
inla.setOption(num.threads = "1:1", safe = FALSE)
rr <- inla(y ~ -1 +
               f(idx,
                 model = "generic0",
                 Cmatrix = C, 
                 initial = 0, fixed = TRUE, 
                 extraconstr = list(A = A, e = e),
                 values = 1:n), 
          data = data.frame(y = seq(-5, 5, len = n), idx = 1:n),
          family = "stdnormal",
          control.compute = list(smtp = "band"))
r <- inla(y ~ -1 + 
               f(idx,
                 model = "generic0",
                 Cmatrix = C, 
                 initial = 0, fixed = TRUE, 
                 extraconstr = list(A = A, e = e),
                 values = 1:n), 
          data = data.frame(y = seq(-5, 5, len = n), idx = 1:n),
          family = "stdnormal",
          verbose = TRUE)

round(dig = 5, cbind(rr$summary.random$idx$mean, r$summary.random$idx$mean))
print(mean(abs(cbind(rr$summary.random$idx$mean - r$summary.random$idx$mean))))

