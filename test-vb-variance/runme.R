n <- 6
m <- 7

A <- matrix(rnorm(m*n), m, n)
s <- 0.1
y <- rpois(m, 1)

Q <- matrix(rnorm(n^2), n, n)
Q <- Q %*% t(Q)
S <- solve(Q)
S <- S/exp(mean(log(diag(S))))
Q <- solve(S)

idx <- 1:n
r <- inla(y ~ -1 + f(idx, model = "generic", Cmatrix = Q,
                     hyper = list(prec = list(initial = 0, fixed = TRUE))),
          data = list(y = y, idx = idx, Q = Q),
          family = "poisson",
          ##control.family = list(hyper = list(prec = list(initial = log(1/s^2), fixed = TRUE), dof = list(initial = 20, fixed = T))), 
          control.predictor = list(A = A),
          inla.mode = "experimental", 
          num.threads = 1,
          safe = F, 
          inla.call = "inla.mkl.work", 
          verbose = TRUE)

QQ <- 1/s^2 * t(A) %*% A + Q
SS <- solve(QQ)

A %*% SS

