INLA:::inla.my.update(b = T)

mat <- matrix(c("a*2", "-3*b", ".2*a", "1.0"), 2, 2)
n <- 100
y <- rnorm(n)

r <- inla(y ~ -1 +
              f(idx, model = "iid") +
              f(jdx, model = "iid") +
              f(a, copy = "idx",  hyper = list(beta = list(initial = 2.2, fixed=!TRUE))) +
              f(b, copy = "jdx",  hyper = list(beta = list(initial = 3.3, fixed=!TRUE))), 
          data = data.frame(y, idx = 1:n, jdx = 1:n, a = 1:n, b = 1:n), 
          family = "sem",
          control.family = list(control.sem = list(B = mat, idx = 2)),
          safe = FALSE,
          verbose = TRUE,
          num.threads = "1")

a <- 2.2
b <- 3.3
B <- matrix(c(2*a, -3*b, 0.2*a, 1), 2, 2)
B <- diag(2)-B
solve(B %*% t(B))

