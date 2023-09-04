n <- 3000
m <- 100
u <- scale(rnorm(m))
v <- scale(rnorm(m))

idx.u <- sample(1:m, n, replace = TRUE)
idx.v <- sample(1:m, n, replace = TRUE)
w <- rep(10, n)

s <- 0.001
y <- w*u[idx.u] + v[idx.v] + rnorm(n, sd = s)

r <- inla(y ~ -1 +
              f(idx.u, w, model = "iid", values = 1:m) +
              f(idx.v, model = "iid", values = 1:m),
          data = list(y = y, idx.u = idx.u, idx.v = idx.v, n = n, w = w),
          control.family = list(hyper = list(prec = list(initial = log(1/s^2), fixed = TRUE))),
          verbose = FALSE, safe = FALSE)

rr <- inla(y ~ -1 +
              f(idx.u, w, model = "iid", values = 1:m) +
              f(idx.v, model = "iid", values = 1:m),
          data = list(y = y, idx.u = idx.u, idx.v = idx.v, n = n, w = w),
          control.family = list(hyper = list(prec = list(initial = log(1/s^2), fixed = TRUE))),
          verbose = TRUE, safe = FALSE,
          inla.call = "inla.mkl.work")
rrr <- inla(y ~ -1 +
                f(idx.u, w, model = "iid", values = 1:(2*m)) + f(idx.v, copy = "idx.u"), 
          data = list(y = y, idx.u = idx.u, idx.v = m + idx.v, n = n, w = w),
          control.family = list(hyper = list(prec = list(initial = log(1/s^2), fixed = TRUE))),
          verbose = FALSE, safe = FALSE)
r$mode$theta
rr$mode$theta
rrr$mode$theta



                              
              


