n <- 1000
h <- 100
s <- 0.01
x <- scale(arima.sim(n, model = list(ar = 0.95)))
A <- sparseMatrix(x = 0, i = 1, j = 1, dims = c(n, n))
for(i in 1:n) {
    idx <- max(1, i-h):min(n, i+h)
    A[i, idx] <- 1/length(idx)
}


y <- A %*% x + rnorm(n, sd = s)
r <- inla(y ~  -1 + f(time, model = "ar1"),
          data = list(y = y, time = 1:n, A = A),
          control.family = list(hyper = list(prec = list(initial = log(1/s^2), fixed = TRUE))),
          control.predictor = list(A = A))
