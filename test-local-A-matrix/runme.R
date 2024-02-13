n <- 300
x <- scale(arima.sim(n, model = list(ar = 0.90)))
xx <- scale(arima.sim(n, model = list(ar = 0.90)))
s <- 0.01
A <- inla.as.sparse(sparseMatrix(i = c(1,n), j = c(1,n), x = c(1, 1), dims = c(n, n)))
A@i <- as.integer(c(1, n, rep(2:(n-1), each = 3)))-1L
A@j <- as.integer(c(1, n, rep(c(-1, 0, 1), n-2) + rep(2:(n-1), each = 3)))-1L
A@x <- rep(1, length(A@i))

##A <- toeplitz(c(1, 1, rep(0, n-3), 1))
y <- as.vector(A %*% x + rnorm(n, sd = s))
yy <- as.vector(A %*% xx + rnorm(n, sd = s))
Y <- c(y, yy)
w <- rep(rep(0, n), 2)
idx <- rep(1:n, 2)
re <- rep(1:2, each = n)
A <- inla.as.sparse(A) 
AA <- bdiag(A, A)

r <- inla(Y ~ 1 + f(idx, w, model = "ar1", replicate = re, 
                    A.local = AA, 
                     hyper = list(prec = list(initial = 0, fixed = T),
                                  rho = list(initial = 3))), 
          data = list(Y = Y, AA = AA, w = w, re = re, idx = idx), 
          family = "gaussian", verbose = !TRUE, safe = FALSE, debug = FALSE, 
          control.family = list(hyper = list(prec = list(initial = log(1/s^2), fixed = TRUE))),
          control.inla = list(int.strategy = "eb"))
summary(r)
nn <- 2*n
par(mfrow = c(1, 2))
plot(r$mode$x[nn + 1:n], pch = 19)
lines(x, lwd = 2, col = "blue")
plot(r$mode$x[nn + n + 1:n], pch = 19)
lines(xx, lwd = 2, col = "blue")
