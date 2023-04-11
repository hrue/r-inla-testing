INLA:::inla.my.update(b = T)
n <- 5
y <- rnorm(n)
idx <- 1:n
idx2 <- 1:n

## the global constraint
A = rbind(1:n, as.vector(scale(1:(2*n))))
e = c(0.11, 0.22)

r <- inla(y ~ -1 + f(idx, constr = TRUE) + f(idx2, constr = FALSE),
          family = "gaussian",
          control.family = list(hyper = list(prec = list(initial = 4, fixed = TRUE))), 
          data = list(y = y, idx = idx, idx2 = idx2, A = A, e = e),
          control.compute = list(config = TRUE), 
          verbose = TRUE, 
          ##
          control.expert = list(globalconstr = list(A = A, e = e)))

## check...
xx <- r$mode$x[-(1:n)]
A %*% xx - e

## the global constraint is appended to the other ones
r$misc$config$constr$A %*% xx - r$misc$configs$constr$e

