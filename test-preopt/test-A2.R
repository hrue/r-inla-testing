n <- 5
x <- seq(2, by = 2, length = n)
xx <- seq(3, by = 2, length = n)

set.seed(123)
n <- 15
m <- 14
x <- rnorm(n)
xx <- rnorm(n)
xx[3:n] <- 0
y <- 1 + x + xx + rnorm(n, sd = 0.1)
A <- inla.as.sparse(matrix(rnorm(n*m) * rbinom(n*m, prob = 1, size = 1), m, n))
A[lower.tri(A)] <- 0

A

y <- A %*% cbind(1 + x + xx) + rnorm(m, sd = 0.1)

r <- inla(y ~ 1 + x + xx,
          data = list(y = y, x = x, xx = xx),
          family = "t", 
          verbose = TRUE, keep = TRUE, 
          control.compute = list(smtp = 'taucs'), 
          control.inla = list(cmin = 0), 
          control.predictor = list(A = A), 
          control.fixed = list(prec.intercept = 1, prec = 1), 
          control.family = list(hyper = list(prec = list(initial = log(10), fixed = FALSE),
                                             dof = list(initial = 6, fixed = TRUE))), 
          inla.call = "inla.valgrind", inla.arg = "-v -t1:1 -b -P")

round(dig = 4, cbind(1, x, xx))
round(dig = 4, A)
AB <- A %*% cbind(1, x, xx)
AtA <- t(AB) %*% AB
round(dig = 4, A %*% cbind(1, x, xx))
round(dig = 4, AtA)

### > round(dig = 4, AtA)
### 3 x 3 Matrix of class "dgeMatrix"
###                 x      xx
###    5.7002 -0.4810  2.2154
### x  -0.4810 10.6175 -5.5123
### xx  2.2154 -5.5123  8.3463

