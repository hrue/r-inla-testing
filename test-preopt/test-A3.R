set.seed(123)
n <- 50
m <- 250
x <- rnorm(n)
xx <- rnorm(n)
xx[25:n] <- 0
y <- 1 + x + xx + rnorm(n, sd = 0.1)
A <- inla.as.sparse(matrix(rnorm(n*m) * rbinom(n*m, prob = 0.5, size = 1), m, n))

y <- A %*% cbind(1 + x + xx) + rnorm(m, sd = 0.1)

r <- inla(y ~ 1 + x + xx,
          data = list(y = y, x = x, xx = xx, A = A),
          family = "t", 
          verbose = TRUE, keep = TRUE, 
          control.compute = list(smtp = 'taucs'), 
          control.inla = list(cmin = 0), 
          control.predictor = list(A = A), 
          control.fixed = list(prec.intercept = 1, prec = 1), 
          control.family = list(hyper = list(prec = list(initial = log(10), fixed = FALSE),
                                             dof = list(initial = 6, fixed = TRUE))), 
          inla.call = "inla.mkl.work", inla.arg = "-v -t1:1 -b -P")

rr <- inla(y ~ 1 + x + xx,
           data = list(y = y, x = x, xx = xx),
           family = "t", 
           verbose = TRUE, keep = TRUE, 
           control.compute = list(smtp = 'taucs'), 
           control.inla = list(cmin = 0), 
           control.predictor = list(A = A), 
           control.fixed = list(prec.intercept = 1, prec = 1), 
           control.family = list(hyper = list(prec = list(initial = log(10), fixed = FALSE),
                                              dof = list(initial = 6, fixed = TRUE))), 
           inla.call = "inla.mkl.work", inla.arg = "-v -t1:1 -b")

r$summary.fixed - rr$summary.fixed
r$mlik - rr$mlik
plot(r$mode$x,  rr$mode$x)
abline(a = 0, b = 1)
