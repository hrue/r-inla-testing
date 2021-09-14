set.seed(123)
n <- 16
x <- rnorm(n)
xx <- rnorm(n)
xxx <- rnorm(n)
xx[(n %/% 2):n] <- 0
x[n:(n %/% 2)] <- 0
s <- 0.1
y <- 1 + x + xx + xxx + rnorm(n, sd = s)

r <- inla(y ~ 1 + x + xx + xxx,
          data = list(y = y, x = x, xx = xx, xxx = xxx, A = A),
          family = "t", 
          verbose = TRUE, keep = T, 
          control.compute = list(smtp = 'pardiso'), 
          control.inla = list(cmin = 0), 
          control.fixed = list(prec.intercept = 1, prec = 1), 
          control.family = list(hyper = list(prec = list(initial = 4, fixed = FALSE),
                                             dof = list(initial = 6, fixed = TRUE))), 
          inla.call = "inla.mkl.work", inla.arg = "-v -t4:1 -b -P")
rr <- inla(y ~ 1 + x + xx + xxx,
           data = list(y = y, x = x, xx = xx, xxx = xxx),
           family = "t", 
           verbose = TRUE,
           control.compute = list(smtp = 'pardiso'), 
           control.inla = list(cmin = 0), 
           control.fixed = list(prec.intercept = 1, prec = 1), 
           control.family = list(hyper = list(prec = list(initial = 4, fixed = FALSE),
                                              dof = list(initial = 6, fixed = TRUE))), 
           inla.call = "inla.mkl.work", inla.arg = "-v -t1:1 -b")

r$summary.fixed - rr$summary.fixed
r$mlik - rr$mlik
plot(r$mode$x,  rr$mode$x)
abline(a = 0, b = 1)
r$mode$theta - rr$mode$theta
