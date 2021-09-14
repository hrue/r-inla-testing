
set.seed(123)
n <- 25
x <- rnorm(n)
xx <- rnorm(n)
y <- 1 + x + xx + rnorm(n, sd = 0.1)

r <- inla(y ~ 1 + x + xx,
          data = list(y = y, x = x, xx = xx),
          family = "t", 
          verbose = TRUE, 
          control.inla = list(cmin = 0, int.strategy = "ccd"), 
          control.compute = list(config = TRUE), 
          control.fixed = list(prec.intercept = 1, prec = 1), 
          control.family = list(hyper = list(prec = list(initial = log(10), fixed = FALSE),
                                             dof = list(initial = 6, fixed = FALSE))), 
          inla.call = "inla.mkl.work", inla.arg = "-v -t1:1 -b")

rr <- inla(y ~ 1 + x + xx,
          data = list(y = y, x = x, xx = xx),
          family = "t", 
          verbose = TRUE, 
          control.inla = list(cmin = 0, int.strategy = "ccd"), 
          control.compute = list(config = TRUE), 
          control.fixed = list(prec.intercept = 1, prec = 1), 
          control.family = list(hyper = list(prec = list(initial = log(10), fixed = FALSE),
                                             dof = list(initial = 6, fixed = FALSE))), 
          inla.call = "inla.mkl.work", inla.arg = "-v -t1:1 -b -P")

nc <- r$misc$configs$nconfig
for(i in 1:nc) print (r$misc$configs$config[[i]]$log.posterior - r$misc$configs$config[[i]]$log.posterior.orig)
nc <- rr$misc$configs$nconfig
for(i in 1:nc) print (rr$misc$configs$config[[i]]$log.posterior - rr$misc$configs$config[[i]]$log.posterior.orig)
