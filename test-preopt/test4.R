n <- 100
y <- rnorm(n) + 1

r <- inla(y ~ 1,
          data = data.frame(y),
          family = "t", 
          control.family = list(hyper = list(prec = list(initial = 0, param = c(100, 100)), 
                                             dof = list(initial = 20, fixed = TRUE))),
          verbose = TRUE,
          keep = TRUE, 
          inla.call = "inla.mkl.work",
          inla.arg = "-v -t1:1 -b -P")
