n <- 3
y <- c(1, 0, 2)
inla.setOption(inla.call = "inla.mkl.work")
rr <- inla(y ~ -1 +
               f(idx, model = "rw1", scale.model = FALSE, constr = FALSE, 
                 hyper = list(prec = list(initial = 0, param = c(100, 100)))), 
          data = data.frame(y, idx = 1:n, iidx = n:1),
          verbose = TRUE,
          family = "t", 
          control.predictor = list(hyper = list(prec = list(initial = 12))), 
          control.family = list(hyper = list(prec = list(initial = 0, fixed = TRUE),
                                             dof = list(initial = 20, fixed = TRUE))), 
          control.mode = list(theta = 0, x = rep(0, 2*n), restart = TRUE), 
          inla.call = "inla.mkl.work", inla.arg = "-v -t1:1 -b")

r <- inla(y ~ -1 +
              f(idx, model = "rw1", scale.model = FALSE, constr = FALSE, 
                 hyper = list(prec = list(initial = 0, param = c(100, 100)))), 
          data = data.frame(y, idx = 1:n, iidx = n:1),
          verbose = TRUE,
          family = "t", 
          control.family = list(hyper = list(prec = list(initial = 0, fixed = TRUE),
                                             dof = list(initial = 20, fixed = TRUE))), 
          keep = T, 
          inla.call = "inla.mkl.work", inla.arg = "-v -t1:1 -b -P")



