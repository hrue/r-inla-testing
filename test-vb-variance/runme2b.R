INLA:::inla.my.update()

rr <- inla(y ~ 1 + x + xx + xxx, 
          data = data.frame(x, xx, xxx, y), 
          family = "poisson",
          control.fixed = list(prec.intercept = 0.1, prec = 0.1), 
          inla.mode = "experimental", 
          num.threads = "1:1",
          control.inla = list(control.vb = list(strategy = "variance", hessian.strategy = "full")), 
          safe = F, 
          inla.call = "inla.mkl.work", 
          verbose = TRUE)
