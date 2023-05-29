n <- 80
y <- rnorm(n)
x <- rnorm(n)
inla.setOption(num.threads = "1:1")
inla.setOption(inla.mode = "compact")
inla.setOption(inla.mode = "classic")
inla.setOption(smtp = "taucs")

r <- inla(y ~ 1 + x, 
          data = data.frame(y, idx = 1:n, x),
          control.fixed = list(prec.intercept = 1, prec = 1), 
          family = "t", 
          control.family = list(hyper = list(prec = list(initial = 0, fixed = TRUE),
                                             dof = list(initial = 10, fixed = TRUE))), 
          control.inla = list(cmin = 0),
          control.compute = list(internal.opt = FALSE), 
          control.predictor = list(hyper = list(prec = list(initial = 12, fixed = TRUE))), 
          control.expert = list(disable.gaussian.check = TRUE))
rr <- inla(y ~ 1 + x, 
           data = data.frame(y, idx = 1:n, x),
           control.fixed = list(prec.intercept = 1, prec = 1), 
           family = "t", 
           control.family = list(hyper = list(prec = list(initial = 0, fixed = TRUE), 
                                              dof = list(initial = 10, fixed = TRUE))), 
           inla.call = "inla.mkl.work", 
           control.inla = list(cmin = 0), 
           control.compute = list(internal.opt = FALSE),
           control.predictor = list(hyper = list(prec = list(initial = 12, fixed = TRUE))), 
           control.expert = list(disable.gaussian.check = TRUE))
    
print(r$summary.fixed[, 1:5] - rr$summary.fixed[, 1:5])


