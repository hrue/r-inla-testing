
r <- inla(y ~ 1 + x + xx,
          data = data.frame(y = NA, x = 1, xx = 1), 
          control.fixed = list(prec.intercept = 0,
                               expand.factor.strategy = "inla"), 
          family = "poisson", 
          inla.mode = "experimental",
          verbose = TRUE,
          safe = TRUE)
