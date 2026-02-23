INLA:::inla.my.update(b = T)
n <- 200
r <- inla(y ~ 1 + f(idx, model = "rw2", diagonal = 0.000001,
                    constr = TRUE, 
                    scale.model = FALSE),
          data = data.frame(idx = 1:n, y = rep(NA, n)),
          verbose = TRUE,
          control.fixed = list(prec.intercept = 0.0000), 
          control.compute = list(smtp = 'stiles'), 
          safe = FALSE)

