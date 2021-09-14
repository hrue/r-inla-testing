
r <- inla(y ~ 1,
          data = data.frame(y = 0),
          ##control.inla = list(constr.marginal.diagonal = 1235),
          verbose = TRUE,
          inla.call = "inla.work", 
          keep = TRUE)

