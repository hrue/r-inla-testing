INLA:::inla.my.update(b = T)

r <- inla(y ~ 1,
          data = data.frame(y = 0),
          verbose = TRUE,
          control.stiles = list(verbose = 1, debug = 1))

