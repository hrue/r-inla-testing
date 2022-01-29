n <- 2
yy <- 1

r <- inla(y ~ 1, data = data.frame(y = yy), 
          family = "poisson",
          inla.mode = "classic", 
          control.fixed = list(prec.intercept = 1), 
          control.predictor = list(compute = TRUE), 
          control.inla = list(strategy = "gaussian",
                              control.vb = list(enable = TRUE)))

rg <- inla(y ~ 1, data = data.frame(y = yy), 
          family = "poisson",
          inla.mode = "classic", 
          control.fixed = list(prec.intercept = 1), 
          control.predictor = list(compute = TRUE), 
          control.inla = list(strategy = "gaussian",
                              control.vb = list(enable = FALSE)))

rr <- inla(y ~ 1, data = data.frame(y = yy),
          family = "poisson",
          inla.mode = "experimental", 
          control.fixed = list(prec.intercept = 1), 
          control.predictor = list(compute = TRUE), 
          control.inla = list(strategy = "gaussian",
                              control.vb = list(enable = TRUE)))

rrg <- inla(y ~ 1, data = data.frame(y = yy),
          family = "poisson",
          inla.mode = "experimental", 
          control.fixed = list(prec.intercept = 1), 
          control.predictor = list(compute = TRUE), 
          control.inla = list(strategy = "gaussian",
                              control.vb = list(enable = FALSE)))

cbind(r$summary.fixed[, "mean"], rr$summary.fixed[, "mean"])
cbind(r$summary.linear.predictor$mean, rr$summary.linear.predictor$mean)
