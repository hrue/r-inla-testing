INLA:::inla.my.update(b = T)

Y <- matrix(NA, 4, 2)
Y[1, 1] <-  1
Y[2, 2] <- 50
r <- inla(Y ~ 1,
          data = list(Y = Y), 
          family = c("poisson", "binomial"),
          Ntrials = c(NA, 100, NA, 100), 
          control.predictor = list(link = c(1, 2, 1, 2)), 
          keep = TRUE, 
          verbose = TRUE,
          num.threads = 1,
          inla.mode = "experimental")

cbind(lp = as.numeric(r$summary.linear.predictor[, "mean"]), 
      fitted = as.numeric(r$summary.fitted.values[, "mean"]))
