data(Tokyo)
formula = y ~ -1 + f(time, model="rw2", cyclic=TRUE,
                     ## fix the precision here so you can see the effect better
                     initial = -4,
                     constr = FALSE,
                     scale.model = TRUE,
                     fixed = TRUE,
                     vb.correct = TRUE) 

r = inla(formula,
         family="binomial",
         Ntrials=n,
         data=Tokyo,
         control.inla = list(strategy = "gaussian"))

rr = inla(formula,
          family="binomial",
          Ntrials=n,
          data=Tokyo,
          control.inla = list(strategy = "gaussian", 
                              control.vb = list(enable = TRUE,
                                                refinement = 5,
                                                max.correct = 0.5,
                                                verbose = TRUE)), 
          verbose = TRUE)

rrr = inla(formula,
           family="binomial",
           Ntrials=n, data=Tokyo,
           control.inla = list(strategy = "laplace"))

plot(r$summary.random$time$mean, col = "black", pch = 19)
lines(rrr$summary.random$time$mean, lwd = 7, lty = 2, col = "red")
lines(rr$summary.random$time$mean, lwd = 3, col = "blue")

