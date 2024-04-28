data(Tokyo)
formula = y ~ -1 + f(time, model="rw2", cyclic=TRUE,
                     ## fix the precision here so you can see the effect better
                     initial = -5,
                     constr = FALSE,
                     scale.model = TRUE,
                     fixed = TRUE,
                     vb.correct = seq(1, dim(Tokyo)[1], by = 1))

r = inla(formula,
         family="binomial",
         Ntrials=n,
         data=Tokyo,
         control.inla = list(control.vb = list(enable = FALSE)))

rr = inla(formula,
          family="binomial",
          Ntrials=n,
          data=Tokyo,
          control.inla = list(control.vb = list(enable = TRUE)))

rrr = inla(formula,
           family="binomial",
           Ntrials=n,
           data=Tokyo,
           inla.mode = "classic", 
           control.inla = list(strategy = "laplace"))

plot(r$summary.random$time$mean, col = "black", pch = 19, cex = 0.2)
lines(rrr$summary.random$time$mean, lwd = 5, lty = 2, col = "red")
lines(rr$summary.random$time$mean, lwd = 3, lty = 3, col = "blue")
