data(Tokyo)

INLA:::inla.my.update()
inla.setOption(inla.call = "inla.mkl.work")
inla.setOption(num.threads = "1:1")

formula = y ~ -1 + f(time, model="rw2", cyclic=TRUE, initial = -4,
                constr = FALSE, scale.model = T, fixed = T, param=c(1,0.0001), vb.correct = TRUE) 

r = inla(formula, family="binomial", Ntrials=n, data=Tokyo,
              control.inla = list(strategy = "gaussian"))

rr = inla(formula, family="binomial", Ntrials=n, data=Tokyo,
          control.inla = list(strategy = "gaussian", 
                              control.vb = list(enable = TRUE, refinement = 5, max.correct = 0.5, enable.limit = 360, verbose = TRUE)), 
          twostage = TRUE, verbose = T)

rrr = inla(formula, family="binomial", Ntrials=n, data=Tokyo,
           control.inla = list(strategy = "gaussian",
                               control.vb = list(enable = TRUE)), 
          twostage = FALSE)

r4 = inla(formula, family="binomial", Ntrials=n, data=Tokyo,
          control.inla = list(strategy = "laplace"))

plot(r$summary.random$time$mean, col = "blue", pch = 19)
lines(rr$summary.random$time$mean, lwd = 5, lty = 2, col = "red")
lines(rrr$summary.random$time$mean, lwd = 3, col = "blue")
lines(r4$summary.random$time$mean, lwd = 3, col = "yellow")

