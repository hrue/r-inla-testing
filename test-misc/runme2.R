data(Tokyo)

formula = y ~ f(time, model="rw2", constr = TRUE, cyclic=TRUE, param=c(1,0.0001)) +1

rr = inla(formula, family="binomial", Ntrials=n, data=Tokyo,
         inla.call = INLA:::inla.call.builtin(), 
         num.threads = "1:1",
         control.compute = list(control.gcpo = list(enable = TRUE, group.size = 25)), 
         inla.mode = "experimental")

r = inla(formula, family="binomial", Ntrials=n, data=Tokyo,
         inla.call = "inla.mkl.work",
         num.threads = "1:1",
         control.compute = list(control.gcpo = list(enable = TRUE, group.size = 25)), 
         inla.mode = "experimental",
         keep = T, verbose = T)

print(mean(abs(r$summary.random$time$mean - rr$summary.random$time$mean)))
print(mean(abs(r$summary.random$time$sd/rr$summary.random$time$sd -1)))
