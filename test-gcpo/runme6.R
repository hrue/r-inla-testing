INLA:::inla.my.update(b = T)
data(Tokyo)
n <- nrow(Tokyo)
formula = y ~ -1 + f(time, model="rw2", constr = F, cyclic=TRUE,
                     hyper =  list(prec =  list(prior = "pc.prec",
                                                param = c(0.5, 0.01))))

r = inla(formula, family="binomial", Ntrials=n, data=Tokyo,
         verbose = TRUE,
         inla.mode = "experimental",
         num.threads = "1:1",
         control.inla = list(int.strategy = "eb"), 
         control.compute = list(cpo = T, 
                                control.gcpo = list(enable = TRUE,
                                                    group.size = 1)))

plot(r$cpo$cpo, r$gcpo$gcpo, pch = 1)
abline(a = 0, b = 1, lwd = 2)
