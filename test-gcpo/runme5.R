INLA:::inla.my.update(b = T)
data(Tokyo)
n <- nrow(Tokyo)
formula = y ~ -1 + f(time, model="rw2", constr = F, cyclic=TRUE,
                     hyper =  list(prec =  list(prior = "pc.prec",
                                                param = c(0.5, 0.01))))

## to define something else that all data, use groups or selection.

## groups are list of list of nodes. NULL if none
groups <- rep(list(list()), n)
for(i in seq(1, n, by = 10)) {
    groups[[i]] <- (((i-1):(i+1) - 1 + n) %% n) + 1
}

## selection is a list of nodes (data-points) to use
selection <- seq(1, n, by = 150)

r = inla(formula, family="binomial", Ntrials=n, data=Tokyo,
         verbose = TRUE,
         inla.mode = "experimental",
         num.threads = "1:1",
         control.inla = list(int.strategy = "eb"), 
         control.compute = list(cpo = T, 
                                control.gcpo = list(enable = TRUE,
                                                    group.size = 1, 
                                                    verbose = !TRUE, 
                                                    ## either of these give the same result
                                                    ##groups = groups
                                                    selection = selection
                                                    )))

plot(r$cpo$cpo, r$gcpo$gcpo, pch = 1)
abline(a = 0, b = 1, lwd = 2)
