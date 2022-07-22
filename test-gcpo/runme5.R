data(Tokyo)
n <- nrow(Tokyo)
formula = y ~ -1 + f(time, model="rw2", constr = F, cyclic=TRUE,
                     hyper =  list(prec =  list(prior = "pc.prec",
                                                param = c(0.5, 0.01))))

## to define something else that all data, use groups or selection.

## groups are list of list of nodes. NULL if none
groups <- rep(list(list()), n)
for(i in seq(1, n, by = 150)) {
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
                                                    num.level.sets = 2, 
                                                    verbose = TRUE, 
                                                    selection = selection
                                                    )))
rr = inla(formula, family="binomial", Ntrials=n, data=Tokyo,
         verbose = TRUE,
         inla.mode = "experimental",
         num.threads = "1:1",
         control.inla = list(int.strategy = "eb"), 
         control.compute = list(cpo = T, 
                                control.gcpo = list(enable = TRUE,
                                                    num.level.sets = 2, 
                                                    verbose = TRUE, 
                                                    groups = groups
                                                    )))

res <- cbind(seq_along(r$gcpo$gcpo),  r$gcpo$gcpo, rr$gcpo$gcpo)
res[!is.na(res[, 2]), ]

