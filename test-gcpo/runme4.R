data(Tokyo)
formula = y ~ -1 + f(time, model="rw2", constr = F, cyclic=TRUE, hyper =  list(prec =  list(prior = "pc.prec", param = c(0.5, 0.01))))

r = inla(formula, family="binomial", Ntrials=n, data=Tokyo,
         verbose = TRUE,
         keep = T, 
         inla.mode = "experimental",
         control.compute = list(cpo = T, 
                                control.gcpo = list(enable = TRUE,
                                                    group.size = 3)))
                                                    
                                                    
print(c(sum(log(r$cpo$cpo)), sum(log(r$gcpo$gcpo))))

res <- numeric(366)
for(i in 10:20) {

    print(i)
    data(Tokyo)
    Tokyo[c(i-1, i+1), "y"] <-  NA

    rr <- inla(formula, family="binomial", Ntrials=n, data=Tokyo,
               inla.mode = "experimental",
               control.mode = list(result = r, restart = TRUE), 
               control.compute = list(cpo = T))

    res[i] <- rr$cpo$cpo[i]
}

