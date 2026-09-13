data(Leuk)
g = system.file("demodata/Leuk.graph", package="INLA")

inla.setOption(scale.model = TRUE)
Leuk$time <- Leuk$time / max(Leuk$time)
formula = inla.surv(time, cens) ~ sex + age +
    f(inla.group(wbc), model="rw2", hyper = list(prec = list(prior = "pc.prec",
                                                                     param = c(0.25, 0.01)))) +
    f(inla.group(tpi), model="rw2",  hyper = list(prec = list(prior = "pc.prec",
                                                                     param = c(0.25, 0.01)))) +
    f(district,model="besag",graph = g,  hyper = list(prec = list(prior = "pc.prec",
                                                                     param = c(0.25, 0.01))))

r = inla(formula,
         family="gammasurv",
         data=Leuk,
         inla.call = "inla.mkl", 
         num.threads = "1",
         control.compute = list(control.gcpo = list(enable = TRUE,
                                                    num.level.sets = 3)), 
         verbose = TRUE)

r = inla(formula,
         family="gammasurv",
         data=Leuk,
         inla.call = "inla.mkl", 
         num.threads = "1",
         control.compute = list(control.gcpo = list(enable = TRUE,
                                                    num.level.sets = 3)), 
         control.mode = list(theta = r$mode$theta, restart = FALSE, fixed = TRUE), 
         verbose = TRUE)
r <- inla.rerun(r)
r <- inla.rerun(r)
r <- inla.rerun(r)
r <- inla.rerun(r)
r <- inla.rerun(r)


rr = inla(formula,
          family="gammasurv",
          data=Leuk,
          num.threads = "1",
          inla.call = "inla.mkl.work", 
          control.compute = list(control.gcpo = list(enable = TRUE,
                                                     num.level.sets = 3)), 
          control.mode = list(theta= r$mode$theta, restart = FALSE, fixed = TRUE), 
          verbose = TRUE)
rr <- inla.rerun(rr)
rr <- inla.rerun(rr)
rr <- inla.rerun(rr)
rr <- inla.rerun(rr)
rr <- inla.rerun(rr)

r$mlik - rr$mlik

for(i in 1:length(r$gcpo$groups)) {
    cat(i, mean(abs(r$gcpo$groups[[i]]$corr - rr$gcpo$groups[[i]]$corr)), "\n")
    if (!(all(r$gcpo$groups[[i]]$idx ==
                  rr$gcpo$groups[[i]]$idx))) {
        print(i)
        print(r$gcpo$groups[[i]])
        print(rr$gcpo$groups[[i]])
    }
                  
}
