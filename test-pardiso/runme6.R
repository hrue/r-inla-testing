## Redo the Leuk-example using more senible PC-priors
data(Leuk)
## get the function to plot the map
source(system.file("demodata/Leuk-map.R", package="INLA"))
g = system.file("demodata/Leuk.graph", package="INLA")

## pc-prior parameters
U = 0.3/0.31
alpha = 0.01
pc.param = list(prec = list(prior = "pc.prec", param = c(U, alpha)))

formula = inla.surv(Leuk$time, Leuk$cens) ~ sex + age +
    ##f(inla.group(wbc), model="rw2",
    ##  scale.model = TRUE, hyper = pc.param)+
    ##f(inla.group(tpi), model="rw2",
    ##  scale.model = TRUE, hyper = pc.param)+
    f(district,model="besag",graph = g,
      scale.model = TRUE, hyper = pc.param)

for(i in 1:100) {
    result = inla(formula, family="coxph", data=Leuk,
                  control.hazard = list(scale.model = TRUE,
                                        n.intervals = 10, 
                                        hyper = pc.param),
                  control.compute = list(config = TRUE), 
                  control.inla = list(tolerance = 1e-12, int.strategy = "eb"), 
                  control.predictor = list(compute = TRUE), 
                  num.threads = "2:4",
                  verbose = F,
                  keep = FALSE, 
                  inla.call = "inla.mkl.work")
    ##if (i == 1) stop("STOP")
    result <- inla.rerun(result)
    print(result$mlik)
}
