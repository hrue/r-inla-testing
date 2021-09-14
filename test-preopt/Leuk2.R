## Redo the Leuk-example using more senible PC-priors
data(Leuk)
## get the function to plot the map
source(system.file("demodata/Leuk-map.R", package="INLA"))
g = system.file("demodata/Leuk.graph", package="INLA")

nres <- 20
## pc-prior parameters
U = 0.3/0.31
alpha = 0.01
pc.param = list(prec = list(prior = "pc.prec", initial = 4, param = c(U, alpha)))
Leuk$age <- scale(Leuk$age)
formula = inla.surv(Leuk$time, Leuk$cens) ~ 1 + sex + age +
    f(inla.group(wbc, n = nres), model="rw2", diagonal = 1e-4, 
      scale.model = TRUE, hyper = pc.param) +
    f(inla.group(tpi, n = nres), model="rw2", diagonal =  1e-4, 
       scale.model = TRUE, hyper = pc.param) +
    f(district,model="besag",graph = g, diagonal = 1e-4, 
      scale.model = TRUE, hyper = pc.param)

inla.setOption(inla.call = "inla.mkl.work", num.threads = "1:1")

run <- function() {
    r <- inla(formula, family="coxph", data=Leuk,
              control.hazard = list(scale.model = FALSE, hyper = pc.param, n.intervals = nres,
                                    constr = TRUE, diagonal = 1e-4),
              control.fixed = list(prec = 1, prec.intercept = .01), 
              control.predictor = list(hyper = list(prec = list(initial = 13))), 
              verbose = TRUE,
              keep = FALSE,
              twostage = twostage,
              control.inla = list(control.twostage = list(stage1only = TRUE),
                                  strategy = "gaussian", 
                                  h = if (twostage) 0.001 else 0.005, 
                                  control.vb = list(refinement = 2, f.enable.limit = nres+1, max.correct = 0.5, verbose = TRUE, enable = TRUE), 
                                  int.strategy = "eb"))
    ##inla.rerun(r)
    return(r)
}

run.l <- function() {
    r <- inla(formula, family="coxph", data=Leuk,
              control.hazard = list(scale.model = FALSE, hyper = pc.param, n.intervals = nres,
                                    constr = TRUE, diagonal = 1e-4),
              control.fixed = list(prec = 1, prec.intercept = .01), 
              control.predictor = list(hyper = list(prec = list(initial = 13))), 
              verbose = TRUE,
              keep = FALSE,
              twostage = FALSE,
              control.inla = list(control.twostage = list(stage1only = FALSE),
                                  strategy = "laplace", 
                                  control.vb = list(refinement = 2, f.enable.limit = nres+1, max.correct = 0.5, verbose = TRUE, enable = FALSE), 
                                  int.strategy = "eb"))
    ##inla.rerun(r)
    return(r)
}



