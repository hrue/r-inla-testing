## Redo the Leuk-example using more senible PC-priors
data(Leuk)
## get the function to plot the map
source(system.file("demodata/Leuk-map.R", package="INLA"))
g = system.file("demodata/Leuk.graph", package="INLA")

inla.setOption(smtp = "pardiso" )
## pc-prior parameters
U = 0.3/0.31
alpha = 0.01
pc.param = list(prec = list(prior = "pc.prec", initial = 4, param = c(U, alpha)))
Leuk$age <- scale(Leuk$age)
formula = inla.surv(Leuk$time, Leuk$cens) ~ 1 + sex + age +
    f(inla.group(wbc), model="rw2", 
      scale.model = TRUE, hyper = pc.param) +
    f(inla.group(tpi), model="rw2", 
       scale.model = TRUE, hyper = pc.param) +
    f(district,model="besag",graph = g, 
      scale.model = TRUE, hyper = pc.param)

run <- function(mode) {
    inla(formula, family="coxph", data=Leuk,
         control.hazard = list(scale.model = TRUE, hyper = pc.param, 
                               constr = TRUE, diagonal = 1e-4),
         control.fixed = list(prec = 1, prec.intercept = 1), 
         inla.mode = mode, verbose = FALSE)
}

r <- run("classic")
rr <- run("twostage")
rrr <- run("experimental")

rbind(r$cpu, rr$cpu, rrr$cpu)

tail(r$logfile, 20)
tail(rr$logfile, 20)
tail(rrr$logfile, 20)
