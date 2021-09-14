## Redo the Leuk-example using more senible PC-priors
data(Leuk)
## get the function to plot the map
source(system.file("demodata/Leuk-map.R", package="INLA"))
g = system.file("demodata/Leuk.graph", package="INLA")

## pc-prior parameters
U = 0.3/0.31
alpha = 0.01
pc.param = list(prec = list(prior = "pc.prec", param = c(U, alpha)))

formula = inla.surv(time, cens) ~ 1 +
    sex + age +
    f(inla.group(wbc), model="rw2",
      scale.model = TRUE, hyper = pc.param)+
    f(inla.group(tpi), model="rw2",
      scale.model = TRUE, hyper = pc.param)+
    f(district,model="besag",graph = g,
      scale.model = TRUE, hyper = pc.param)

INLA:::inla.my.update()
inla.setOption(inla.call = "inla.mkl.work", num.threads = "4:1")

r = inla(formula, 
         family = "coxph", 
         data=Leuk, 
         inla.mode = "experimental")

rr = inla(formula, 
         family = "coxph", 
         data=Leuk, 
         inla.mode = "classic")
print(r$mlik - rr$mlik)

         

