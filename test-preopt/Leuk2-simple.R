## Redo the Leuk-example using more senible PC-priors
data(Leuk)
## get the function to plot the map
source(system.file("demodata/Leuk-map.R", package="INLA"))
g = system.file("demodata/Leuk.graph", package="INLA")

## pc-prior parameters
U = 0.3/0.31
alpha = 0.01
pc.param = list(prec = list(prior = "pc.prec", param = c(U, alpha)))
Leuk <- Leuk[1:10, ]

Leuk$age <- scale(Leuk$age)
formula = inla.surv(Leuk$time, Leuk$cens) ~ 1 ##+ sex + age 
##    f(inla.group(wbc), model="rw2",
##      scale.model = TRUE, hyper = pc.param)
##    f(inla.group(tpi), model="rw2",
##      scale.model = TRUE, hyper = pc.param)
#    f(district,model="besag",graph = g,
#      scale.model = TRUE, hyper = pc.param)

inla.setOption(inla.call = "inla.mkl.work")
inla.setOption(num.threads = "1:1")
set.seed(123)
Leuk$time <- rnorm(nrow(Leuk))
Leuk$idx <- 1
formula = Leuk$time ~ 1 + sex + age + f(idx, model = "iid", values = 1:4, vb.correct = 1, hyper = list(prec = list(initial = 0, fixed = TRUE)))
result = inla(formula, family="t", data=Leuk,
              control.family = list(hyper = list(dof = list(initial = 1, fixed = TRUE))), 
              control.hazard = list(scale.model = FALSE, hyper = pc.param, n.intervals = 2, diagonal = 0),
              control.fixed = list(prec = 1, prec.intercept = 11), 
              control.predictor = list(hyper = list(prec = list(initial = 15))), 
              verbose = TRUE,
              keep = FALSE,
              twostage = twostage,
              control.mode = list(theta = c(0), fixed = T), 
              control.inla = list(control.twostage = list(stage1only = TRUE),
                                  cmin = 0, 
                                  strategy = "gaussian", 
                                  control.vb = list(f.enable.limit = 1, verbose = TRUE, enable = TRUE), 
                                  int.strategy = "eb"))
summary(result)

