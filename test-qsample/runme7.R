n <- 40
R <- INLA:::inla.rw2(n, scale.model = TRUE)
Q <- R %x% R %x% R
diag(Q) <- diag(Q) + 1:n

xx <- inla.qinv(Q, num.threads = "1:6")
stop("xx")

xx <- inla.qsample(1, Q, num.threads = "1:6")

## Redo the Leuk-example using more senible PC-priors
data(Leuk)
## get the function to plot the map
source(system.file("demodata/Leuk-map.R", package="INLA"))
g = system.file("demodata/Leuk.graph", package="INLA")

## pc-prior parameters
U = 0.3/0.31
alpha = 0.01
pc.param = list(prec = list(prior = "pc.prec", param = c(U, alpha)))

Leuk <- rbind(Leuk, Leuk)
Leuk <- rbind(Leuk, Leuk)
Leuk <- rbind(Leuk, Leuk)
Leuk <- rbind(Leuk, Leuk)
Leuk <- rbind(Leuk, Leuk)
formula = inla.surv(Leuk$time, Leuk$cens) ~ sex + age +
    f(inla.group(wbc), model="rw2", diagonal = 1, 
      scale.model = TRUE, hyper = pc.param)+
    f(inla.group(tpi), model="rw2", diagonal = 1, 
      scale.model = TRUE, hyper = pc.param)+
    f(district,model="besag",graph = g, diagonal =  1, 
      scale.model = TRUE, hyper = pc.param)

result = inla(formula, family="coxph", data=Leuk,
              control.fixed = list(prec = 1, prec.intercept = 1), 
              control.hazard = list(scale.model = TRUE, hyper = pc.param),
              verbose = T,
              num.threads = "1:6")

summary(result)
Leuk.map(result$summary.random$district$mean)
plot(result)
