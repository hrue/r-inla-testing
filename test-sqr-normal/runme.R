inla.setOption("inla.call",  "inla.work")
inla.my.update()

n = 100
z = runif(n)
eta = 1 + z
s=0.1
y = 0.5 * eta^2 + rnorm(n,  sd = s)

formula = y ~ 1 + z
r = inla(formula, data = data.frame(y, z),  verbose=TRUE,
        family = "sqrnormal",
        control.fixed = list(prec.intercept = 0.0001), 
        control.data = list(hyper = list(scale = list(initial = 1,  fixed=FALSE),
                                    prec = list(initial = log(1/s^2), fixed=TRUE))),
        control.mode = list(theta = c(1), x = c(eta,  1,  10),  restart=TRUE))

