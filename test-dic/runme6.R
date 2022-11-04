n = 250
if (!exists("y")) {
    x <- rnorm(n, sd = 0.3)
    eta = 2 + x
    y = eta + rnorm(n, sd = 0.1)
}

inla.setOption(inla.call = "inla.mkl.work")

r = inla(y ~ 1 + x, 
         control.fixed = list(mean.intercept = log(mean(y)), prec.intercept = 0.001), 
         data = data.frame(y, x), 
         control.predictor = list(hyper = list(prec = list(initial = 13))), 
         control.compute = list(dic = TRUE, po = TRUE), 
         control.inla = list(strategy = "gaussian"), 
         inla.mode = "classic",
         verbose = T)

rr = inla(y ~ 1 + x, 
         control.fixed = list(mean.intercept = log(mean(y)), prec.intercept = 0.001), 
         data = data.frame(y, x), 
         control.compute = list(dic = TRUE, po = TRUE), 
         control.inla = list(strategy = "gaussian"), 
         inla.mode = "experimental",
         verbose = T)

