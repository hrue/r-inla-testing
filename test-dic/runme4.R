n = 5000
eta = 5
if (!exists("y"))
    y = rpois(n, exp(eta))

r = inla(y ~ 1, 
         family = "poisson", 
         control.fixed = list(mean.intercept = log(mean(y)), prec.intercept = 0.001), 
         data = data.frame(y), 
         control.predictor = list(hyper = list(prec = list(initial = 15))), 
         control.compute = list(dic = TRUE), 
         control.inla = list(strategy = "gaussian"), 
         inla.call = "inla.mkl.work", 
         inla.mode = "classic",
         verbose = T)

rr = inla(y ~ 1, 
         family = "poisson", 
         control.fixed = list(mean.intercept = log(mean(y)), prec.intercept = 0.001), 
         data = data.frame(y), 
         control.compute = list(dic = TRUE), 
         control.inla = list(strategy = "gaussian"), 
         inla.call = "inla.mkl.work", 
         inla.mode = "experimental",
         verbose = T)

head(cbind(classic = r$dic$local.dic,
           experimental = rr$dic$local.dic,
           plugin = -2*dpois(y, lambda = mean(y), log = TRUE)))

head(cbind(sat.classic = r$dic$local.dic.sat,
           sat.experimental = rr$dic$local.dic.sat,
           sat.plugin = -2*(dpois(y, lambda = mean(y), log = TRUE) - dpois(y, lambda = y, log = TRUE))))


r$summary.fixed
rr$summary.fixed
